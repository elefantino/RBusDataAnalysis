FindTransferTimes = function(path, line1, dir1, t1, stop1, line2, dir2, t2, stop2, transfer = 60) {
  
  #data.table is a package to accelerate reading from files
  require(data.table)
  
  #arrivals
  A = data.frame(d = 0, arrival = 0, departure = 0, delta = 0, connection = 0)
  A = A[-1,]
    
  setwd(path)
  file_list <- list.files()
  for(i in file_list) {
    #a faster function to read csv files (from the package "data.table")
    Df = fread(i, header=FALSE, sep=",", stringsAsFactors=FALSE, select=c(1:10), colClasses=list(character=c(1,2,3,4,5,6,7,8)))
    D = as.data.frame(Df)
    arr = D[which(D[,1] == line1 & D[,3] == dir1 & D[,6] == t1 & D[,8] == stop1),]
    dep = D[which(D[,1] == line2 & D[,3] == dir2 & D[,6] == t2 & D[,8] == stop2),]
    Atemp = A
    delta = dep[1,10]-arr[1,9]
    
    if(!is.na(delta)) {
      if(delta - transfer >= 0) 
        connection = 1
      else
        connection = 0
      A = rbind(Atemp, data.frame(d = arr[1,7], arrival = arr[1,9], departure = dep[1,10], delta, connection))
    }
  }
  return(A)
  
}

FindDelays = function(path, line1, dir1, stop1) {
  
  #data.table is a package to accelerate reading from files
  require(data.table)
  
  A = data.frame(date = 0, time = 0, delay = 0)
  A = A[-1,]
  
  setwd(path)
  file_list <- list.files()
  for(i in file_list) {
    #a faster function to read csv files (from the package "data.table")
    Df = fread(i, header=TRUE, sep=",", stringsAsFactors=FALSE, select=c(1:11), colClasses=list(character=c(1,2)))
    D = as.data.frame(Df)
    arr = D[which(D[,1] == line1 & D[,3] == dir1 & D[,8] == stop1),]
    Atemp = A
    A = rbind(Atemp, data.frame(date = arr[,7], time = arr[,6], delay = arr[,11]))
    
  }
  return(A)
  
}

FindJunctionData = function(path, junction) {
  
  #data.table is a package to accelerate reading from files
  require(data.table)
  
  A = data.frame(time = 0, location = 0, sequenceNumber = 0, tickCount = 0)
  A = A[-1,]
  
  setwd(path)
  file_list <- list.files()
  for(i in file_list) {
    #a faster function to read csv files (from the package "data.table")
    Df = fread(i, header=TRUE, sep=",", stringsAsFactors=FALSE, select=c(1:4))
    D = as.data.frame(Df)
    arr = D[which(D[,1] == junction),]
    Atemp = A
    A = rbind(Atemp, data.frame(time = arr[,3], location = arr[,1], sequenceNumber = arr[,2],
                                tickCount = arr[,4]))
    
  }
  
  A = unique(A) #remove duplicates
  #return(A)
  write.table(A, file = "test.csv", sep = ",", row.names = FALSE, col.names = TRUE)
  
}


MinConvert = function(minutes) {
  
  if (minutes < 15) 
    m = 0
  else if (minutes < 30)
    m = 15
  else if (minutes < 45)
    m = 30
  else if (minutes < 60)
    m = 45
  else 
    m = 0
  
  return(m)
}

#F = ProcessFilesVC('C:/Users/eb97593/Desktop/uta work/products/trafficdataAPI/Prisma/TRE507', "TRE507", 1, 1)
ProcessFilesVC = function(path, jun, ftype = 1, changetime = 0) {
  
  #data.table is a package to accelerate reading from files
  require(data.table)
  
  A = data.frame(junction = "", date = "", time_end = "", vehicles = "")
  A = A[-1,]
  
  setwd(path)
  file_list <- list.files()
  for(i in file_list) {
    Df = fread(i, header=FALSE, sep="&", stringsAsFactors=FALSE, select=c(1:1))
    D = as.data.frame(Df)
    if (ftype == 1) {
      
      d = gsub(".", "-", substr(D[4,1], 7, nchar(D[4,1])), fixed = TRUE) #date YYYY-MM-DD
      if ((jun == "TRE507") | (jun == "TRE753")) #the first 19 elements are inductive loops
        n = 19
      else if (jun == "TRE610") #the first 38 elements are inductive loops
        n = 38
      else if (jun == "TRE668") 
        n = 18
      else if (jun == "TRE702") 
        n = 15
      else if ((jun == "TRE713") | (jun == "TRE725"))
        n = 14
      else n = 0
      
      v_prev = rep(0, n)
      
      for(i in 7:nrow(D)) { 
        t = substr(D[i,1], 1, 5) #time HH:MM
        #v = substr(D[i,1], 7, nchar(D[i,1])) #vehicles
        v_list = as.integer(unlist(strsplit(substr(D[i,1], 7, nchar(D[i,1])),",")))
        if (changetime == 0) {
          v = paste0(unlist(v_list[1:n]),",", collapse = "")
        }
        else {
          #here we convert end_time to the periods 00, 15, 30, 45
          minutes = as.integer(substr(t, 4, 5))
          #print(paste0(d, ' ', t,' ', minutes, collapse = ""))
          m = MinConvert(minutes)
          p = (15 - (minutes - m))/15 #percentage of vehicles to get
          t = paste0(substr(t, 1, 3), sprintf("%02d", m), collapse = "")
          new_list = unlist(lapply(seq_along(v_list),function(i) round(v_list[i]*p)+v_prev[i]))
          v_prev = round(v_list*(1-p))
          v = paste0(unlist(new_list[1:n]),",", collapse = "")
        }
        
        Atemp = A
        A = rbind(Atemp, data.frame(junction = as.character(jun), date = d, time_end = t, vehicles = v))
      }
    }
    
    else {
      if (jun == "TRE854") 
        n = 25
      else if (jun == "TRE535") 
        n = 34
      
      v_prev = rep(0, n)
      
      for(i in 2:nrow(D)) { 
        d = substr(D[i,1], 21, 30) #date
        t = substr(D[i,1], 32, 36) #time
        v_list = as.integer(unlist(strsplit(substr(D[i,1], 41, nchar(D[i,1])),";")))
        if (changetime == 0) {
          v = paste0(unlist(v_list[1:n]),",", collapse = "")
        }
        else {
          #here we convert end_time to the periods 00, 15, 30, 45
          minutes = as.integer(substr(t, 4, 5))
          m = MinConvert(minutes)
          p = (15 - (minutes - m))/15 #percentage of vehicles to get
          t = paste0(substr(t, 1, 3), sprintf("%02d", m), collapse = "")
          new_list = unlist(lapply(seq_along(v_list),function(i) round(v_list[i]*p)+v_prev[i]))
          v_prev = round(v_list*(1-p))
          v = paste0(unlist(new_list[1:n]),",", collapse = "")
        }
        
        Atemp = A
        A = rbind(Atemp, data.frame(junction = as.character(jun), date = d, time_end = t, vehicles = v))
      }
    }
      
  }
  
  if (changetime == 0) 
    ending = ""
  else
    ending = "_newtime"
  
  write.table(A, 
              file = paste("C:/Users/eb97593/Desktop/uta work/products/trafficdataAPI/Prisma/" ,
              jun, ending, ".csv", sep=""),
              sep = ",", row.names = FALSE, col.names = TRUE)
  
  return(A)
  
}