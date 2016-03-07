#Loading data from all files in the directory (pathtodir) to one data frame
#Arguments:
# pathtodir is a path to files
InitDFromDir = function (pathtodir) {
  
  setwd(pathtodir)
  file_list <- list.files()
  result <- do.call(rbind, lapply(file_list, read.csv))
  return(result)
  
}

SimpleDistance = function (latA,lonA,latB,lonB) {
  
  R1 = 6370000; # earth radius, meters
  R2 = R1*cos(latA[1]);  # R1*cos(latitude)
  
  dlat=R1*(latA-latB)
  dlon=R2*(lonA-lonB)
  d=sqrt(dlat*dlat+dlon*dlon)
  return(d)
}

SimpleDistanceDeg = function (deglatA,deglonA,deglatB,deglonB) {
  
  latA=deglatA*pi/180
  lonA=deglonA*pi/180
  latB=deglatB*pi/180
  lonB=deglonB*pi/180
  return(SimpleDistance(latA,lonA,latB,lonB))
  
}

#Creating a data frame. Depending on "res" the format of data frame can be:
# 1 (arrivals, full info) = arrivalhour+arrivaltime+date+weekday+route_id+delay+time(H:M:S)+...
# 2 (delays) = vector of delays
# 3 (schedule) = scheduledtime+date+weekday+route
#Other arguments: 
# line is a line identificator or a vectore with line ids
# direction is 1 or 2 
# routeStartTime (optionaly, default = 0) is the departure time from the first busstop in the route
# stopid is a busstop identificator
# STOPS is a data frame with data from busstops files
# D is a data frame with data from busdata files
Arrivals = function (line, direction, routeStartTime = 0, stopid, STOPS, D, res = 1) {
  
  deg2rad = pi/180
  coordinates = which(STOPS$stop_code == stopid)
  stoplat = STOPS$lat[coordinates] * deg2rad
  stoplon = STOPS$lon[coordinates] * deg2rad
  
  #select data of a single line in a single direction -> LD
  if(routeStartTime == 0){
    LD = D[which(D[,1] %in% line & D[,3] == direction),]
  }else{
    LD = D[which(D[,1] %in% line & D[,3] == direction & D[,4] == routeStartTime),]
  }
  
  Mlat = LD[,5] * deg2rad
  Mlon = LD[,6] * deg2rad
  
  #select data within the radius "stoparea" of the stopid for each Mlat and Mlon -> S
  stoparea = 15
  S = which(SimpleDistance(Mlat,Mlon,stoplat,stoplon) < stoparea)
  if (length(S) < 1) { return(NULL) }
  
  #select data of a single line in a single direction in the area of the busstop -> LDS
  LDS = LD[S,] 
  
  #select datalines for arrivals
  i = which(diff(LDS[,2]/1000) > 120) #the difference between time in lines more than 2 minutes
  t = c(1, (i + 1))
  if (t[length(t)] > nrow(LDS)) { t = t[-length(t)] }
  
  #arrivals
  if (res == 1) {  
    arrivalhour = as.integer(format(as.POSIXlt(LDS[t,2]/1e3, origin="1970-01-01", tz="Europe/Helsinki"), "%H"))
    arrivaltime = LDS[t,9]
    #arrivaltime = format(as.POSIXlt(LDS[t,2]/1e3, origin="1970-01-01", tz="Europe/Helsinki"), "%H%M%S") 
    date = format(as.POSIXlt(LDS[t,2]/1e3, origin="1970-01-01", tz="Europe/Helsinki"), "%d%m")
    month = format(as.POSIXlt(LDS[t,2]/1e3, origin="1970-01-01", tz="Europe/Helsinki"), "%m")
    weekday = format(as.POSIXlt(LDS[t,2]/1e3, origin="1970-01-01", tz="Europe/Helsinki"), "%u")
    route = LDS[t,4]
    delay = LDS[t,8]
    scheduledtime = format(as.POSIXlt(LDS[t,2]/1e3 - LDS[t,8], origin="1970-01-01", tz="Europe/Helsinki"), "%H%M%S") #scheduled time
    scheduledhour = as.integer(format(as.POSIXlt(LDS[t,2]/1e3 - LDS[t,8], origin="1970-01-01", tz="Europe/Helsinki"), "%H")) #scheduled hour
    busstop = stopid
    f = data.frame(arrivalhour, arrivaltime, date, weekday, route, delay, scheduledhour, scheduledtime, month, busstop, stringsAsFactors = FALSE)
    #filter
    #f = A[which((A[,6]-900) < 0 & (A[,6]+900) > 0 & A[,4] < 6 & A[,3] != "2412" & A[,3] != "2612" & A[,3] != "0601" & A[,3] != "0101" & A[,3] != "1804" & A[,3] != "2104"),] #remove outliers more than 15 min, holidays and weekends
  }
  #delays
  else if (res == 2) {
    f = LDS[t,8]
  }
  #schedule
  else {
    scheduledtime = format(as.POSIXlt(LDS[t,2]/1e3 - LDS[t,8], origin="1970-01-01", tz="Europe/Helsinki"), "%H%M%S") #arrival time - delay
    date = format(as.POSIXlt(LDS[t,2]/1e3, origin="1970-01-01", tz="Europe/Helsinki"), "%d%m%Y")
    weekday = format(as.POSIXlt(LDS[t,2]/1e3, origin="1970-01-01", tz="Europe/Helsinki"), "%u")
    route = LDS[t,4]
    f = data.frame(scheduledtime, date, weekday, route, stringsAsFactors = FALSE)
  }
  
  return (f)
  
}

#Creating a dataframe according to Arrivals()
#Arguments:
# pathS is a path to the directory with busstops files
# pathD is a path to the directory with busdata files 
#Example of use: 
# A13 = ArrivalsAllFiles("C:/Users/eb97593/Desktop/uta work/Renvironment/DataSet/busdata/", STOPS, 13, 2, 0, 4016, 1)
#Example of seconds transformation to arrival time:
# paste(S22_4016_$arrivaltime%/%3600,":",(S22_4016_$arrivaltime%%3600)%/%60,":",(S22_4016_$arrivaltime%%3600)%%60,sep="")
#Example of plotting the result:
# #par(bty='n') #remove borders
# boxplot(S13_4016$delay~S13_4016$route, xlab="Route", ylab="Delay (sec)", ylim = c(-500, 1300))
# plot(density(S13_4016$delay), xlab="Delay (sec)", ylab="Density", main="Delays during weekdays,\n Koskipuisto, line 16, direction 2")
# p = shapiro.test(Prior16_2_502$delays)
# text(800, 0.0015, bquote(p == .(p$p.value)), adj=c(0.5,-1.0), cex=1.2)
ArrivalsAllFiles = function (pathD, STOPS, line, direction, routeStartTime = 0, stopid, res = 1) {
  
  #data.table is a package to accelerate reading from files
  require(data.table)
  
  #arrivals
  if (res == 1) {  
    A = data.frame(arrivalhour=0, arrivaltime=0, date=0, weekday=0, route=0, delay=0, scheduledhour=0, scheduledtime=0, month=0, busstop=0)
  }
  #delays
  else if (res == 2) {
    A = data.frame(delay=0)
  }
  #schedule
  else {
    A = data.frame(scheduledtime=0, date=0, weekday=0, route=0)
  }  
  A = A[-1,]
  
  setwd(pathD)
  file_list <- list.files()
  for(i in file_list) {
    #a faster function to read csv files (from the package "data.table")
    Df = fread(i, header=TRUE, sep=",", stringsAsFactors=FALSE, select=c(1:9), colClasses=list(double=2))
    Dtemp = as.data.frame(Df)
    Atemp = A
    A = rbind(Atemp, Arrivals(line, direction, routeStartTime, stopid, STOPS, Dtemp, res))
  }
  return(A)
  
}

#route plotting in google maps
#! requires the package ggmap
#Arguments:
# n is a number of routes to plot
# vectors of route parameters
# line1, dir1, stopstart1, stopfinish1, line2, dir2, stopstart2, stopfinish2, routecolor
#Example of use:
# m = PlotRoute(5, c(13,18,18,25,29), c(2,2,2,2,1), c(1519,1597,1601,2005,5075), c(4016,508,502,10,5003), 
#   c(22,2,5,30,30), c(2,2,2,2,2), c(4016,508,520,1,5004), c(4028,5021,3168,3615,3734), c("red", "green", "blue", "purple", "yellow"))
#v2
# windows(9,9)
# m = ggmap(get_googlemap(center='tampere', zoom=12, maptype='roadmap'), extent='device')
# A2 = PlotRoute(line1[2], dir1[2], start1[2], finish1[2], line2[2], dir2[2], start2[2], finish2[2])
# m = m + geom_point(data=A2, aes(x=lon,y=lat), colour='blue', alpha=0.7) +
#    geom_path(data=A2, mapping=aes(x=lon,y=lat), size=1, color="red")
PlotRoute = function (line1, dir1, start1, finish1, line2, dir2, start2, finish2) {
  
  setwd("C:/Users/eb97593/Desktop/uta work/Renvironment/DataSet/busstops/")
  #v1
  #windows(9,9)
  #m = ggmap(get_googlemap(center='tampere', zoom=12, maptype='roadmap'), extent='device')
#   for(ind in 1:length(n) {
#     i = n[ind]
#     A1 = read.csv(paste0("stops_", line1[i], "_d", dir1[i], ".csv"))
#     s = A1[which(A1$stop_code == start1[i]),1]
#     f = A1[which(A1$stop_code == finish1[i]),1]
#     A1 = A1[s:f,]
#     A2 = read.csv(paste0("stops_", line2[i], "_d", dir2[i], ".csv"))
#     s = A2[which(A2$stop_code == start2[i]),1]
#     f = A2[which(A2$stop_code == finish2[i]),1]
#     A2 = A2[s:f,]
#     A = rbind(A1, A2)
#     m = m + 
#       geom_point(data=A, aes(x=lon,y=lat), colour='blue', alpha=0.7) +
#       geom_path(data=A, mapping=aes(x=lon,y=lat), size=1, color=routecolor[i])
#   }
  A1 = read.csv(paste0("stops_", line1, "_d", dir1, ".csv"))
  s = A1[which(A1$stop_code == start1),1]
  f = A1[which(A1$stop_code == finish1),1]
  A1 = A1[s:f,]
  A2 = read.csv(paste0("stops_", line2, "_d", dir2, ".csv"))
  s = A2[which(A2$stop_code == start2),1]
  f = A2[which(A2$stop_code == finish2),1]
  A2 = A2[s:f,]
  A = rbind(A1, A2)
  return(A)
}


#!!!I didn't finish it!!!
#line plotting in google maps
#! requires the package ggmap
#Arguments:
# line is a line number
# dir is a direction (1 or 2)
# D is a dataframe received from Arrivals
#Example of use:
PlotLineHour = function (line, dir, Ars) {
  
  A = read.csv(paste0("/home/trafficdata/test/busstops/stops_", line, "_d", dir, ".csv"))
  lon = 0
  lat = 0
  normal = ""
  t = data.frame(lat, lon, normal)
  t = t[-1,]
  
  Ars = ArrivalsAllFiles("~/test/1", STOPS, line, dir, 0, A$stop_code, 1)
  
  for (i in nrow(A)) {
    
    
  }
  
  m = ggmap(get_googlemap(center='tampere', zoom=12, maptype='roadmap'), extent='device') +
    geom_point(data=t, aes(x=lon,y=lat), colour=normal, alpha=0.7) +
    geom_path(data=t, mapping=aes(x=lon,y=lat), size=1, color="grey")
  return(m)
  
}

#Queries to SQL db
#Example of use:
# x = SQLQuery("", "", "SELECT id, sequence_number, grint, dint, detector_signal_levels, location_id, 
#        tick_count, date_time FROM signals
#              WHERE date_time >= TO_TIMESTAMP('2014-08-13 15','yyyy-mm-dd HH24')
#              AND date_time < TO_TIMESTAMP('2014-08-13 17','yyyy-mm-dd HH24');")
# write.csv(file="/home/trafficdata/test/signals.csv",x)
SQLQuery = function(user, pass, q) {
  
  host = "db1.sis.uta.fi"
  db = "trafficdata"
  
  con = dbConnect(PostgreSQL(), host = host, user = user, password = pass, dbname = db)
  rs = dbSendQuery(con, q)
  
  df = fetch(rs, n = -1)
  return (df)
  
}


#DISTRIBUTION CHECK

#Create a table with distributions parameters
#Arguments: 
# data is a numeric vector of observations of unknown distribution
# fit is a list of distributions to fit data
# sig is a number of significant digits to round
# s is a need to sort (default = 0 , no sorting) 
#! for Weibul distribution parameters are defined using fitdist in fitdistrplus package
#Example of use:
# t = FitData(A12$delay+901, fit=c("normal","lognormal","weibull","gamma","logistic","exponential","poisson"), sig=4, s=1)
FitData = function(data, fit=c("normal","lognormal","weibull","gamma","logistic","exponential","poisson"), sig=2, s=0) {
  
  require(fitdistrplus)
  
  distrib = list()
  numfit = length(fit)
  results = matrix(0, ncol=5, nrow=numfit)
  colnames(results) = c("distribution", "param1", "param2", "ksstat", "kspvalue")
  
  for(i in 1:numfit){
    if((fit[i] == "gamma") | (fit[i] == "poisson") | (fit[i] == "weibull") | 
         (fit[i] == "exponential") | (fit[i] == "logistic") | (fit[i] == "normal") | 
         (fit[i] == "lognormal") | (fit[i] == "geometric")) 
      distrib[[i]] = fit[i]
    else stop("Provide a valid distribution to fit data" )
  }
  
  for(i in 1:numfit) {
    
    #fd = fitdistr(data, distrib[[i]])
    #fd = tryCatch(fitdistr(data, distrib[[i]]), 
    #              finally = structure(list(estimate = c(NA,NA),sd = c(NA,NA), 
    #                    vcov = matrix(NA, ncol = 1, nrow = 1),
    #                    n = length(data), loglik = NA), class = "fitdistr"))
    #if(fd$estimate[[1]] == NA) results[i,] = c(distrib[[i]], 0, 0, 0, 0)
    
    er = FALSE
    tryCatch(
      {
        fd = fitdistr(data, distrib[[i]])
        #fd = fitdist(data, "weibull")
      },
      error = { er=TRUE },
      finally = {}
    )
    if (er == TRUE) results[i,] = c(distrib[[i]], NA, NA, NA, NA)
    else {
    
    fd = fitdistr(data, distrib[[i]])
    if(distrib[[i]] == "gamma") {
      est_shape = fd$estimate[[1]]
      est_rate = fd$estimate[[2]]
      ks = ks.test(data, "pgamma", shape=est_shape, rate=est_rate)
      results[i,] = c(distrib[[i]], round(est_shape,sig), round(est_rate,sig), round(ks$statistic,sig), round(ks$p.value,sig))
    }
    
    else if(distrib[[i]] == "poisson"){
      est_lambda = fd$estimate[[1]]
      ks = ks.test(data, "ppois", lambda=est_lambda)
      results[i,] = c(distrib[[i]], round(est_lambda,sig), "NA", round(ks$statistic,sig), round(ks$p.value,sig))
    }
    
    else if(distrib[[i]] == "weibull"){
      #fd = fitdistr(data,densfun=dweibull,start=list(scale=1,shape=2))
      #fd = fitdist(data, "weibull",lower=c(0.01,0.01))
      fd = fitdist(data, "weibull")
      est_shape = fd$estimate[[1]]
      est_scale = fd$estimate[[2]]
      ks = ks.test(data, "pweibull", shape=est_shape, scale=est_scale)
      results[i,] = c(distrib[[i]], round(est_shape,sig), round(est_scale,sig), round(ks$statistic,sig), round(ks$p.value,sig)) 
    }
    
    else if(distrib[[i]] == "normal"){
      est_mean = fd$estimate[[1]]
      est_sd = fd$estimate[[2]]
      ks = ks.test(data, "pnorm", mean=est_mean, sd=est_sd)
      results[i,] = c(distrib[[i]], round(est_mean,sig), round(est_sd,sig), round(ks$statistic,sig), round(ks$p.value,sig))
    }
    
    else if(distrib[[i]] == "lognormal"){
      est_mean = fd$estimate[[1]]
      est_sd = fd$estimate[[2]]
      ks = ks.test(data, "plnorm", mean=est_mean, sd=est_sd)
      results[i,] = c(distrib[[i]], round(est_mean,sig), round(est_sd,sig), round(ks$statistic,sig), round(ks$p.value,sig))
    }
    
    else if(distrib[[i]] == "exponential"){
      est_rate = fd$estimate[[1]]
      ks = ks.test(data, "pexp", rate=est_rate)
      results[i,] = c(distrib[[i]], round(est_rate,sig), "NA", round(ks$statistic,3), round(ks$p.value,sig))
    }
    
    else if(distrib[[i]] == "logistic"){
      est_location = fd$estimate[[1]]
      est_scale = fd$estimate[[2]]
      ks = ks.test(data, "plogis", location=est_location, scale=est_scale)
      results[i,] = c(distrib[[i]], round(est_location,sig), round(est_scale,sig), round(ks$statistic,sig), round(ks$p.value,sig)) 
    }
    
    else if(distrib[[i]] == "geometric"){
      est_location = fd$estimate[[1]]
      est_scale = fd$estimate[[2]]
      ks = ks.test(data, "pgeom", location=est_location, scale=est_scale)
      results[i,] = c(distrib[[i]], round(est_location,sig), round(est_scale,sig), round(ks$statistic,sig), round(ks$p.value,sig)) 
    }
    }

  } #end of cycle
  
  if (s==1) results = results[order(results[,4]),] #sorting
  #print(results)
  return(results)
  
}

#P-values of delays distributions, outliers (more than 15 min) are excluded
#Arguments:
# A is a dataframe received in Arrivals()
# routeId is a route identificator for selection
# timeMin is the minimum time (hours) for selection
# timeMax is the maxinmum time (hours) for selection (including)
# step is a mark for a factor: 0 - route id, 1 - hour
# sig is a number of significant dif´gits to round values (default = 2)
#Example of use:
# t = FitDataTable(S16_501, step = 0, timeMin = 17, timeMax = 23)
FitDataTable = function(A, step = 0, routeId = 0, timeMin = 0, timeMax = 0, sig = 2) {
  
  normal = 0
  lognormal = 0
  weibull = 0
  gamma = 0
  logistic = 0
  #exponential = 0 
  #poisson = 0
  tab = data.frame(fact= "name", normal, lognormal, weibull, gamma, logistic, stringsAsFactors=FALSE)
  #tab = tab[-1,]
  percentage = data.frame(normal, lognormal, weibull, gamma, logistic)
  
  #filter
  A = A[which((A[,6]-900) < 0 & (A[,6]+900) > 0 & A[,4] < 6 & A[,3] != "2412" & A[,3] != "2612" & A[,3] != "0601" & A[,3] != "0101" & A[,3] != "1804" & A[,3] != "2104"),] #remove outliers more than 15 min, holidays and weekends
  if (timeMin != 0 | timeMax != 0) {
    A = A[which(A[,7] >= timeMin & A[,7] <= timeMax),] #time period
  }  
  if (routeId != 0) {
    A = A[which(A[,5] == routeId),] #route id
  }  
  
  #factor
  titleFactor = ""
  #route id
  if (step  == 0) {
    ls = sort(unique(A$route))
    j = 1
    for (i in 1:length(ls)) {
      Atemp = A[which(A[,5] == ls[i]),]
      if (nrow(Atemp) > 2) {
        tempstat = FitData(Atemp$delay+901, fit=c("normal","lognormal","weibull","gamma","logistic"), sig=2, s=0)
        tab[j,1] = paste("Route ",ls[i],sep="")
        tab[j,2] = tempstat[[1,5]]
        tab[j,3] = tempstat[[2,5]]
        tab[j,4] = tempstat[[3,5]]
        tab[j,5] = tempstat[[4,5]]
        tab[j,6] = tempstat[[5,5]]
        #percentage
        if (tempstat[[1,5]] >= 0.05) percentage[j,1] = 1 else percentage[j,1] = 0
        if (tempstat[[2,5]] >= 0.05) percentage[j,2] = 1 else percentage[j,2] = 0
        if (tempstat[[3,5]] >= 0.05) percentage[j,3] = 1 else percentage[j,3] = 0
        if (tempstat[[4,5]] >= 0.05) percentage[j,4] = 1 else percentage[j,4] = 0
        if (tempstat[[5,5]] >= 0.05) percentage[j,5] = 1 else percentage[j,5] = 0
        j = j + 1
      }  
    }
  }
  #hour
  else if (step == 1) {
    ls = c(0:23)
    j = 1
    for (i in 1:24) {
      Atemp = A[which(A[,7] == ls[i]),]
      if (nrow(Atemp) > 2) {
        #print(Atemp$delay+901)
        tempstat = FitData(Atemp$delay+901, fit=c("normal","lognormal","weibull","gamma","logistic"), sig=2, s=0)
        tab[j,1] = paste(ls[i],":00-",ls[i],":59",sep="")
        tab[j,2] = tempstat[[1,5]]
        tab[j,3] = tempstat[[2,5]]
        tab[j,4] = tempstat[[3,5]]
        tab[j,5] = tempstat[[4,5]]
        tab[j,6] = tempstat[[5,5]]
        #percentage
        if (tempstat[[1,5]] >= 0.05) percentage[j,1] = 1 else percentage[j,1] = 0
        if (tempstat[[2,5]] >= 0.05) percentage[j,2] = 1 else percentage[j,2] = 0
        if (tempstat[[3,5]] >= 0.05) percentage[j,3] = 1 else percentage[j,3] = 0
        if (tempstat[[4,5]] >= 0.05) percentage[j,4] = 1 else percentage[j,4] = 0
        if (tempstat[[5,5]] >= 0.05) percentage[j,5] = 1 else percentage[j,5] = 0
        j = j + 1
      }
    }  
  }
  
  tab[j,1] = "Percentage"
  tab[j,2] = round(sum(percentage$normal)/(j - 1),sig)
  tab[j,3] = round(sum(percentage$lognormal)/(j - 1),sig)
  tab[j,4] = round(sum(percentage$weibull)/(j - 1),sig)
  tab[j,5] = round(sum(percentage$gamma)/(j - 1),sig)
  tab[j,6] = round(sum(percentage$logistic)/(j - 1),sig)
  #tab[j,7] = round(sum(percentage$exponential)/(j - 1),sig)
      
  return(tab)
  
} #FitDataTable()

#Gathering statistics for the route
#Example of use: 
# A = FitDataTableNightLoad("/home/trafficdata/test/1/", STOPS, 27, 1, c(1512,1510))
# #study roote of line 27, dir 1
# A = FitDataTableNightLoad("/home/trafficdata/test/1/", STOPS, 27, 1, c(5121,5111,5109,5101,5097,5137,5099,5139,5057,4521,4519,4517,4515,4507,4505,4503,4501,513,511,505,501,15,16,58,60,1544,1502,1504,1506,1508,1510,1512,1010,1072,1014,1409,1052,1060,1064,1054,1056,1058))
# A2 = FitDataTableNightLoad("/home/trafficdata/test/1/", STOPS, 27, 1, STOPS27_2$stop_code)
FitDataTableNightLoad = function(path, STOPS, line, dir, stops) {
  
  result = data.frame(fact = "route", normal = 0, lognormal = 0, weibull = 0, gamma = 0, logistic = 0, stringsAsFactors=FALSE)
  for (i in 1:length(stops)) {
    print(i)
    tempframe = ArrivalsAllFiles(path, STOPS, line, dir, 0, stops[i], 1)
    temp = FitDataTable(tempframe, step = 1)
    result[i,1] = stops[i]
    j = nrow(temp)
    result[i,2] = temp[j,2]
    result[i,3] = temp[j,3]
    result[i,4] = temp[j,4]
    result[i,5] = temp[j,5]
    result[i,6] = temp[j,6]
  }
  
  return(result)
  
}

#Weibull distribution
#package "fitdistrplus"
# xtest <- rweibull(n=1000, shape=1.9, scale=1)
# plot(density(xtest))
# x = fitdistr(xtest,"weibull")
# summary(x)
# plot(x)

#restoring par:
#par(load("/home/trafficdata/test/R.default.par.RData"))