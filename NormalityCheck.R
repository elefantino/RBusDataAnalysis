#NORMALITY CHECK

#Transformation to normality
TransToNorm = function(trans, t, sig = 2) {
  if (trans == 1)       #inverse
    d = 1/(t + 901)
  else if (trans == 2)  #lognormal
    d = log(t + 901, 2.7182818)
  else if (trans == 3)  #square root
    d = sqrt(t + 901)
  else if (trans == 4)  #minutes
    d = round(t/60)
  else # trans == 0 
    d = t
  st = round(shapiro.test(d)$p.value, sig)
  return(st)
}

#Normality plots of delays. outliers (more than 15 min) are excluded
#Arguments:
# day is an ordering number of the weekday for selection
# routeId is a route identificator for selection
# timeMin is the minimum time (hours) for selection
# timeMax is the maxinmum time (hours) for selection (including)
# step is a mark for a factor: 0 - route id, -1 - weekday, 1 - hour
# trans is an mark for data transformation (see TransToNorm())
# tableview is a mark to form the table instead of the plot
# sig is a number of significant dif´gits to round values (default = 2)
#Example of use:
# t = NormalityPlot(S16_501, trans = 0, step = 0, timeMin = 17, timeMax = 23)
NormalityPlot = function(A, tableview = 0, trans = 0, step = 0, day = 0, routeId = 0, timeMin = 0, timeMax = 0, sig = 2) {
  
  titleEnd = ""
  titleStart = "Bus delays distributions,\n"
  legendc = 1
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(fBasics)
  
  distribution = "name"
  n = 0
  m = 0
  sd = 0
  skewness = 0
  kurtosis = 0
  normal = 0
  lognormal = 0 
  t = data.frame(distribution, n, m, sd, skewness, kurtosis, normal, lognormal, stringsAsFactors=FALSE)
  #t = t[-1,]
  
  #filter
  A = A[which((A[,6]-900) < 0 & (A[,6]+900) > 0 & A[,4] < 6 & A[,3] != "2412" & A[,3] != "2612" & A[,3] != "0601" & A[,3] != "0101" & A[,3] != "1804" & A[,3] != "2104"),] #remove outliers more than 15 min, holidays and weekends
  if (day != 0) {
    A = A[which(A[,4] == day),] #day number 
    titleEnd = paste(", filter = weekday: ", day, sep ="")
  }  
  if (timeMin != 0 | timeMax != 0) {
    A = A[which(A[,7] >= timeMin & A[,7] <= timeMax),] #time period
    titleEnd = paste(titleEnd, ", filter = arrival time: ", timeMin, ":00-", timeMax, ":59", sep ="")
  }  
  if (routeId != 0) {
    A = A[which(A[,5] == routeId),] #route id
    titleEnd = paste(titleEnd, ", filter = route: ", routeId, sep ="")
  }  
  
  #factor
  titleFactor = ""
  #day
  if (step  == -1) {
    ls = c("Mon","Tue","Wed","Thu","Fri")
    for (i in 1:5) {
      Atemp = A[which(A[,4] == i),]
      if (nrow(Atemp) > 2) {
        if (tableview == 1) {
          t[i,1] = ls[i]
          t[i,2] = nrow(Atemp)
          t[i,3] = round(mean(Atemp$delay), sig)
          t[i,4] = round(sd(Atemp$delay), sig)
          t[i,5] = round(skewness(Atemp$delay), sig)
          t[i,6] = round(kurtosis(Atemp$delay), sig)
          t[i,7] = TransToNorm(0, Atemp$delay, sig)
          t[i,8] = TransToNorm(2, Atemp$delay, sig) #lognormal
        }  
        else {
          st = TransToNorm(trans, Atemp$delay)
          ls[i] = paste(ls[i]," p=",st,", n=",nrow(Atemp),sep="")
        }
      }  
    }
    if (tableview == 0) A$factor <- factor(A$weekday, levels=c(1:5), labels=ls)
    titleFactor = "factor = weekday"
  }
  #route id
  else if (step  == 0) {
    legendc = 3
    ls = sort(unique(A$route))
    ls_init = ls
    j = 1
    for (i in 1:length(ls)) {
      Atemp = A[which(A[,5] == ls[i]),]
      if (nrow(Atemp) > 2) {
        if (tableview == 1) {
          t[j,1] = paste("Route ",ls[i],sep="")
          t[j,2] = nrow(Atemp)
          t[j,3] = round(mean(Atemp$delay), sig)
          t[j,4] = round(sd(Atemp$delay), sig)
          t[j,5] = round(skewness(Atemp$delay), sig)
          t[j,6] = round(kurtosis(Atemp$delay), sig)
          t[j,7] = TransToNorm(0, Atemp$delay, sig)
          t[j,8] = TransToNorm(2, Atemp$delay, sig) #lognormal
          j = j + 1
        }
        else {
          st = TransToNorm(trans, Atemp$delay)
          #ls[i] = paste("Route ",ls[i],"  p=",st,", n=",nrow(Atemp),sep="")
          ls[i] = paste("Route ",ls[i],sep="")
        }
      }  
    }
    if (tableview == 0) A$factor <- factor(A$route, levels=ls_init, labels=ls)
    titleFactor = "factor = route"
  }
  #hour
  else if (step == 1) {
    legendc = 1
    ls = c(0:23)
    j = 1
    for (i in 1:24) {
      Atemp = A[which(A[,7] == ls[i]),]
      if (nrow(Atemp) > 2) {
        if (tableview == 1) {
          t[j,1] = paste(ls[i],":00-",ls[i],":59",sep="")
          t[j,2] = nrow(Atemp)
          t[j,3] = round(mean(Atemp$delay), sig)
          t[j,4] = round(sd(Atemp$delay), sig)
          t[j,5] = round(skewness(Atemp$delay), sig)
          t[j,6] = round(kurtosis(Atemp$delay), sig)
          t[j,7] = TransToNorm(0, Atemp$delay, sig)
          t[j,8] = TransToNorm(2, Atemp$delay, sig) #lognormal
          j = j + 1
        }
        else {
          st = TransToNorm(trans, Atemp$delay)
          #ls[i] = paste(ls[i],":00-",ls[i],":59  p=",st,", n=",nrow(Atemp),sep="")
          ls[i] = paste(ls[i],":00-",ls[i],":59",sep="")
        }  
      }
    }  
    if (tableview == 0) A$factor <- factor(A$scheduledhour, levels=c(0:23), labels=ls)
    titleFactor = "factor = hour"
  }
  
  if (tableview == 0) { 
    p = ggplot(data = A, aes(x=delay, colour=factor)) + geom_density() +
      #ggtitle(paste(titleStart, titleFactor, titleEnd, "\n", sep="")) +
      ylim(0, 0.03) + xlim(-600, 900) +
      guides(col=guide_legend(ncol=legendc))
    return(p)
  }  
  else 
    return(t)
  
}