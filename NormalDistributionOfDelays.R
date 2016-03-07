#Normal distribution for delays between lines

NormalPosteriorBatch = function(m,w,v,y){
  
  mn=(m/w+sum(y)/v)/(1/w+length(y)/v)
  wn=1/(1/w+length(y)/v)
  return(c(mn,wn))
  
}

ProbabilityOfConnectionSuccessNormal = function(m1,w1,m2,w2,ds,DT){
  
  # how much do the expected delays of both lines change the connection time
  # this is also the mean of the distribution of d2-d1
  dc = m2-m1
  # the variation of d2-d1
  wc = w1+w2
  # probability of d2-d1 > DT-ds
  p = pnorm(DT-ds, mean=dc, sd=sqrt(wc), lower.tail=FALSE)
  return(p)
  
}

ProbabilityVsConnectionTime = function(line1, d1, stop1, M1, line2, d2, stop2, M2, STOPS, DT){
  
  v = 100^2
  d1 = DelayAtStop(line1, d1, STOPS, stop1, M1, 1)
  d2 = DelayAtStop(line2, d2, STOPS, stop2, M2, 1)
  # choose the number of observations used for the posterior
  N = 10
  N1 = round(length(d1))-N
  N2 = round(length(d2))-N
  #1st line
  mw1 = NormalPosteriorBatch(mean(d1[1:N1]), sd(d1[1:N1])^2, v, d1[(length(d1)-N+1):length(d1)])
  print(mw1)
  #2d line
  mw2 = NormalPosteriorBatch(mean(d2[1:N2]), sd(d2[1:N2])^2, v, d2[(length(d2)-N+1):length(d2)])
  print(mw2)
  
  minctime = -5
  maxctime = 5
  times = ((2*minctime):(2*maxctime))*0.5
  p = rep(0,length(times))
  for(i in 1:length(times)){
    p[i] = ProbabilityOfConnectionSuccessNormal(mw1[1], mw1[2], mw2[1], mw2[2], times[i]*60, DT)
  }
  plot(times, p)
  return(p)
  
}

ProbabilityDistributionFunction = function(D13prior, D13post, v) {
  
  mw = NormalPosteriorBatch(mean(D13prior$delay), sd(D13prior$delay)^2, v, D13post$delay)
  minctime = -200
  maxctime = 400
  t = (minctime):(maxctime)
  p = rep(0,length(t))
  for(i in 1:length(t)){
    #p[i] = pnorm(t[i]*60, mw[1], sqrt(mw[2]),lower.tail=FALSE)
    p[i] = (1 / (mw[2] * sqrt(2*pi))) * exp( -.5 * ((t[i] - mw[1])/mw[2])^2)
  }
  plot(t, p, xlab = "delay, sec", ylab = "p(delay)", main = "Normal probability density function\n for line 13 dep 2 stop 566 28.04.14-02.05.2014")
  lines(t, p)
  return(p)
  
}