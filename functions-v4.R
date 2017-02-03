
# Function to produce old age dependency ratio
oadr <- function(x, sim, pension.age=65, historical.pension.age=65, level=80)
{  
  adjustment<-(historical.pension.age%%1)*colSums(extract.ages(x,as.integer(historical.pension.age)+1,FALSE)$pop$total,na.rm=TRUE)
  workers <-colSums(extract.ages(x,15:as.integer(historical.pension.age),FALSE)$pop$total,na.rm=TRUE)+adjustment
  aged <- colSums(extract.ages(x,as.integer(historical.pension.age):max(x$age),TRUE)$pop$total,na.rm=TRUE)-adjustment
  
  xhistory <- ts(aged/workers,s=x$year[1],f=1) 

  ##ts is "time series" s is starting f is frequency
  
  h <- dim(sim[[1]])[2]
  N <- dim(sim[[1]])[3]
  if(length(pension.age)==1)
    pension.age <- rep(pension.age, h)
  if(length(pension.age) != h)
    stop("length of pension.age should be equal to forecast horizon")
  # Check that pension.age does not decrease
  if(max(diff(pension.age)) < 0)
    stop("Pension ages should not decrease")
  # Check that pension.age does not increase too quickly
  if(max(diff(pension.age)) > 1)
    stop("Pension age should not rise more than 1 year at a time")
  
  simages <- as.numeric(dimnames(sim[[1]])[[1]]) # Ages vector from sim
  # Set up matrices to store populations from each forecast horizon and each simulation
  male.workers <- female.workers <- male.aged <- female.aged <- matrix(NA,nrow=h,ncol=N)
  
  for (i in (1:h))
  {
    worker.rows <- (simages >= 15 & simages < as.integer(pension.age[i]))
    pension.rows <- (simages >= as.integer(pension.age[i]))
    adjustment.male<-(pension.age[i]%%1)*sim$male[as.integer(pension.age[i]),i,]
    adjustment.female<-(pension.age[i]%%1)*sim$female[as.integer(pension.age[i]),i,]
    male.workers[i,] <- colSums(sim$male[worker.rows,i,])+adjustment.male
    female.workers[i,] <- colSums(sim$female[worker.rows,i,])+adjustment.female
    male.aged[i,] <- colSums(sim$male[pension.rows,i,])-adjustment.male
    female.aged[i,] <- colSums(sim$female[pension.rows,i,])-adjustment.female
  
  }
  
  oadp.f <- (male.aged+female.aged)/(male.workers+female.workers)
  oadp.lo <- apply(oadp.f,1,quantile,prob=(0.5-level/200))
  oadp.hi <- apply(oadp.f,1,quantile,prob=1-(0.5-level/200))
  
  firstyear <- min(as.numeric(dimnames(sim[[1]])[[2]]))
  oadp.f <- structure(list(mean=ts(rowMeans(oadp.f),s=firstyear+1),x=xhistory,
                           upper=ts(oadp.hi,s=firstyear+1),lower=ts(oadp.lo,s=firstyear+1),
                           level=level),class="forecast") #s=firstyear+1?
  return(oadp.f)
}
