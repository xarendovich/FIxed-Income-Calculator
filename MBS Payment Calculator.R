library(tcltk)

mywait <- function() {
    tt <- tktoplevel()
    tkpack( tkbutton(tt, text='Yeah, press it', command=function()tkdestroy(tt)),
          side='bottom')
  tkbind(tt,'<Key>', function()tkdestroy(tt) )
  
  tkwait.window(tt)
}



cbal <- function(wac=6,age=1,wam=360) {
  return( (1-(1+wac/1200)^(age-wam))/(1-(1+wac/1200)^(-wam)))
}
psa2cpr <- function(p,a) { return( (a <= 30)*p/100*a/30*6 + (a>30)*p/100*6 ) }
cpr2smm <- function(c) { return( 100*(1-(1-c/100)^(1/12))) }
cpr2psa <- function(c,a) { return( (a <= 30)*c/(a*.2)*100 + (a>30)*c/6*100 ) }
smm2cpr <- function(s) { return( 100*(1-(1-s/100)^12)) }
psa2smm <- function(p,a) { return(cpr2smm(psa2cpr(p,a))) }
smm2fac <- function(smm,f) { return(f*(1-smm/100)) }
mpmt <- function(trm,rt,amt) { return(amt*rt/1200/(1-(1+rt/1200)^(-trm))) }
ALife <- function(wam,prin) { return(sum((1:wam)*prin)/sum(prin)*1/12) }
CvtYld <- function(r,n,m) { return( m * ( (1+r/n)^(n/m) - 1)) }

genPP <- function(typ=1,sp=6,wam=360,age=1) {
  if(typ==1)
    return(cpr2smm(sp*array(1,c(wam,1)))/100)
  else if(typ==2)
    return(psa2smm(sp,age:(wam+age-1))/100)
  else {
    "Prepay model not defined"
    return(-1)
  }
}

MBScf <- function(bal,wam,wac,svc,ptyp=1,speed=6,age=1) {
  # fac M Int Srv Sch PP CF Bal Prin
  pp <- genPP(ptyp,speed,wam,age)
  d <- array(0,c(wam,9))
  N <- nrow(d)
  pmt <- mpmt(wam,wac,bal)
  ni <- (wac-svc)/1200
  ss <- svc/1200
  d[,1] <- c(1,cumprod(1-pp[1:(N-1)]))
  d[,2] <- pmt*d[,1]
  d[1,3:4] <- bal*c(ni,ss)
  d[1,5] <- d[1,2]-d[1,3]-d[1,4]
  d[1,6] <- pp[1]*(bal-d[1,5])
  d[1,7] <- sum(d[1,c(3,5,6)])
  d[1,8] <- bal-d[1,6]-d[1,5]
  for(i in 2:N) {
    d[i,3:4] <- d[i-1,8]*c(ni,ss)
    d[i,5] <- d[i,2]-d[i,3]-d[i,4]
    d[i,6] <- pp[i]*(d[i-1,8]-d[i,5])
    d[i,7] <- sum(d[i,c(3,5,6)])
    d[i,8] <- d[i-1,8]-d[i,6]-d[i,5]
  }
  d[d[,6]<0,6] <- 0
  d[d[,8]<0,8] <- 0
  d[,9] <- d[,5] + d[,6]
  d <- data.frame(d)
  names(d) <- c('fac', 'M', 'Int', 'Svc', 'Sch', 'PP', 'CF', 'Bal', 'Prin')
  return(d)
}
#1 Generate cash flows and for 100, 300, 500, 900 percent PSA. Solve for average life 


  MBScf(25000000,355,4.5,.5,2,100,5) [,7] #100% PSA 
      mywait()
  ALife(355,MBScf(25000000,355,4.5,.5,2,100,5) [,9]) #Average life for 100% PSA
      mywait()
  MBScf(25000000,355,4.5,.5,2,300,5) [,7] #300% PSA
      mywait()
  ALife(355,MBScf(25000000,355,4.5,.5,2,300,5) [,9]) #Average life for 300% PSA
  mywait()
  MBScf(25000000,355,4.5,.5,2,500,5) [,7] #500% PSA
  mywait()
  ALife(355,MBScf(25000000,355,4.5,.5,2,500,5) [,9]) #Average life for 500% PSA
  mywait()
  MBScf(25000000,355,4.5,.5,2,900,5) [,7] #900% PSA
  mywait()
  ALife(355,MBScf(25000000,355,4.5,.5,2,900,5) [,9]) #Average life for 900% PSA
  mywait()

  #2
  print("Scheduled balance next month") #A
schbal <- cbal(3.45,1,325)*75000000
  schbal
smm    <- 100*(1-(74125000/schbal))
  smm2cpr(smm)                          #B
  cpr2psa(smm2cpr(smm),28)          #C
  mywait()
  
  #3
    accint <- .035/12*75000000*(16/30) 
    accint #A
    curtrsy <- 1.86   #Current Treasury Yield
    MBSy <- curtrsy+1.35 
    MBSy #B
   
    C3 <- MBScf2Price(350,4.15,3.5,2,275,10,MBSy/100,24) 
    C3
    print("101-4+") #C
    
    invoice <- 75000000*(C3/100)+accint
    invoice
    
    
    
  
  
MBSprice <- function(yld,cf,prin) {
  return( 100/sum(prin)*sum(cf/(1+yld/12)^(1:length(cf))))
}

MBScf2Price <- function(wam,wac,net,ptyp,speed,age,yld,delay=14) {
  svc <- wac - net
  cf <- MBScf(100,wam,wac,svc,ptyp,speed,age)
  return(1/(1+yld/12)^((delay)/30)*MBSprice(yld,cf$CF,cf$Prin))
}

MBSpacBands <- function(PSAl,PSAu,bal,wam,wac,svc,age=1) {
  cfU <- MBScf(bal,wam,wac,svc,2,PSAu,age)
  cfD <- MBScf(bal,wam,wac,svc,2,PSAl,age)
  cf <- cbind( cfU$Prin, cfD$Prin)
  return( apply(cf,1,min) )
}

PACcf <- function(PSAl,PSAu,bal,wam,wac,svc,ptyp,speed,age=1) {
  pacSch <- MBSpacBands(PSAl,PSAu,bal,wam,wac,svc,age)
  pac <- sum(pacSch)
  comp <- bal-pac
  cf <- MBScf(bal,wam,wac,svc,ptyp,speed,age)
  d <- array(0,c(wam,5))
  d[,1] <- cf$Sch + cf$PP
  d[1,2] <- pac
  d[1,3] <- comp
  for(i in 1:wam) {
    if(d[i,1] < pacSch[i] & d[i,3] > 0) {  # prepays are below sch => extending
      d[i,3] <- d[i,1]
      if(i<wam)
        d[i+1,3] <- d[i+1,3] + pacSch[i] - d[i,1]
    } else {
      if(d[i,3] > 0) {  # PAC isn't broken
        diff <- d[i,1] - pacSch[i]
        if(diff < d[i,3])
          d[i,5] <- diff
        else
          d[i,5] <- d[i,3]
        d[i,4] <- d[i,1] - d[i,5]
      } else
        d[i,4] <- d[i,1]
    }
    if(i<wam) {   # update PAC/companion balances
      d[i+1,2] <- d[i,2]-d[i,4]
      d[i+1,3] <- d[i,3]-d[i,5]
    }
  }
  d <- data.frame(d)
  names(d) <- c('prin','PAC','comp','PACcf','COMPcf')
  return(d)
}

Seqcf <- function(num=3,shr=c(bal/3, bal/3, bal/3),prin){
  n <- length(prin)
  cf <- array(0,c(n,num))
  i <- j <- 1
  while (i<=n) {
    if(shr[j]>prin[i]) {
      cf[i,j] <- prin[i]
      shr[j] <- shr[j] - prin[i]
    } else {
      if(shr[j] > 0) {
        cf[i,j] <- shr[j]
        j <- min(j+1,num)
        cf[i,j] <- prin[i] - shr[j-1]
        shr[j] <- shr[j] - cf[i,j]
        shr[j-1] <- 0
      } else {
        j <- min(j+1,num)
        cf[i,j] <- prin[i]
        shr[j] <- shr[j] - prin[i]
      }
    }
    i <- i+1
  }
  return(cf)
}
