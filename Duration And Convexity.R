# Xenon's House of R. Fixed Income Duration Valuation
# Bond Price (Miller UIC)
library(foreach)
BPrice <- function(cpn, term, yield, period=2) {
  T <- period*term     #Number of coupon periods
  C <- cpn/period      #Periodic Coupon value
  Y <- yield/period    #Periodic Yield
  return( C*(1-(1+Y)^(-T))/Y+100/(1+Y)^T) # Return Bond Price
}
c(BPrice(4,10,seq(.03,.05,.01),2),BPrice(4,10,seq(.03,.05,.01))) # test 

# Modified Duration

MDur <- function(cpn, term, yield, period=2) {
  T <- period*term    #NUmber of Coupon periods
  C <- cpn/period      #Periodic Coupon value
  Y <- yield/period    #Periodic Yield
  cashflow <- c()
  for(i in 1:T) {cashflow[i] <-((i*C)/(1+Y)^i)}
  A <- sum(cashflow)   #Duration with respect to Cash flows over time
  B <- (T*100)/(1+Y)^T  # Duration with respect to NPV
  Z <- 1/(1+Y)      # Inverse percentage of yield
  return((Z * (A+B) / (BPrice(cpn,term,yield,period=2)))/2)
}  

MacDur <- function(cpn, term, yield, period=2) {
  T <- period*term    #NUmber of Coupon periods
  C <- cpn/period      #Periodic Coupon value
  Y <- yield/period    #Periodic Yield
  cashflow <- c(1:T)
  for(i in 1:length(cashflow)) cashflow[i] <-((cashflow[i]*C)/(1+Y)^cashflow[i])
  A <- sum(cashflow)   #Duration with respect to Cash flows over time
  B <- (T*100)/(1+Y)^T  # Duration with respect to NPV
    return((A+B) / (BPrice(cpn,term,yield,period=2)))
}  
 #DV01 Change in value for a 1 bp change in yield (measured per millon)

DV01 <- function(cpn, term, yield, period=2) {
  T <- period*term    #NUmber of Coupon periods
  C <- cpn/period      #Periodic Coupon value
  Y <- yield/period    #Periodic Yield
  return(MDur(cpn,term,yield,period=2)*BPrice(cpn,term,yield,period=2))
}

# Convexity Measure 

DCvx <- function(cpn, term, yield, period=2) {
  T <- period*term    #NUmber of Coupon periods
  C <- cpn/period      #Periodic Coupon value
  Y <- yield/period    #Periodic Yield
  h1 <- ((2*C)/Y^3)
  h2 <- (1-(1+Y)^-T)
  h3 <- (2*T*C)/(Y^2*(1+Y)^(T+1))
  h4 <- (T*(T+1)*(100-C/Y))/(1+Y)^(T+2)
  h5 <- BPrice(cpn,term,yield, period=2)*period^2
  return((h1*h2-h3+h4)/h5)
}

DCvx(6,20,.06)
# Table Values . Question 3 Part (d)


TableF <- function(cpn, term, yield, period=2) {
  T <- period*term    #NUmber of Coupon periods
  C <- cpn/period      #Periodic Coupon value
  Y <- yield/period    #Periodic Yield)
  X <- c(-.02,-.01,-.005,-.001,-.0001,0,.0001,.001,.005,.01,.02)
  blmberg <-data.frame(matrix(0, nrow=11, ncol=8))
  colnames(blmberg) <- c("bp change", "interest change", "yield change", "Actual change", 
                         "DV01","predicted price" ,"estimated profit", "price - actual")
  blmberg[,2] <- c(-.02,-.01,-.005,-.001,-.0001,0,.0001,.001,.005,.01,.02)
  blmberg[,3] <- blmberg[,2]+yield
  blmberg[,4] <- BPrice(cpn, term, blmberg[,3],period=2)
  blmberg[,5] <- DV01(cpn, term, yield ,period=2)*blmberg[,2]
  blmberg[,6] <- (BPrice(cpn, term, yield, period=2)
                  -MDur(cpn, term, yield, period=2)*BPrice(cpn, term, yield, period=2)*X
                  +BPrice(cpn, term, yield, period=2)/2*DCvx(cpn, term, yield, period=2)*X^2)
  
    return(blmberg)
    }
TableF(2.25,30,.024)


