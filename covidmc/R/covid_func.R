#' Simulate the spread of COVID disease among people  
#' 
#' This function gives out the distributions of certain measurements that help evaluate COVID situation. These measurements include daily new cases, active cases etc.
#' 
#' @param days observation period
#' @param nd simulation period
#' @param Rt infection rate pattern
#' @param muT the mean time an infected person will transmit the virus to another person
#' @param sizeV the dispersion parameter so that variance = mu + mu^2/size
#' @param limit the target/study population size
#' @param pp the proportion of people with immunity in the population
#' @param n0 the initial number of infectious persons
#' 
#' @return the function returns a list of daily active cases number, daily new cases number and daily total number of cases over the observation period.
#' 
#' @examples 
#' rr = c(rep(3.4,10),rep(3.1,10),rep(2.2,5),rep(1.7,4),rep(1.4,6),rep(1.2,6),rep(1.1,4),rep(1,8),rep(0.9,7),rep(0.6,10),rep(0.1,30))
#' TransSimu(days = 100, nd = 30, Rt = rr)
#' 
#' @export

TransSimu = function(days = 300, nd = 30, Rt = rr, muT = 4, sizeV = 1,limit=1000000, pp=0.001,n0=1) 
{
  kk = atrisk = rep(0,days); nn = length(kk)   
  # kk: daily new cases; atrisk: number of active cases each day; simulation period of nn days
  #tt = 0   # the cumulative total number of confirmed cases. 
  if(nd > length(Rt)) stop("The length of Rt should not be smaller than nd.")
  stoplimit = limit*(1-pp)
  
  nk = n0   # The initial number of existing infectious persons.  
  tt = n0
  tt_c = c(tt)
  # there must be a first patient to kick off the transmission process! 
  for(k in 1:nk) {
    #   
    if(tt>stoplimit)  Rt[1]=0.001
    ni = rpois(1,Rt[1])    # how many people will be infected by this existing virus carrier person.
    imuind = sample(c(0,1), 1, prob=c((1-pp),pp))
    if(imuind==1) ni=0
    tt=tt+ni
    if(ni > 0) {
      tk = rep(0,ni)
      for (i in 1:ni) {
        tk[i] = rnbinom(1,size=sizeV,mu=muT)+1  # this is the nth day on which a new case occurs
        kk[tk[i]] = kk[tk[i]] + 1
      }
      #       
      pastevent = c(rep(1,(max(tk)-1)),rep(0, (days-max(tk)+1)))
      atrisk = atrisk + pastevent   
    } 
  }  
  for(j in 2:nd) {
    nk = kk[j-1]    # this is the number of people newly infected (i.e., new cases) on (j-1)th day
    if(nk > 0) {      
      for(k in 1:nk) {
        #   
        if(tt>stoplimit)  Rt[j]=0.001
        ni = rpois(1,Rt[j])    # how many people will be infected by this existing virus carrier person.
        imuind = sample(c(0,1), 1, prob=c((1-pp),pp))
        if(imuind==1) ni=0
        tt=tt+ni
        if(ni > 0) {
          tk = rep(0,ni)
          for (i in 1:ni) {
            tk[i] = rnbinom(1,size=sizeV,mu=muT)+1+j  
            kk[tk[i]] = kk[tk[i]] + 1
          }
          #       
          pastevent = c(rep(0, (j-1) ), rep(1,(max(tk)+1-j)),rep(0, (days-max(tk))))
          atrisk = atrisk + pastevent   
        }
      }  
    }  
      
  }     # end of j loop
  tt_c = cumsum(c(tt_c, kk))
  list(riskpopu = atrisk, dailynew = kk, total=tt_c)     # output information
  
} 
