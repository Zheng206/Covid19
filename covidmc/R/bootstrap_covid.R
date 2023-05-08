#' Bootstrap in covid simulation  
#' 
#' This function helps to do bootstrap. 
#' 
#' @param n number of times to bootstrap
#' @param country country of interest
#' @param covid_df the real world covid data set
#' 
#' @return the function returns a list of daily active cases number, daily new cases number after bootstrapping.
#' 
#' @importFrom dplyr filter select
#' 
#' @export

bootstrap_run = function(n, country, covid_df){
  outMA = NULL
  newMA = NULL
  totalMA = NULL
  for (i in 1:n){
    if(country == "uk"){
      uk = covid_df %>% filter(location == "United Kingdom", date > "2020-03-01" & date < "2022-12-31") 
      uk_pop = unique(uk$population)
      rr_uk = uk$reproduction_rate
      rr_uk[which(is.na(rr_uk))] = rr_uk[2]
      runi = TransSimu(nd = 200, Rt = rr_uk, muT = 3.65, sizeV = 0.8, n0 = 24, limit = uk_pop)
    }else if(country == "us"){
      us = covid_df %>% filter(location == "United States", date > "2020-03-01" & date < "2022-12-31") 
      us_pop = unique(us$population)
      rr_us = us$reproduction_rate
      rr_us[which(is.na(rr_us))] = rr_us[4]
      runi = TransSimu(nd = 200, Rt = rr_us, muT = 5, sizeV = 1.1, n0 = 46, limit = us_pop)
    }
    this = runi$riskpopu  
    outMA = rbind(outMA, this)
    thisnew = runi$dailynew
    newMA = rbind(newMA, thisnew)
    #total = runi$total
    #totalMA = rbind(totalMA, total)
  }
  return(list(outMA, newMA))
}