#' Tuning covid spread model using real world data sets  
#' 
#' This function uses real world data set to tune covid spread model, aiming to minimize the difference between simulated total number of cases and actutal total number of cases.
#' 
#' @param range1 muT tuning ranges
#' @param range2 sizeV tuning ranges
#' @param df real world data set of a specific country
#' @param parallel a Boolean variable, indicating whether a parallel processing is wanted, the default value is TRUE.
#' @param ... Other arguments passed on to methods.
#' 
#' @return the function returns a data frame of the best value of muT, sizeV, and the minimized difference.
#' 
#' @import tidyr parallel dplyr
#' 
#' @export

utils::globalVariables(c("dv"))
parameter_tuning = function(range1,range2, df, parallel = TRUE, ...){
  param = tidyr::expand_grid(range1, range2)
  if (parallel == TRUE){
    best = parallel::mclapply(1:nrow(param), function(i){
      set.seed(123)
      sim = TransSimu(nd = 200, muT = param$range1[i], sizeV = param$range2[i], ...)
      pred = sim$total[1:200]
      act = df$total_cases[1:200]
      dv = sum((act - pred)^2)
      dv_df = data.frame(cbind(param$range1[i], param$range2[i], dv))
      colnames(dv_df) = c("range1", "range2", "dv")
      return(dv_df)
    }, mc.cores = detectCores()) %>% bind_rows() %>% dplyr::arrange(dv) %>% head(1)
  }else{
    best = lapply(1:nrow(param), function(i){
      set.seed(123)
      sim = TransSimu(nd = 200, muT = param$range1[i], sizeV = param$range2[i], ...)
      pred = sim$total[1:200]
      act = df$total_cases[1:200]
      dv = sum((act - pred)^2)
      dv_df = data.frame(cbind(param$range1[i], param$range2[i], dv))
      colnames(dv_df) = c("range1", "range2", "dv")
      return(dv_df)
    }) %>% bind_rows() %>% dplyr::arrange(dv) %>% head(1)
  }
  return(best)
}