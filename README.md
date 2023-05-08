# COVID-19 Spread Simulation

This study is based on Gang Xie's paper 'A novel Monte carlo simulation procedure for modelling COVID-19 spread over time' (2020), aiming to develop a COVID-19 spread dynamics model that can be used as a decision making tool for disease control.The proposed method is characterized by a Monte Carlo Simulation model, which captured infection rate and population changes, and treated each individual in a population as a random point. 

## Set Up Section

1.  install the package from GitHub and import the package:

```{r}
devtools::install_github('Zheng206/Covid19/covidmc')
library(covidmc)
data(covid_df)
```

2.  read in simulation results for the purpose of illustration:

```{r}
ukfile = "https://raw.githubusercontent.com/Zheng206/Covid19/main/data/uk_tune.csv"
usfile = "https://raw.githubusercontent.com/Zheng206/Covid19/main/data/us_new.csv"
timefile = "https://raw.githubusercontent.com/Zheng206/Covid19/main/data/time_statistics.csv"
uk_boot = read_csv(url(ukfile))
us_boot = read_csv(url(usfile))
statistics = read_csv(url(timefile))
```

3.  R CMD Check

```{r}
devtools::check(pkg = "covidmc")
```

## Main functions

1. Tuning paramaters for covid spread model

```{r}
parameter_tunning(range1,range2, df, parallel = TRUE, Rt = rr_uk, n0 = n0_uk, limit = uk_pop)
```

2. Simulation model

```{r}
sim_uk = TransSimu(nd = 200, Rt = rr_uk, muT = best_uk$range1, sizeV = best_uk$range2, n0 = n0_uk, limit = uk_pop)
```

3. Bootstrap

```{r}
uk_result = bootstrap_run(100, country, covid_df)
```