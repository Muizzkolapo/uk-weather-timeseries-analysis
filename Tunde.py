
# Task 2 – R programming (5%)

names(lapply(Tmin, FUN = max) %>%  which.max() ) -> st
 data_min_value_year <- floor(time(Tmin[[st]])[which.max(Tmin[[st]])])
 data_min_value_month_abb <- month.abb[(time(Tmin[[st]])[which.max(Tmin[[st]])] %% 1)*12+1]
 
 names(lapply(Tmean, FUN = max) %>%  which.max() ) -> stTmean
 data_min_value_year <- floor(time(Tmean[[stTmean]])[which.max(Tmean[[stTmean]])])
 data_min_value_month_abb <- month.abb[(time(Tmean[[stTmean]])[which.max(Tmean[[stTmean]])] %% 1)*12+1]
 
 names(lapply(Tmax, FUN = max) %>%  which.max() ) -> stTmax
 data_min_value_year <- floor(time(Tmax[[stTmax]])[which.max(Tmax[[stTmax]])])
 data_min_value_month_abb <- month.abb[(time(Tmax[[stTmax]])[which.max(Tmax[[stTmax]])] %% 1)*12+1]







#-------Which district is the coldest/warmest?

 lapply(Tmean, mean) %>% which.max()
lapply(Tmean, mean) %>% which.min()



#-------Which district has the widest temperature range?
```{r}
temp_range <- function(x) {
  min(x) -> temp_min
  max(x) -> temp_max
  res<- temp_max - temp_min
  return(res)
}

```




```{r}
means <- c()
lapply(Tmean,temp_range) -> t_range
#get highest valie
unlist(lapply(t_range, FUN = max)) %>% max()->val
#return names
names(t_range)[which(t_range %in% val)] 
```






















-------
all.equal(Tmin_best_harmonics$Northern_Ireland$seas.har2,Tmin_best_harmonics$Northern_Ireland$seas.har3,
          Tmin_best_harmonics$Northern_Ireland$seas.har4,Tmin_best_harmonics$Northern_Ireland$seas.har5,
          Tmin_best_harmonics$Northern_Ireland$seas.har6)