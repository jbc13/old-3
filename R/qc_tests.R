
# [Funções para a aplicação dos QCs]

################################################################################

## (Testes da amplitude de variação)

# data = var_data
# variable = "tavg"

qc_1a <- function(data, variable, exmin, exmax) {
  library(dplyr)
  qc <- '1a'
  d <- data %>% select(c('site', 'date', variable))
  lgcl <- (d[[variable]] < exmin | d[[variable]] > exmax)
  suspect <- ifelse(lgcl == TRUE,1,0)
  test <- d %>% mutate(qc, suspect)
  n <- which(names(test) == 'suspect')
  name <- paste0(variable, '_qc', qc)
  names(test) [n] <- name
  # p <- which(is.na(test[,name]))
  # test[p,name] <- 0
  return(test) }

qc_1b <- function(data, variable, tlow, thigh) {
  library(dplyr)
  qc <- '1b'
  d <- data %>% select(c('site', 'date', variable, tlow , thigh))
  lgcl <- (d[[variable]] > d[[thigh]] | d[[variable]] < d[[tlow]])
  suspect <- ifelse(lgcl == TRUE,1,0)
  test <- d %>% mutate(qc, suspect)
  n <- which(names(test) == 'suspect')
  name <- paste0(variable, '_qc', qc)
  names(test) [n] <- name
  return(test) }

################################################################################

## (Teste de persistência temporal: dados repetidos)

qc_2 <- function(data, variable) {
  qc <- '2'
  d <- data %>% select(c('site', 'date', variable))
  test <-
    d %>%
    group_by(site) %>%
    mutate(
      qc,
      suspect =
        as.integer(
          crv_detect(
            x = tavg,
            thresh = -99,
            min.steps = 2) ) ) %>%
    ungroup()
  n <- which(names(test) == 'suspect')
  name <- paste0(variable, '_qc', qc)
  names(test) [n] <- name
  return(test) }

################################################################################

## (Testes de Consistência Interna)

qc_3a <- function(data, var_min, var_max) {
  qc <- '3a'
  d <- data %>% dplyr::select(c('site', 'date', var_min, var_max))
  lgcl <- d[[var_min]] >= d[[var_max]]
  suspect <- ifelse(lgcl == TRUE,1,0)
  test <- d %>% dplyr::mutate(qc, suspect)
  n <- which(names(test) == 'suspect')
  name <- paste0(var_min, '_qc', qc)
  names(test) [n] <- name
  return(test) }

qc_3b <- function(data, variable, var_min, var_max) {
  library(dplyr)
  qc <- '3b'
  d <- data %>% dplyr::select(c('site', 'date', variable, var_min, var_max))
  lgcl <- d[[variable]] < d[[var_min]] | d[[variable]] > d[[var_max]]
  suspect <- ifelse(lgcl == TRUE,1,0)
  test <- d %>% dplyr::mutate(qc, suspect)
  n <- which(names(test) == 'suspect')
  name <- paste0(variable, '_qc', qc)
  names(test) [n] <- name
  return(test) }

qc_3c <- function(data, variable, var_td) {
  qc <- '3c'
  d <- data %>% dplyr::select(c('site', 'date', variable, var_td))
  lgcl <- d[[variable]] < d[[var_td]]
  suspect <- ifelse(lgcl == TRUE,1,0)
  test <- d %>% dplyr::mutate(qc, suspect)
  n <- which(names(test) == 'suspect')
  name <- paste0(variable, '_qc', qc)
  names(test) [n] <- name
  return(test) }

qc_3d <- function(data, variable, var_min, var_max) {
  qc <- '3d'
  d <- data %>%
    mutate(date_D = as.Date(date)) %>%
    select(site, date_D, everything())
  d_inf <- d %>%
    group_by(site, date_D) %>%
    summarise(
      tinst = mean(tinst, na.rm = TRUE),
      tmin = min(tmin, na.rm = TRUE),
      tavg = mean(tavg, na.rm = TRUE),
      tmax = max(tmax, na.rm = TRUE),
      tdinst = mean(tdinst, na.rm = TRUE),
      tdmin = min(tdmin, na.rm = TRUE),
      tdavg = mean(tdavg, na.rm = TRUE),
      tdmax = max(tdmax, na.rm = TRUE) )
  d_NA <- d_inf
  n_daily <- which(names(d_NA) == 'date_D')
  names(d_NA) [n_daily] <- 'date'
  n_inf_tinst <- which(is.nan(d_NA$tinst) == TRUE)
  n_inf_tmin <- which(is.infinite(d_NA$tmin) == TRUE)
  n_inf_tavg <- which(is.nan(d_NA$tavg) == TRUE)
  n_inf_tmax <- which(is.infinite(d_NA$tmax) == TRUE)
  n_inf_tdinst <- which(is.nan(d_NA$tdinst) == TRUE)
  n_inf_tdmin <- which(is.infinite(d_NA$tdmin) == TRUE)
  n_inf_tdavg <- which(is.nan(d_NA$tdavg) == TRUE)
  n_inf_tdmax <- which(is.infinite(d_NA$tdmax) == TRUE)
  d_NA[n_inf_tinst, 'tinst'] <- NA
  d_NA[n_inf_tmin, 'tmin'] <- NA
  d_NA[n_inf_tavg, 'tavg'] <- NA
  d_NA[n_inf_tmax, 'tmax'] <- NA
  d_NA[n_inf_tdinst, 'tdinst'] <- NA
  d_NA[n_inf_tdmin, 'tdmin'] <- NA
  d_NA[n_inf_tdavg, 'tdavg'] <- NA
  d_NA[n_inf_tdmax, 'tdmax'] <- NA
  d_sel <- d_NA %>%
    dplyr::select(c('site', 'date', var_min, variable, var_max))
  if (variable == 'tinst' & var_min == 'tmin' & var_max == 'tmax')
    {test <- d_sel %>%
      group_by(site) %>%
      mutate(
        lgcl = tinst > tmax | tinst < tmin,
        lgcl = replace(lgcl, is.na(lgcl), FALSE),
        qc,
        suspect = ifelse(lgcl == TRUE, 1, 0) ) %>%
      select(-lgcl) %>%
      ungroup()}
  if (variable == 'tavg' & var_min == 'tmin' & var_max == 'tmax')
    {test <- d_sel %>%
      group_by(site) %>%
      mutate(
        lgcl = tavg > tmax | tavg < tmin,
        lgcl = replace(lgcl, is.na(lgcl), FALSE),
        qc,
        suspect = ifelse(lgcl == TRUE, 1, 0) ) %>%
      select(-lgcl) %>%
      ungroup()}
  if (variable == 'tdinst' & var_min == 'tdmin' & var_max == 'tdmax')
    {test <- d_sel %>%
      group_by(site) %>%
      mutate(
        lgcl = tdinst > tdmax | tdinst < tdmin,
        lgcl = replace(lgcl, is.na(lgcl), FALSE),
        qc,
        suspect = ifelse(lgcl == TRUE, 1, 0) ) %>%
      select(-lgcl) %>%
      ungroup()}
  if (variable == 'tdavg' & var_min == 'tdmin' & var_max == 'tdmax')
    {test <- d_sel %>%
      group_by(site) %>%
      mutate(
        lgcl = tdavg > tdmax | tdavg < tdmin,
        lgcl = replace(lgcl, is.na(lgcl), FALSE),
        qc,
        suspect = ifelse(lgcl == TRUE, 1, 0) ) %>%
      select(-lgcl) %>%
      ungroup()}
  n <- which(names(test) == 'suspect')
  name <- paste0(variable, '_qc', qc)
  names(test) [n] <- name
  return(test) }

qc_3e <- function(data, var_max, var_min) {
  qc <- '3e'
  d <- data %>%
    mutate(daily_date = as.Date(date)) %>%
    select(site, daily_date, everything())
  d_inf <- d %>%
    group_by(site, daily_date) %>%
    summarise(
      tmin = min(tmin, na.rm = TRUE),
      tmax = max(tmax, na.rm = TRUE),
      tdmin = min(tdmin, na.rm = TRUE),
      tdmax = max(tdmax, na.rm = TRUE) )
  d_NA <- d_inf
  n_daily <- which(names(d_NA) == 'daily_date')
  names(d_NA) [n_daily] <- 'date'
  n_inf_tmin <- which(is.infinite(d_NA$tmin) == TRUE)
  n_inf_tmax <- which(is.infinite(d_NA$tmax) == TRUE)
  n_inf_tdmin <- which(is.infinite(d_NA$tdmin) == TRUE)
  n_inf_tdmax <- which(is.infinite(d_NA$tdmax) == TRUE)
  d_NA[n_inf_tmin, 'tmin'] <- NA
  d_NA[n_inf_tmax, 'tmax'] <- NA
  d_NA[n_inf_tdmin, 'tdmin'] <- NA
  d_NA[n_inf_tdmax, 'tdmax'] <- NA
  d_NA_sel <- d_NA %>%
    dplyr::select(c('site', 'date', var_min, var_max))
  if (var_min == 'tmin' & var_max == 'tmax')
    {d_lag <- d_NA_sel %>%
      group_by(site) %>%
      mutate(
        tmin_lag = dplyr::lag(tmin) ) %>%
      select(-tmin) %>%
      ungroup()}
  if (var_min == 'tdmin' & var_max == 'tdmax')
    {d_lag <- d_NA_sel %>%
      group_by(site) %>%
      mutate(
        tdmin_lag = dplyr::lag(tdmin) ) %>%
      select(-tdmin) %>%
      ungroup()}
  if (var_min == 'tmin' & var_max == 'tmax')
    {test <- d_lag %>%
      group_by(site) %>%
      mutate(
        lgcl = tmax < tmin_lag,
        lgcl = replace(lgcl, is.na(lgcl), FALSE),
        qc,
        suspect = ifelse(lgcl == TRUE, 1, 0) ) %>%
      select(-lgcl)}
  if (var_min == 'tdmin' & var_max == 'tdmax')
    {test <- d_lag %>%
      group_by(site) %>%
      mutate(
        lgcl = tdmax < tdmin_lag,
        lgcl = replace(lgcl, is.na(lgcl), FALSE),
        qc,
        suspect = ifelse(lgcl == TRUE, 1, 0) ) %>%
      select(-lgcl)}
  n <- which(names(test) == 'suspect')
  name <- paste0(var_max, '_qc', qc)
  names(test) [n] <- name
  return(test) }

qc_3f <- function(data, var_min, var_max) {
  qc <- '3f'
  d <- data %>%
    mutate(daily_date = as.Date(date)) %>%
    select(site, daily_date, everything())
  d_inf <- d %>%
    group_by(site, daily_date) %>%
    summarise(
      tmin = min(tmin, na.rm = TRUE),
      tmax = max(tmax, na.rm = TRUE),
      tdmin = min(tdmin, na.rm = TRUE),
      tdmax = max(tdmax, na.rm = TRUE) )
  d_NA <- d_inf
  n_daily <- which(names(d_NA) == 'daily_date')
  names(d_NA) [n_daily] <- 'date'
  n_inf_tmin <- which(is.infinite(d_NA$tmin) == TRUE)
  n_inf_tmax <- which(is.infinite(d_NA$tmax) == TRUE)
  n_inf_tdmin <- which(is.infinite(d_NA$tdmin) == TRUE)
  n_inf_tdmax <- which(is.infinite(d_NA$tdmax) == TRUE)
  d_NA[n_inf_tmin, 'tmin'] <- NA
  d_NA[n_inf_tmax, 'tmax'] <- NA
  d_NA[n_inf_tdmin, 'tdmin'] <- NA
  d_NA[n_inf_tdmax, 'tdmax'] <- NA
  d_NA_sel <- d_NA %>%
    dplyr::select(c('site', 'date', var_min, var_max))
  if (var_min == 'tmin' & var_max == 'tmax')
    {d_lag <- d_NA_sel %>%
      group_by(site) %>%
      mutate(
        tmax_lag = dplyr::lag(tmax) ) %>%
      select(-tmax) %>%
      ungroup()}
  if (var_min == 'tdmin' & var_max == 'tdmax')
    {d_lag <- d_NA_sel %>%
      group_by(site) %>%
      mutate(
        tdmax_lag = dplyr::lag(tdmax) ) %>%
      select(-tdmax) %>%
      ungroup()}
  if (var_min == 'tmin' & var_max == 'tmax')
    {test <- d_lag %>%
      group_by(site) %>%
      mutate(
        lgcl = tmin >= tmax_lag,
        lgcl = replace(lgcl, is.na(lgcl), FALSE),
        qc,
        suspect = ifelse(lgcl == TRUE, 1, 0) ) %>%
      select(-lgcl)}
  if (var_min == 'tdmin' & var_max == 'tdmax')
    {test <- d_lag %>%
      group_by(site) %>%
      mutate(
        lgcl = tdmin >= tdmax_lag,
        lgcl = replace(lgcl, is.na(lgcl), FALSE),
        qc,
        suspect = ifelse(lgcl == TRUE, 1, 0) ) %>%
      select(-lgcl)}
  n <- which(names(test) == 'suspect')
  name <- paste0(var_min, '_qc', qc)
  names(test) [n] <- name
  return(test) }

################################################################################

## (Testes de Consistência Temporal)

qc_4a <- function(data, variable, tol1, tol2, tol3, tol6, tol12) {
  qc <- '4a'
  d <- data %>% select(site, date, variable)
  test <-
    d %>%
    group_by(site) %>%
    mutate(
      tavg_lag1 = dplyr::lag(tavg, n = 1),
      tavg_lag2 = dplyr::lag(tavg, n = 2),
      tavg_lag3 = dplyr::lag(tavg, n = 3),
      tavg_lag6 = dplyr::lag(tavg, n = 6),
      tavg_lag12 = dplyr::lag(tavg, n = 12)
    ) %>%
    mutate(
      qc,
      suspect_1 = as.integer(abs(tavg - tavg_lag1) > tol1),
      suspect_2 = as.integer(abs(tavg - tavg_lag2) > tol2),
      suspect_3 = as.integer(abs(tavg - tavg_lag3) > tol3),
      suspect_6 = as.integer(abs(tavg - tavg_lag6) > tol6),
      suspect_12 = as.integer(abs(tavg - tavg_lag12 > tol12))
    ) %>%
    ungroup()
  n1 <- which(names(test) == 'suspect_1')
  name1 <- paste0(variable, '_qc', qc, '_1h')
  names(test) [n1] <- name1
  n2 <- which(names(test) == 'suspect_2')
  name2 <- paste0(variable, '_qc', qc, '_2h')
  names(test) [n2] <- name2
  n3 <- which(names(test) == 'suspect_3')
  name3 <- paste0(variable, '_qc', qc, '_3h')
  names(test) [n3] <- name3
  n6 <- which(names(test) == 'suspect_6')
  name6 <- paste0(variable, '_qc', qc, '_6h')
  names(test) [n6] <- name6
  n12 <- which(names(test) == 'suspect_12')
  name12 <- paste0(variable, '_qc', qc, '_12h')
  names(test) [n12] <- name12
  return(test) }











