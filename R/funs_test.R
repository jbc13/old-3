
# [Funções em teste, não usadas oficialmente ainda]

empty.frame <- function(values, qc_number)
{ dt <- data.table::data.table(values['site'], qc = qc_number, tot = 0, perc = 0)
  return(dt)}

stardata <- function(dframe)
{ library(dplyr)
  t1 <- c(-999,diff(dframe[['suspect']]))
  t2 <- c(diff(dframe[['suspect']]), -999)
  df_an <- dframe %>% mutate(t1, t2)
  ds1 <- df_an %>% filter(t1 == 1 & t2 == -1)
  s1 <- ds1[['date']]
  ds2 <- df_an %>% filter(t1 == 1 & t2 == 0)
  s2 <- ds2[['date']]
  start <- sort(c(s1, s2))
  return(start) }

endata <- function(dframe)
{ library(dplyr)
  t1 <- c(-999,diff(dframe[['suspect']]))
  t2 <- c(diff(dframe[['suspect']]), -999)
  df_an <- dframe %>% mutate(t1, t2)
  de1 <- df_an %>% filter(t1 == 1 & t2 == -1)
  e1 <- de1[['date']]
  de2 <- df_an %>% filter(t1 == 0 & t2 == -1)
  e2 <- de2[['date']]
  end <- sort(c(e1, e2))
  return(end) }

range.test.1 <- function(dates,variable,exmin,exmax)
{ d <- dates[,c('site','date', variable)]
  lgcl <- (d[[variable]] > exmin & d[[variable]] < exmax)
  qc <- 1
  suspect <- ifelse(lgcl == FALSE | is.na(lgcl),1,0)
  rt1 <- data.frame(d,qc,suspect)
  return(rt1) }

range.test.2 <- function(dates, abs_dates, variable, na_equal_nopass = FALSE, return = 'grouped')
{ library(dplyr)
  abs_all <- abs.by.site(abs_dates)
  df_all <- full_join(dates, abs_all, by = 'site')
  if (variable == 'tair')
  {df_var = df_all[c('site','date','tair','abs_tair_min','abs_tair_max')]
  var_min = 'abs_tair_min'
  var_max = 'abs_tair_max'}
  if (variable == 'prec')
  {df_var = df_all[c('site','date','prec','abs_prec_min','abs_prec_max')]
  var_min = 'abs_prec_min'
  var_max = 'abs_prec_max'}
  if (variable == 'p')
  {df_var = df_all[c('site','date','p','abs_p_min','abs_p_max')]
  var_min = 'abs_p_min'
  var_max = 'abs_p_max'}
#  rt01 = c(T,T,T,T,T,T,T,T,T,NA,NA,NA,NA,T,T,T,T,T,F,T,T,F,T,T,T,T,T,F)
#  rt01
#  table(rt01 == -1)
  rt01 <- df_var[[variable]] >= df_var[[var_min]] & df_var[[variable]] <= df_var[[var_max]]
  rt01[is.na(rt01)] <- -1
  df_sdrt01 <- data.frame(df_all['site'],df_all['date'],range_test_1 = rt01)
  rt <- df_sdrt01 %>%
    group_by(site) %>%
    summarise(
      pass = sum(rt01 == 1),
      not_pass = sum(rt01 == 0),
      NA_val = sum(rt01 == -1))
  return(rt) }

#######################################

# # [r product3]
# 
# var1_summary_qc <- if (all(var1_meta_qc['n'] == 0))
#   {var1_meta_qc %>% 
#     select(c('site','qc','n')) %>% 
#     group_by(site) %>% 
#     summarise(
#       qc = ID,
#       tot = sum(n),
#       perc = pctg(tot,n()))} else
#       
#         {total_size <- var1_data_qc %>%  
#           group_by(site) %>% 
#           summarise(size = n())
#         x <- var1_meta_qc %>%
#           group_by(site) %>%
#           summarise(
#             qc = first(qc),
#             tot = sum(n)) %>% 
#           inner_join(total_size, by = 'site')
#         s_qc <- x %>% mutate(perc = pctg(tot,size, 5), size = NULL)
#         }
# 
# var1_summary_qc
