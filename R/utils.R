# Funções criadas pelo professor Jonatan

##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange_vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}


replace_inf <- function(x) ifelse(!is.finite(x), NA, x)
#replace_inf(c(Inf, -Inf, 1, NA))

# Apply fun dealing with vectors values that may be all NA ---------------------
fun_NA <- function(x, fun, ...){
  if(all(is.na(x))) return(NA)
  fun(x, ...)
}


replace_NA <- function(x, fill.value = 0) {
  #x <- summary_qc_j$tot
  replace(x, is.na(x), fill.value)
}


point <- scales::format_format(big.mark = ".",
                               decimal.mark = ",",
                               scientific = FALSE)

###############################################################################

# Funções extras que criei

## Calcula a porcentagem
pctg <- function(value, total, houses = 1)
{ p = round(((value/total) * 100), houses)
  return(p) }

## Calcula a porcentagem de NAs
pctg_NA <- function(value, houses = 1, not_NA = FALSE)
{ p = round(((sum(is.na(value))/length(value)) * 100), houses)
  if (not_NA == FALSE) {p_NA = p}
  if (not_NA == TRUE) {p_NA = 100 - p}
  return(p_NA) }

## Salva determinado produto automáticamente na pasta "output"
saveRDS_product <- function(number_product, data_product, prefix, id_qc)
{ if (number_product == 1) {prod_file_name <- "../output/qcID-application-PREFIX.rds"}
  if (number_product == 2) {prod_file_name <- "../output/qcID-metadata-PREFIX.rds"}
  if (number_product == 3) {prod_file_name <- "../output/qcID-summary-PREFIX.rds"}
  prod_file <- prod_file_name %>%
    str_replace("PREFIX", as.character(prefix)) %>%
    str_replace("ID", as.character(id_qc))
  saveRDS(data_product, file = prod_file) }

## Determina o número de EMAs adicionadas a cada ano
emas_by_year <- function(info, sdate) {
  info_mut <- # coluna 'year' adicionada ao conjunto 'info' original
    info %>%
    mutate(year = lubridate::year(info[[sdate]])) %>%
    select(site, state, name, year, everything())
  syear <- # ano inicial do conjunto de emas
    min(info_mut$year)
  eyear <- # ano final do conjunto de emas
    max(info_mut$year)
  en <- # número de emas por ano sem a adição de anos com zero emas
    info_mut %>%
    group_by(year) %>%
    summarise(emas_number = length(site)) %>%
    ungroup()
  seq_year <- # sequência de anos baseadas no ano inicial e final
    syear:eyear
  missing_year <- # anos com zero emas adicionadas
    seq_year[which((seq_year %in% en[['year']]) == FALSE)]
  dt_my <- # data table com os anos onde nenhuma ema foi adicionada
    data.table::data.table(year = missing_year, emas_number = 0)
  bind_en_dt <- rbind(en, dt_my)
  eby <- # organizando a ordem dos anos
    dplyr::arrange(bind_en_dt, year)
  return(eby)
}

# Função para a filtragem da variável tdavg que será usada no qc3c
qc3c_filter_td <- 
  function(data, var_inst, var_avg, var_min, var_max, save_repository) {
    d <- data %>% select(site, date, c(var_inst, var_avg, var_min, var_max))
    name_3a <- paste0(var_min, '_qc', '3a')
    name_3b <- paste0(var_inst, '_qc', '3b')
    t3a <- qc_3a(
      data = data, var_min = var_min, var_max = var_max) %>%
      select(name_3a)
    t3b <- qc_3b(
      data = data, variable = var_inst, var_min = var_min, var_max = var_max) %>%
      select(name_3b)
    d_avg <- d %>% select(var_avg)
    avg_filter <- paste0(var_avg,'_','filter')
    names(d_avg) [1] <- avg_filter
    p3a <- which(t3a[[name_3a]] == 1)
    p3b <- which(t3b[[name_3b]] == 1)
    d_avg[p3a,] <- NA
    d_avg[p3b,] <- NA
    dt <- data.table::data.table(d,t3a,t3b,d_avg) %>%
      select(site, date,
        c(var_inst, var_min, var_max, var_avg, avg_filter, name_3a, name_3b))
    saveRDS(object = dt, file = save_repository)
    return(dt)}