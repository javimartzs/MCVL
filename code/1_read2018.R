source('code/0_master.R')
year <- 2018

#' Import personal data
#' --------------------------------------------------------
df <- fread(glue('raw/MCVL{year}PERSONAL.TXT'), header = F)
colnames(df) <- c(
    'pid',
    'birth',
    'sex',
    'nationality',
    'province_nac',
    'province_af',
    'address',
    'death',
    'country',
    'education')

# There should be only one record per ID
df[, tagpid := as.integer(.N > 1), by = .(pid)]
table(df$tagpid)
df[, tagpid := NULL]

# Ola de MCVL
df[, mcvl_wo := year]

# Save personal data
saveRDS(df, glue('data/MCVL{year}PERSONAL.rds'))


#' Import Convivientes 
#' -----------------------------------------------
df <- fread(glue('raw/MCVL{year}CONVIVIR.TXT'), header = F)
colnames(df) <- c(
    'pid',
    'birth',
    'sex',
    'birth2',
    'sex2',
    'birth3',
    'sex3',
    'birth4',
    'sex4',
    'birth5',
    'sex5',
    'birth6',
    'sex6',
    'birth7',
    'sex7',
    'birth8',
    'sex8',
    'birth9',
    'sex9',
    'birth10',
    'sex10')

# There should be only one record per ID
df[, tagpid := as.integer(.N > 1), by = .(pid)]
table(df$tagpid)
df[, tagpid := NULL]

# Arrange database
df <- df[order(pid, birth, sex)]

saveRDS(df, glue('data/MCVL{year}CONVIVIR.rds'))


#' Import division file
#' ---------------------------------------------------
df <- fread(glue('raw/MCVL{year}DIVISION.TXT'), header = F)
colnames(df) <- c(
    'pid',
    'relacionesfile', # Ficheros de relaciones laborales
    'basesfile') # Ficheros de bases de cotizacion

df[basesfile == 11, basesfile := 1]
df[basesfile == 12, basesfile := 2]
df[basesfile == 13, basesfile := 3]
df[basesfile == 21, basesfile := 4]
df[basesfile == 22, basesfile := 5]
df[basesfile == 23, basesfile := 6]
df[basesfile == 31, basesfile := 7]
df[basesfile == 32, basesfile := 8]
df[basesfile == 33, basesfile := 9]
df[basesfile == 41, basesfile := 10]
df[basesfile == 42, basesfile := 11]
df[basesfile == 43, basesfile := 12]

# There should be only one record per ID
df[, tagpid := as.integer(.N > 1), by = .(pid)]
table(df$tagpid)
df[, tagpid := NULL]

saveRDS(df, glue('data/MCVL{year}DIVISION.rds'))

#' Impor cotizacion files
#' ------------------------------------------------
setwd('raw')
files <- glue("MCVL{year}COTIZA{1:12}.TXT")
list <- lapply(files, fread, header = F)
df <- rbindlist(list)
setwd('../')
colnames(df) <- c(
    'pid',
    'firm_cc2',
    'year',
    'inc1', # Base de cotización por contingencias comunes Enero
    'inc2', # Base de cotización por contingencias comunes Febero
    'inc3', # Base de cotización por contingencias comunes Marzo
    'inc4', # Base de cotización por contingencias comunes Abril
    'inc5', # Base de cotización por contingencias comunes Mayo
    'inc6', # Base de cotización por contingencias comunes Junio
    'inc7', # Base de cotización por contingencias comunes Julio
    'inc8', # Base de cotización por contingencias comunes Agosto
    'inc9', # Base de cotización por contingencias comunes Septiembre
    'inc10', # Base de cotización por contingencias comunes Octubre
    'inc11', # Base de cotización por contingencias comunes Noviembre
    'inc12', # Base de cotización por contingencias comunes Diciembre
    'total_contribution')

df <- df[order(pid, firm_cc2, year)]
saveRDS(df, glue('data/MCVL{year}COTIZA.rds'))

# Cotización de los Autonomos
df <- fread(glue('raw/MCVL{year}COTIZA13.TXT'), header = F)
colnames(df) <- c(
    'pid',
    'firm_cc2',
    'year',
    'incaut1', # Base de cotización por cuenta propia y otros Enero
    'incauto2', # Base de cotización por cuenta propia y otros Febero
    'incauto3', # Base de cotización por cuenta propia y otros Marzo
    'incauto4', # Base de cotización por cuenta propia y otros Abril
    'incauto5', # Base de cotización por cuenta propia y otros Mayo
    'incauto6', # Base de cotización por cuenta propia y otros Junio
    'incauto7', # Base de cotización por cuenta propia y otros Julio
    'incauto8', # Base de cotización por cuenta propia y otros Agosto
    'incauto9', # Base de cotización por cuenta propia y otros Septiembre
    'incauto10', # Base de cotización por cuenta propia y otros Octubre
    'incauto11', # Base de cotización por cuenta propia y otros Noviembre
    'incauto12', # Base de cotización por cuenta propia y otros Diciembre
    'contributiontot')

df <- df[order(pid, firm_cc2, year)]
save(df, glue('data/MCVL{year}COTIZA13.TXT'))


#' Import Afiliaciones files
#' ------------------------------------------------------
setwd('raw')
files <- glue("MCVL{year}AFILIAD{1:4}.TXT")
list <- lapply(files, fread, header = F)
df <- rbindlist(list)
setwd('../')
colnames(df) <- c(
    'pid',
    'regime',
    'group',
    'contract_type',
    'partime_coef',
    'alta_date',
    'baja_date',
    'baja_reason',
    'disability',
    'firm_cc2',
    'faddress2',
    'factivity09',
    'fzise',
    'altafirst_employment',
    'TRL',
    'ETT',
    'employer_type',
    'firm_type',
    'firm_cc1',
    'faddress',
    'newcontract_date1',
    'prevcontract_type1',
    'prevpartime_coef1',
    'newcontract_date2',
    'prevcontract_type2',
    'prevpartime_coef1',
    'newcontributiongroup_date',
    'prevcontributiongroup',
    'factivity93',
    'SETA',
    'TRaut',
    'efecalta_date',
    'efecbaja_date') 
df <- df[order(pid, alta_date, baja_date, firm_cc2)]
saveRDS(df, glue('data/MCVL{year}AFILIAD.rds'))