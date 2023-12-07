source('code/0_master.R')

#' ==============================================================
#' ==============================================================
#' INDIVIDUOS - CARACTERISTICAS
#' ==============================================================
#' ==============================================================
for (j in 2013:2019) {

#' -------------------------------------------------------------------------------
#' 1) Creamos un fichero por cada MCVL, con la info de los individuos para ese año
#' -------------------------------------------------------------------------------
df <- readRDS(glue('data/MCVL{j}PERSONAL.rds'))

#' -------------------------------------------------------------------------------
#' 2) Merge personal + Convivientes. Matching by pid
#' -------------------------------------------------------------------------------
convi <- readRDS(glue('data/MCVL{j}CONVIVIR.rds'))
df <- merge(df, convi, by = c('pid', 'birth', 'sex'), all = TRUE)
rm(convi)

#' -------------------------------------------------------------------------------
#' 3) Sexo
#' -------------------------------------------------------------------------------
df[, sex := as.character(sex)]
df[sex == '1', sex := 'Hombre']
df[sex == '2', sex := 'Mujer']

#' -------------------------------------------------------------------------------
#' 4) Nacionality
#' -------------------------------------------------------------------------------
df[, nationality := as.integer(str_remove_all(nationality, 'N'))]
df[nationality == 99, nationality := NA]

df[, country := as.integer(str_remove_all(country, 'N'))]
df[country == 99, country := NA]

df[is.na(country), country := nationality]


#' -------------------------------------------------------------------------------
#' 5) Education level
#' -------------------------------------------------------------------------------
df[education %in% c(10:11), education := 10]
df[education %in% c(20:22), education := 20]
df[education %in% c(30:32), education := 30]
df[education %in% c(40:42), education := 40]
df[education %in% c(43:45), education := 50]
df[education %in% c(46:47), education := 60]
df[education %in% c(48), education := 70]
df[!education %in% c(10, 20, 30, 40, 50, 60, 70), education := NA]

df[education == 10, education := 'No sabe leer ni escribir']
df[education == 20, education := 'Titulación inferior a graduado escolar']
df[education == 30, education := 'Graduado escolar o equivalente']
df[education == 40, education := 'Bachiller o Formación Profesional 2ºgrado']
df[education == 50, education := 'Diplomado, Técnico u otra titulación media']
df[education == 60, education := 'Licenciado o Graduado Universitario']
df[education == 70, education := 'Máster, Doctorado o estudios de postgrado']

kable(table(df$education))

#' -------------------------------------------------------------------------------
#' 6) Creamos la variable 'Year' y nos quedamos con una observacion por individuo
#' (es solo un check, porque en realidad, deberiamos tener solo una obs por individuo)
#' -------------------------------------------------------------------------------
df[, year := j]
df[, tagpid := as.integer(.N > 1), by = .(pid)]
table(df$tagpid)
df <- df[tagpid == 0]
df[, tagpid := NULL]


saveRDS(df, glue('data/cohort2019/MCVL{j}PERSONALfull.rds'))
}

#' ==============================================================
#' ==============================================================
#' INDIVIDUOS - PANEL
#' ==============================================================
#' ==============================================================

#' -------------------------------------------------------------------------------
#' 7) Unimos todos los años para tener un panel pid-year
#' -------------------------------------------------------------------------------
setwd('data/cohort2019')
files <- list.files(pattern = 'full')
list <- lapply(files, readRDS)
df <- rbindlist(list)

table(df$mcvl_wo, df$year)
df[, mcvl_wo := NULL]

df[, mcvl_wo := max(year), by = pid] # Ultima mcvl en la que aparece el id
df[, mcvl_entry := min(year), by = pid] # Primera mcvl en la que aparece el id

table(df$mcvl_wo[df$year == df$mcvl_wo]) # N ids segun la ultima ola en la qe aparecen

saveRDS(df, glue('IndividualsFULL_original.rds'))

#' -------------------------------------------------------------------------------
#' 8) Depuracion de individuos (según fechas de nacimiento)
#' -------------------------------------------------------------------------------
df <- readRDS(glue('IndividualsFULL_original.rds'))

#' 8.1) Tiramos a individuos que cambien de fecha de nacimiento a lo largo de 
#' las distintas ediciones de la MCVL
df[, birth := as.character(birth)]
df[, birth := ym(birth)]
df[, multi_birth := as.integer(uniqueN(birth) > 1), by = pid]
df <- df[multi_birth != TRUE]

#' 8.2) Tiramos a individuos cuya fecha de nacimiento no este disponible
setorder(df, pid, year)
df[, birth := birth[1], by = pid]
df <- df[!is.na(birth)]
df[, multi_birth := NULL]
table(df$mcvl_wo[df$year == df$mcvl_wo])

#' -------------------------------------------------------------------------------
#' 9) Rectangularizamos el panel, si hay huecos rellenamos con la informacion disponible
#' e inmediatamente anterior
#' -------------------------------------------------------------------------------
combination <- CJ(pid = unique(df$pid), year = unique(df$year))
df <- df[combination, on = .(pid, year)]
df[is.na(birth) & is.na(sex), fillin := 1]
df[is.na(fillin), fillin := 0]

setnames(df, "mcvl_wo", "mcvl_tmp")
setorder(df, pid, year)
df[, mcvl_wo := max(mcvl_tmp, na.rm = TRUE), by = pid]
df[, mcvl_tmp := NULL]

setnames(df, "mcvl_entry", "mcvl_tmp")
setorder(df, pid, year)
df[, mcvl_entry := max(mcvl_tmp, na.rm = TRUE), by = pid]
df[, mcvl_tmp := NULL]

#' Nos quedamos solo conlos años entre la primera vez que le observo (mcvl_entry)
#' y la ultima (mcvl_wo). Los otros se han generado artificialmenete con combination
df <- df[year <= mcvl_wo]
df <- df[year >= ]


#' Rellenamos los huecos creados entre por combination entre ambos años (gente que
#' entra y sale de la muestra) con la info del año anterior
varlist <- setdiff(names(df), c("pid", "year", "mcvl_wo", "mcvl_entry", "fillin"))
# Recorrer cada variable en varlist
for (var in varlist) {
  # Reemplazar el valor de la variable por el valor de la fila anterior si _fillin es 1
  df[order(pid, year), (var) := shift(get(var), type = "lag", fill = NA), by = pid][fillin == 1]
}


#' IMPORTANTE: es posible que veamos missings en las anteriores variables si es así 
#' como venian en el dato original. Únicamente rellenamos missings de los años creados
#' "artificialmente" para rellenar huecos en el panel. E incluso, se puede dar la 
#' posibilidad de haber rellenado con missings esos años creados artificialmente si
#' el último dato de la MCVL inmediantamente anterior tambien fuese missing originalmente.

#' -------------------------------------------------------------------------------
#' 10) Nos aseguramos de que los datos del individuo que deberian ser invariables en el tiempo, 
#' sean constantes a lo largo de toda la historia del individuo, y aprovechamos para rellenar
#' posibles missings en esas variables.
#' DEJAMOS EL DATO DE LA MCVL MAS RECIENTE EN LA QUE EL DATO ESTE DISPONIBLE
#' -------------------------------------------------------------------------------

#' 10.1) Para las siguientes variables el cero es equiparable a un mising
varlist <- c('sex', 'province', 'provinceaf')
df[, negyear := -year]
for (var in varlist){
    df[is.na(get(var)) | get(var) == 0, negyear := NA]
    setorder(df, pid, negyear)
    df[, (var) := nafill(get(var), type = 'locf'), by = .(pid)]
    df[, negyear := NULL]
}

#' 10.2) Para las variables country y death, el cero tiene un valor, no es un missing
varlist <- c('country', 'death')
df[, negyear := -year]
for (var in varlist){

}

#' -------------------------------------------------------------------------------
#' 11) Creamos tantas filas como años haya desde que el individuo tiene 16 años hasta
#' que le observo por primera vez
#' -------------------------------------------------------------------------------
setorder(df, pid, year)

df[, yearbirth := as.Date(birth)]
df[, d16 := yearbirth + 16]