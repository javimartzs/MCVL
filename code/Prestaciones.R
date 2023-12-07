library(esadeecpol)
library(data.table)
library(knitr)

# Importamos el resgitro de afiliaciones 
df <- readRDS('data/MCVL2019AFILIAD.rds')
view(head(df))

# Convertimos las variables de alta y baja en fechas
df[, alta_date := ymd(alta_date)]
df[, baja_date := ymd(baja_date)]

# Eliminamos las altas anteriores a 2019
df <- df[alta_date > as.Date('2019-01-01')]
# Nos quedamos con los que tienen prestacion
df <- df[TRL %in% c(751:756)]

# Recodificamos TRL
df[TRL == 751, relacion := 'Prestación desempleo (extinción contrato)']
df[TRL == 752, relacion := 'Prestación desempleo (suspensión contrato)']
df[TRL == 753, relacion := 'Subsidio desempleo >52/55 años (extinción contrato)']
df[TRL == 754, relacion := 'Subsidio desempleo >52/55 años (suspensión contrato)']
df[TRL == 755, relacion := 'Subsidio desempleo incluyendo agrario (extinción contrato)']
df[TRL == 756, relacion := 'Subsidio desempleo (suspensión contrato)']
kable(table(df$relacion))
df <- df[TRL %in% c(753, 755)]

# Nos quedamos con una lista de pid
df <- unique(df, by = "pid")
df <- df[, .(pid, relacion)]


# Importamos personal file
per <- readRDS('data/MCVL2019PERSONAL.rds')

df <- merge(df, per, by = 'pid')
kable(colnames(df))

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
