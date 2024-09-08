# COLOCAR LA FECHA DE ESTUDIO

date0 <- as.character(230918)

# COLOCAR LA RUTA DE LA INFORMACION

rc <- "C:/Users/ERITK S. BASTOS/Desktop/Insumos/ATRASO/"

# COMIENZA A CONSTRUIR LA RUTA

r0 <- gsub(" ","", paste('IBSRM14FTB_',date0,'.TXT'))

# SE AJUSTA LA RUTA PARA PODER LEER

rf0 <- gsub("/ ","/", paste(rc,r0))

# LEER EL TXT

tod0 <- read.delim(rf0, header = FALSE, sep = "©", dec = ",")

# COLOCAR LA FECHA DEL PROXIMO CIERRE

FECHA1 <- as.Date("2023-09-30","%Y-%m-%d")

FECHA2 <- data.frame(FECHA1) 

CIERRE <- rename(FECHA2, c("CIERRE"="FECHA1"))

# CALCULAR LA ESTIMACION DE DIAS AL CIERRE 

# SEPARAR LOS DIAS, MESES Y AÑOS

da <- str_sub(date0, 1, 2)
dm <- str_sub(date0, 3, 4)
dd <- str_sub(date0, 5, 6)

# FORMAR LA FECHA CON EL FORMATO DE LA FECHA

FECHA <- paste(20,da,"-",dm,"-",dd)
FECHA <- gsub(" ","",FECHA)
FECHA <- as.Date(FECHA,"%Y-%m-%d")

# CALCULAR LOS DIAS ESTIMADOS AL CIERRE

ds <- as.numeric(FECHA1-FECHA)

# SE ESTIMA LOS DIAS VENCIDOS 

dcv <- mutate(CV, DIA_VEN_EST=DIAS_VENCIDOS+ds)
