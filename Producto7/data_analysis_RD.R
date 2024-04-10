#------------------------------------------------------------------------------
# Project: ROAR Project
# Title: Dominican Republic - Summary statistics
# Date: November 2023
#------------------------------------------------------------------------------

install.packages("readxl")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("rlang")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("vctrs")
install.packages("stringr")
install.packages("summarytools")


library(readxl)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(vctrs)
library(summarytools)
library(lubridate)



# set working directory
setwd("C:/Users/gar14685/OneDrive - Texas Tech University/WORK/CATIE/data/DR")
getwd()

data <- read_excel("data_april7_2024_RD.xlsx")[-1,]




# DATA CLEANING
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# We will delete observation who did Not want to participate
data <- data[data$Q1 != "No", ]

# We will delete incomplete surveys
data$Progress <- as.numeric(as.character(data$Progress))
data <- data[data$Progress == 100 | is.na(data$Progress), ]
data <- data[data$Finished == "True", ]



# SUMMARY STATISTICS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Recorded date
data$RecordedDate <- as.numeric(data$RecordedDate)
data$date <- as.Date(data$RecordedDate, origin = "1899-12-30")  # Format the date as YYMMDD


         
# Consentimiento de participacion, Nombre, número de cedula, y Teléfono/WhatsApp
data <- data %>%
  rename(consentimiento = Q1,
         nombre = Q2,
         cedula = Q230,
         telefono = Q3)



# Summary of Productores encuestados
dfSummary(data[,"consentimiento"])




# Summary of ¿En cuál provincia está su finca de cacao?
# we renamed the column as "provincia" for clarity
data <- data %>%
  rename(provincia = Q4)

sorted_province_table <- sort(table(data$provincia), decreasing = TRUE)
print(sorted_province_table)




# Summary of ¿En cuál municipio está su finca de cacao?
# We create a new column named "municipio" for clarity
data <- data %>%
  tidyr::unite("municipio", Q5:Q36, sep = " ", na.rm = TRUE, remove = TRUE)
print(table(data$municipio))



# Summary of ¿En que distrito municipal está su finca de cacao?
# We create a new column named "distrito"
data <- data %>%
  tidyr::unite("distrito", Q37:Q138, sep = " ", na.rm = TRUE, remove = TRUE)
print(table(data$distrito))




# Summary of Escriba el paraje o dirección aproximada de su finca de cacao
# We create a new column named "paraje"
data <- data %>%
  rename(paraje = Q139)

dfSummary(data[,"paraje"])


sorted_paraje_table <- sort(table(data$paraje), decreasing = TRUE)
print(sorted_paraje_table)

# Calculate proportions
print(prop.table(sorted_paraje_table) * 100)


# Rename columns for latitud, longitud y altura
data <- data %>%
  rename(longitud = Q229_1,
         latitud = Q229_2,
         altura = Q229_3)

# Summary of Género del productor
# we create a new dummy variables  "genero", where Mujer =1 and Hombre = 0
dfSummary(data[,"Q140"])

data$genero <- ifelse(data$Q140 == "Mujer", 1,
                            ifelse(data$Q140 == "Hombre", 0, NA))
dfSummary(data[,"genero"])



# Summary of Edad (años)
# we convert age to numeric (non-numeric values will become NA) and saved on a new column as "edad"
data$edad <- as.numeric(as.character(data$Q141_1))
summary(data$edad)



# Summary of Estado civil
dfSummary(data[,"Q142"])
data$estado_civil <- ifelse(data$Q142 == "Casado", 1,
                            ifelse(data$Q142 == "Soltero", 2,
                                   ifelse(data$Q142 == "Unión Libre", 3, NA)))
dfSummary(data[,"estado_civil"])




# Summary of ¿Cuál es su nivel de educación escolar?
# we renamed the column as "edu"
data <- data %>%
  rename(edu = Q143)
dfSummary(data[,"edu"])



# Summary of ¿Cuántas personas viven en su casa?
# we convert it to numeric (non-numeric values will become NA) and saved on a new column as "hogar_size"
data$hogar_size <- as.numeric(as.character(data$Q144_1))
summary(data$hogar_size)



# Summary of ¿Cuántas personas dependen de sus ingresos?
# we convert it to numeric (non-numeric values will become NA) and saved on a new column as "dependientes"
data$dependientes <- as.numeric(as.character(data$Q145_1))
summary(data$dependientes)



# Summary of ¿Cuántos años tiene de experiencia produciendo cacao?
# we convert it to numeric (non-numeric values will become NA) and saved on a new column as "experiencia"
data$experiencia <- as.numeric(as.character(data$Q146_1))
summary(data$experiencia)



# Summary of Problemas con viento
# We create a dummy variable "viento," where if viento is a problem: 1 = Yes; 0 = No.
data <- data %>%
  mutate(viento = ifelse(Q147 == "Sí", 1, 0))

dfSummary(data[,"Q147"])
dfSummary(data[,"viento"])



# Summary of Seleccione el rango del área total del cacaotal en producción (tareas)
# we renamed the column as "area_rango"
data <- data %>%
  rename(area_rango = Q148)
dfSummary(data[,"area_rango"])




# Summary of Area de la Finca, each column has the exact number of tareas per category
data$tareas_1_80 <- as.numeric(as.character(data$Q149_7))
data$tareas_81_160 <- as.numeric(as.character(data$Q150_7))
data$tareas_160 <- as.numeric(str_extract(data$Q151, "\\d+"))

# We create a new column named "area_finca" to have a continuous measure of total finca area (tareas)
data <- data %>%
  tidyr::unite("area_finca_str", tareas_1_80,tareas_81_160,tareas_160, sep = " ", na.rm = TRUE, remove = TRUE)
data$area_finca <- as.numeric(as.character(data$area_finca_str))
summary(data$area_finca)
dfSummary(data[,"area_finca"])



# Summary of edad aproximada de su plantación de cacao en años
# we renamed the column as "edad_plantacion"
data$edad_plantacion <- as.numeric(as.character(data$Q152_1))
summary(data$edad_plantacion)
dfSummary(data[,"edad_plantacion"])



# Summary of ¿Cuál es el marco de plantación (planta x hileras) de su cacaotal? and Dummy variable, Si=1; No=0
data <- data %>%
  rename(`marco2x2` = Q153_1,
         `marco2.5x2` = Q153_2,
         `marco2.5x2.5` = Q153_3,
         `marco2.5x3` = Q153_4,
         `marco3x3` = Q153_5,
         `marco3x3.5` = Q153_6,
         `marco3.5x3.5` = Q153_7,
         `marco3.5x4` = Q153_8,
         `marco4x4` = Q153_9,
         `marco_otro` = Q153_10,
         `marco_notengo_nosé` = Q153_11,
         `marco_otro_texto` = Q153_10_TEXT)

data <- data %>%
  mutate(across(starts_with("marco"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("marco2x2","marco2.5x2","marco2.5x2.5","marco2.5x3","marco3x3",
                  "marco3x3.5","marco3.5x3.5","marco3.5x4","marco4x4","marco_otro",
                  "marco_notengo_nosé","marco_otro_texto")])
             
dfSummary(data[,c("marco2x2_dummy","marco2.5x2_dummy","marco2.5x2.5_dummy","marco2.5x3_dummy","marco3x3_dummy",
                  "marco3x3.5_dummy","marco3.5x3.5_dummy","marco3.5x4_dummy","marco4x4_dummy","marco_otro_dummy",
                  "marco_notengo_nosé_dummy","marco_otro_texto_dummy")])




# Summary of Si no tiene un marco de plantación definido, cuántas plantas por tarea tiene en promedio
# Display this question, If ¿Cuál es el marco de plantación (planta x hileras) de su cacaotal?: = No tengo o no lo sé
# we renamed the column as "plantas_tarea"
data$plantas_tarea <- as.numeric(as.character(data$Q154_1))
summary(data$plantas_tarea)
dfSummary(data[,"plantas_tarea"])



# Summary of ¿Cómo es la pendiente del terreno en promedio? and Dummy variables; Si=1; No=0
data <- data %>%
  rename(`pendiente_plano` = Q155_1,
         `pendiente_suaveligera` = Q155_2,
         `pendiente_media` = Q155_3,
         `pendiente_meidafuerte` = Q155_4,
         `pendiente_fuerte` = Q155_5,
         `pendiente_fuertemuyfuerte` = Q155_6,
         `pendiente_muyfuerte` = Q155_7)

data <- data %>%
  mutate(across(starts_with("pendiente"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("pendiente_plano_dummy","pendiente_suaveligera_dummy",
                  "pendiente_media_dummy","pendiente_meidafuerte_dummy",
                  "pendiente_fuerte_dummy","pendiente_fuertemuyfuerte_dummy",
                  "pendiente_muyfuerte_dummy")])


# Summary of Producción Anual (quintales)
# we renamed the column as "quintales_year"
data$quintales_year <- as.numeric(as.character(data$Q156_1))
summary(data$quintales_year)
	


# Summary of Precio de venta RD$/quintal
# we renamed the column as "precio_quintal"
data$precio_quintal <- as.numeric(as.character(data$Q156_3))
summary(data$precio_quintal)



# Summary of ¿Cómo vende el cacao? Dummy variables, Si=1; No=0
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
data <- data %>%
  rename(`venta_baba` = Q157_1,
         `venta_fermentado` = Q157_2,
         `venta_seco` = Q157_3,
         `venta_procesado` = Q157_4)

data <- data %>%
  mutate(across(starts_with("venta"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("venta_baba_dummy","venta_fermentado_dummy","venta_seco_dummy",
                  "venta_procesado_dummy")])





# Summary of ¿A quién vende el cacao? Dummy variables, Si=1; No=0
data <- data %>%
  rename(`compra_RoigAgroCacao` = Q158_1,
         `compra_Conacado` = Q158_2,
         `compra_RizekCacao` = Q158_3,
         `compra_Biocafcao` = Q158_4,
         `compra_Munne` = Q158_5,
         `compra_CortesHermanos` = Q158_6,
         `compra_Chocal` = Q158_7,
         `compra_Cafiesa` = Q158_8,
         `compra_ChocolateAntillano` = Q158_9,
         `compra_Intermediario` = Q158_10,
         `compra_Ninguno` = Q158_11,
         `compra_Otro` = Q158_12,
         `compra_Cooproagro` = Q158_13,
         `compra_otro_texto` = Q158_12_TEXT)

data <- data %>%
  mutate(across(starts_with("compra"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))


dfSummary(data[,c("compra_RoigAgroCacao_dummy", "compra_Conacado_dummy",
                  "compra_RizekCacao_dummy", "compra_Biocafcao_dummy",
                  "compra_Munne_dummy", "compra_CortesHermanos_dummy",
                  "compra_Chocal_dummy", "compra_Cafiesa_dummy",
                  "compra_ChocolateAntillano_dummy", "compra_Intermediario_dummy",
                  "compra_Ninguno_dummy", "compra_Otro_dummy",
                  "compra_Cooproagro_dummy", "compra_otro_texto_dummy")])



# Summary of ¿Cómo clasificaría su conocimiento en la producción y manejo del cacao? 
# we renamed the column as "conocimiento_cacao"
# (1) es Deficiente, (5) es Promedio, y (9) Excelente
data$conocimiento_cacao <- as.numeric(as.character(data$Q159))
summary(data$conocimiento_cacao) #continuous scale




# Summary of Seleccione los principales problemas que tiene en la plantación de cacao. Dummy variables, Si=1; No=0
data <- data %>%
  rename(`problema_mazorcanegra` = Q160_1,
         `problema_antracnosis` = Q160_2,
         `problema_buba` = Q160_3,
         `problema_rata` = Q160_4,
         `problema_manoobra` = Q160_5,
         `problema_edadplantacion` = Q160_6,
         `problema_financiamiento` = Q160_7,
         `problema_fertilidadsuelo` = Q160_8,
         `problema_costoinsumos` = Q160_9,
         `problema_excesosombra` = Q160_10,
         `problema_productividadcacao` = Q160_11,
         `problema_asistenciatecnica` = Q160_12,
         `problema_otro` = Q160_13,
         `problema_otro_texto` = Q160_13_TEXT)

data <- data %>%
  mutate(across(starts_with("problema"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))


dfSummary(data[,c("problema_mazorcanegra_dummy","problema_antracnosis_dummy","problema_buba_dummy",
                  "problema_rata_dummy","problema_manoobra_dummy","problema_edadplantacion_dummy",
                  "problema_financiamiento_dummy","problema_fertilidadsuelo_dummy","problema_costoinsumos_dummy",
                  "problema_excesosombra_dummy","problema_productividadcacao_dummy","problema_asistenciatecnica_dummy",
                  "problema_otro_dummy","problema_otro_texto_dummy")])





# Enumere los tres problemas más importantes. Variable --> Variable continua
#Seleccionando 1 para el problema más importante, 2 para el segundo más importante, y 3 para el tercer problema más importante.
data <- data %>%
  rename(`importancia_mazorcanegra` = Q161_1,
         `importancia_antracnosis` = Q161_2,
         `importancia_buba` = Q161_3,
         `importancia_rata` = Q161_4,
         `importancia_manoobra` = Q161_5,
         `importancia_edadplantacion` = Q161_6,
         `importancia_financiamiento` = Q161_7,
         `importancia_fertilidadsuelo` = Q161_8,
         `importancia_costoinsumos` = Q161_9,
         `importancia_excesosombra` = Q161_10,
         `importancia_productividadcacao` = Q161_11,
         `importancia_asistenciatecnica` = Q161_12,
         `importancia_otro` = Q161_13,
         `importancia_otro_texto` = Q161_13_TEXT)

dfSummary(data[,c("importancia_mazorcanegra","importancia_antracnosis","importancia_buba",
                  "importancia_rata","importancia_manoobra","importancia_edadplantacion",
                  "importancia_financiamiento","importancia_fertilidadsuelo","importancia_costoinsumos",
                  "importancia_excesosombra","importancia_productividadcacao","importancia_asistenciatecnica",
                  "importancia_otro","importancia_otro_texto")])






# Summary of ¿Qué tipo de material genético tiene y en qué porcentaje?
# The baseline is Plantas híbridas (semilla) out of 100% 
# For exmaple, 90% means, 90% are plantas híbridas and only 10% clones
data$material_hibrido <- as.numeric(as.character(data$Q162_1))
data$material_clon <- as.numeric(as.character(data$Q162_2))

dfSummary(data[,c("material_hibrido","material_clon")])





# Summary of ¿Conoce que tipo de plantas/clones tiene en su finca?
# We create a dummy variable "conocimiento_matgenetico" where: 
#if the producer conoce que tipo de plantas/clones tiene en su finca: 1 = Si; 0 = No.
data <- data %>%
  mutate(conocimiento_matgenetico = ifelse(Q163 == "Si", 1, 0))
dfSummary(data[,"conocimiento_matgenetico"])
dfSummary(data[,"Q163"])





# Summary of ¿Tiene sistema de riego para el cultivo de cacao?
# We create a dummy variable "riego" where, tiene riego: 1 = Si; 0 = No.
data <- data %>%
  mutate(riego = ifelse(Q166 == "Si", 1, 0))
dfSummary(data[,"riego"])
dfSummary(data[,"Q166"])






# Summary of ¿En qué condición posee la finca?
# we renamed the column as "tenencia_finca"
data <- data %>%
  rename(tenencia_finca = Q167)
dfSummary(data[,"tenencia_finca"])





# Summary of ¿Cuánto paga anualmente por tarea, en promedio?
# we renamed the column as "renta_tarea"
data$renta_tarea <- as.numeric(as.character(data$Q168_1))
summary(data$renta_tarea)





# Summary of ¿El cacaotal está combinado con otras especies, ejemplo, frutales, maderables, servicios, etc.)?
# we renamed the column as "plantacion_combinada" where; if cacaotal esta combinado con otras especia: 1 = Yes; 0 = No.
data <- data %>%
  mutate(plantacion_combinada = ifelse(Q169 == "Si", 1, 0))
dfSummary(data[,"plantacion_combinada"])






# Summary of Por favor, indique el tipo de especies que tiene:  Dummy variable, Si=1, No=0
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
data <- data %>%
  rename(`especies_maderables` = Q170_1,
         `especies_servicio` = Q170_2,
         `especies_frutales` = Q170_3,
         `especies_musaceas` = Q170_4)

data <- data %>%
  mutate(across(starts_with("especies"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("especies_maderables_dummy","especies_servicio_dummy","especies_frutales_dummy","especies_musaceas_dummy")])





# Summary of Por favor, indique la cantidad aproximada que tiene en su finca por tarea
data$maderables_tareas <- as.numeric(as.character(data$Q171_1))
data$servicios_tareas <- as.numeric(as.character(data$Q172_2))
data$frutales_tareas <- as.numeric(as.character(data$Q173_2))
data$musaceas_tareas <- as.numeric(as.character(data$Q174_2))

dfSummary(data[,c("maderables_tareas","servicios_tareas","frutales_tareas","musaceas_tareas")])

summary(data$maderables_tareas)
summary(data$servicios_tareas)
summary(data$frutales_tareas)
summary(data$musaceas_tareas)






#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# ESTRUCTURA DE COSTOS, GASTOS Y ACTIVOS EN EL CACAOTAL


# Summary of Conoce el costo de producción por cada quintal de cacao producido?
# we renamed the column as "conocimiento_costoprod" where; if conoce el costo de produccion por quintal: 1 = Si; 0 = No.
data <- data %>%
  mutate(conocimiento_costoprod = ifelse(Q178 == "Si", 1, 0))
dfSummary(data[,"conocimiento_costoprod"])





# Summary of Si lo conoce, en cuánto estima el costo de producir un quintal de cacao
# we renamed the column as "costo_quintal"
non_numeric_values <- data$Q179_1[!grepl("^\\d+$", data$Q179_1)]
unique(non_numeric_values)
# Replace commas and convert to numeric
data$Q179_1_cleaned <- gsub(",", "", data$Q179_1)
data$costo_quintal <- as.numeric(data$Q179_1_cleaned)

# We checked for any remaining non-numeric values
non_numeric_costoquintal <- data$costo_quintal[!is.na(data$costo_quintal) & !is.finite(data$costo_quintal)]
unique(non_numeric_costoquintal)

# Print summary of el costo de producir 1 quintal de cacao (RD$/QQ)
summary(data$costo_quintal)






# Summary of Por favor, indique que tipo de mano de obra utiliza para el manejo de su plantación de cacao.  Dummy variable, Si=1, No=0
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
data <- data %>%
  rename(`manoobra_temporal` = Q180_1,
         `manoobra_permanente` = Q180_2,
         `manoobra_familiar` = Q180_3)

data <- data %>%
  mutate(across(starts_with("manoobra"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("manoobra_temporal_dummy","manoobra_permanente_dummy","manoobra_familiar_dummy")])





# Summary of Cómo maneja su plantación
# we created two new dummy variables (not one, because options are not mutually exclusive), 
# "organico" and "convencional" where; if plantacion es: 1 = Yes; 0 = No.
data <- data %>%
  rename(`manejo_organico` = Q182_1,
         `manejo_convencional` = Q182_2)

data <- data %>%
  mutate(across(starts_with("manejo"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,"manejo_organico_dummy"])






# Summary of Por favor indique si utiliza alguno de los siguientes insumos. Dummy variable, Si=1, No=0.
data <- data %>%
  rename(`insumo_ferti_organico` = Q183_1,
         `insumo_ferti_quimico` = Q183_2,
         `insumo_insecti_organico` = Q183_3,
         `insumo_insecti_quimico` = Q183_4,
         `insumo_fungi_organico` = Q183_5,
         `insumo_fungi_quimico` = Q183_6,
         `insumo_herbicida` = Q183_7,
         `insumo_alambre` = Q183_8,
         `insumo_postes` = Q183_9,
         `insumo_grapas` = Q183_10,
         `insumo_sacos` = Q183_11,
         `insumo_combustible_secado` = Q183_12,
         `insumo_combustible_otro` = Q183_13,
         `insumo_otros` = Q183_14,
         `insumo_otros_texto` = Q183_14_TEXT)

data <- data %>%
  mutate(across(starts_with("insumo"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("insumo_ferti_organico_dummy","insumo_ferti_quimico_dummy",
                  "insumo_insecti_organico_dummy","insumo_insecti_quimico_dummy",
                  "insumo_fungi_organico_dummy","insumo_fungi_quimico_dummy",
                  "insumo_herbicida_dummy","insumo_alambre_dummy","insumo_postes_dummy",
                  "insumo_grapas_dummy","insumo_sacos_dummy","insumo_combustible_secado_dummy",
                  "insumo_combustible_otro_dummy","insumo_otros_dummy","insumo_otros_texto_dummy")])





# Summary of Por favor, indique las herramientas que usa en la actividad productiva.  Dummy variable, Si=1, No=0.
data <- data %>%
  rename(`herramienta_machete` = Q186_1,
         `herramienta_cuchilla` = Q186_2,
         `herramienta_podadora` = Q186_3,
         `herramienta_motosierra` = Q186_4,
         `herramienta_serrucho` = Q186_5,
         `herramienta_guillotina` = Q186_6,
         `herramienta_tanque` = Q186_7,
         `herramienta_pala` = Q186_8,
         `herramienta_rastrillo` = Q186_9,
         `herramienta_pico` = Q186_10,
         `herramienta_coas` = Q186_11,
         `herramienta_martillo` = Q186_12,
         `herramienta_otra` = Q186_13,
         `herramienta_otra_texto` = Q186_13_TEXT)

data <- data %>%
  mutate(across(starts_with("herramienta"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("herramienta_machete_dummy","herramienta_cuchilla_dummy",
                  "herramienta_podadora_dummy","herramienta_motosierra_dummy",
                  "herramienta_serrucho_dummy","herramienta_guillotina_dummy",
                  "herramienta_tanque_dummy","herramienta_pala_dummy",
                  "herramienta_rastrillo_dummy","herramienta_pico_dummy",
                  "herramienta_coas_dummy","herramienta_martillo_dummy",
                  "herramienta_otra_dummy","herramienta_otra_texto_dummy")])






#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# GASTOS FINANCIEROS

# Summary of ¿Tiene préstamos vigentes vinculados a la plantación de cacao
#(ejemplo, fomento, rehabilitación, renovación, transporte, etc.? Nota: Si tiene préstamos que no
#están relacionados al cacao, no aplican.
# we create a dummy variable "deuda_vigente_dummy" where; 1 = Si; 0 = No.
data$deuda_vigente_dummy <- ifelse(data$Q188 == "Si", 1,
                                      ifelse(data$Q188 == "No", 0, NA))
dfSummary(data[,"deuda_vigente_dummy"])





# Follow-up question
# Summary of ¿Cuánto es el monto de la deuda vinculada a la plantación de cacao?
# we renamed the column as "monto_deuda"
non_numeric_values <- data$Q189_1[!grepl("^\\d+$", data$Q189_1)]
unique(non_numeric_values)

# Replace commas and convert to numeric
data$Q189_1_cleaned <- gsub(",", "", data$Q189_1)
data$monto_deuda <- as.numeric(data$Q189_1_cleaned)

dfSummary(data[,"monto_deuda"])
summary(data$monto_deuda)





# Summary of ¿Cuál es el % interés? 
# we renamed the column as "interes_anual"
data$interes_anual <- as.numeric(as.character(data$Q190_2))
summary(data$interes_anual)




# Summary of ¿De cuánto es el plazo en años para pagar la deuda?
# we renamed the column as "plazo_deuda" en años
data$plazo_deuda <- as.numeric(as.character(data$Q191_3))
summary(data$plazo_deuda) #años





# Summary of Con qué entidad tiene el préstamo: Dummy variable: Si=1, No=0.
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
data <- data %>%
  rename(`prestamo_bagricola` = Q192_1,
         `prestamo_casaexportadora` = Q192_2,
         `prestamo_intermediario` = Q192_3,
         `prestamo_cooperativas` = Q192_4,
         `prestamo_bancosprivados` = Q192_5)

data <- data %>%
  mutate(across(starts_with("prestamo"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("prestamo_bagricola_dummy","prestamo_casaexportadora_dummy",
                  "prestamo_intermediario_dummy","prestamo_cooperativas_dummy",
                  "prestamo_bancosprivados_dummy")])






# Summary of ¿Cuál es el valor promedio por tarea de las fincas/terreno en los alrededores?
# we renamed the column as "valor_finca"
data <- data %>%
  rename(valor_finca = Q193)
dfSummary(data[,"valor_finca"])





# Summary of Indique las actividades que realiza en su finca
data <- data %>%
  rename(`actividad_cosechacacao` = Q194_1,
         `actividad_desmonte_mazorcasenfermas` = Q194_2,
         `actividad_podacacao` = Q194_3,
         `actividad_deschuponado` = Q194_4,
         `actividad_abonamiento` = Q194_5,
         `actividad_aplicac_insecti` = Q194_6,
         `actividad_aplicac_fungi` = Q194_7,
         `actividad_deshierbe` = Q194_8,
         `actividad_resiembra` = Q194_9,
         `actividad_controlratas` = Q194_10,
         `actividad_renovacion` = Q194_11,
         `actividad_rehabilitacion_plantasviejas` = Q194_12,
         `actividad_deshije_musaceas` = Q194_13,
         `actividad_deshoje_musaceas` = Q194_14,
         `actividad_cosecha_musaceas` = Q194_15,
         `actividad_cosecha_otrosfrutales` = Q194_16,
         `actividad_fertiliz_otrosfrutales` = Q194_17,
         `actividad_poda_sombra` = Q194_18,
         `actividad_poda_frutales` = Q194_19,
         `actividad_poda_maderables` = Q194_20)

data <- data %>%
  mutate(across(starts_with("actividad"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("actividad_cosechacacao_dummy","actividad_desmonte_mazorcasenfermas_dummy",
                  "actividad_podacacao_dummy","actividad_deschuponado_dummy",
                  "actividad_abonamiento_dummy","actividad_aplicac_insecti_dummy",
                  "actividad_aplicac_fungi_dummy","actividad_deshierbe_dummy",
                  "actividad_resiembra_dummy","actividad_controlratas_dummy",
                  "actividad_renovacion_dummy","actividad_rehabilitacion_plantasviejas_dummy",
                  "actividad_deshije_musaceas_dummy","actividad_deshoje_musaceas_dummy",
                  "actividad_cosecha_musaceas_dummy","actividad_cosecha_otrosfrutales_dummy",
                  "actividad_fertiliz_otrosfrutales_dummy","actividad_poda_sombra_dummy",
                  "actividad_poda_frutales_dummy","actividad_poda_maderables_dummy")])







# Summary of ¿Tiene otra actividad agropecuaria, adicional a su sistema agroforestal de cacao? Dummy variable
# we create a dummy variable "otra_actividad" where; Si=1; No=0.
data$actividad_extra_dummy <- ifelse(data$Q196 == "Si", 1,
                                  ifelse(data$Q196 == "No", 0, NA))
dfSummary(data[,"actividad_extra_dummy"])
                  
                  



# Follow-up question
# Summary of Seleccione las actividades extras que aplican:
data <- data %>%
  rename(`extra_agroturismo` = Q197_1,
         `extra_ganaderia` = Q197_2,
         `extra_avicultura` = Q197_3,
         `extra_porcicultura` = Q197_4,
         `extra_aguacate` = Q197_5,
         `extra_cafe` = Q197_6,
         `extra_vegetales` = Q197_7,
         `extra_citricos` = Q197_8,
         `extra_raicestuberculos` = Q197_9,
         `extra_arroz` = Q197_10,
         `extra_palmaaceite` = Q197_11,
         `extra_coco` = Q197_12,
         `extra_mango` = Q197_13,
         `extra_musaceas` = Q197_14,
         `extra_otros` = Q197_15,
         `extra_otros_texto` = Q197_15_TEXT)

data <- data %>%
  mutate(across(starts_with("extra"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("extra_agroturismo_dummy","extra_ganaderia_dummy","extra_avicultura_dummy",
                  "extra_porcicultura_dummy","extra_aguacate_dummy","extra_cafe_dummy",
                  "extra_vegetales_dummy","extra_citricos_dummy","extra_raicestuberculos_dummy",
                  "extra_arroz_dummy","extra_palmaaceite_dummy","extra_coco_dummy",
                  "extra_mango_dummy","extra_musaceas_dummy","extra_otros_dummy",
                  "extra_otros_texto_dummy")])





# Summary of Seleccione las que aplican:
#Fuera de la finca, ¿Realiza usted y sus dependientes en el hogar otras actividades que
#generan ingresos fuera de la agricultura? (Ejemplo, empleado, comerciante, consultorías, etc.)

# USTED
# we create a dummy variable "otrasactividades_usted" where; 1 = Yes; 0 = No.
data$otrasactividades_usted <- ifelse(data$Q198_1 == "Si\n", 1,
                                       ifelse(data$Q198_1 == "No\n", 0, NA))
dfSummary(data[,"otrasactividades_usted"])




#OTRO MIEMBRO DEL HOGAR
# we create a dummy variable "otrasactividades_otros" where; 1 = Yes; 0 = No.
data$otrasactividades_otros <- ifelse(data$Q198_2 == "Si\n", 1,
                                      ifelse(data$Q198_2 == "No\n", 0, NA))
dfSummary(data[,"otrasactividades_otros"])


dfSummary(data[,c("otrasactividades_usted", "otrasactividades_otros")])
dfSummary(data[,c("Q198_1", "Q198_2")])





# ¿Qué porcentaje del ingreso familiar proviene de cacaotal (un estimado)?
data$ingresofamilia_cacao <- as.numeric(as.character(data$Q199_11))
summary(data$ingresofamilia_cacao)





#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# CONOCIMIENTO DE LA ENFERMEDAD MONILIASIS
dfSummary(data[,c("Q200","Q201")])


# ¿Ha escuchado hablar de la moniliasis del cacao?
# we create a dummy variable "conocimiento_moniliasis" where; 1 = Yes; 0 = No.
data$conocimiento_moniliasis <- ifelse(data$Q200 == "Si", 1,
                                       ifelse(data$Q200 == "No", 0, NA))
dfSummary(data[,"conocimiento_moniliasis"])




# ¿Sabe cómo se controla?
# we create a dummy variable "conocimiento_control" where; 1 = Yes; 0 = No.
data$conocimiento_control <- ifelse(data$Q201 == "Si", 1,
                                       ifelse(data$Q201 == "No", 0, NA))
dfSummary(data[,"conocimiento_control"])




dfSummary(data[,c("conocimiento_moniliasis","conocimiento_control")])




#¿Qué piensa que podría pasar con la Producción Nacional si esta enfermedad (Moniliasis del cacao) llega al país?
data = data %>%
  mutate(llegada_moniliasis_opinion = case_when(
    Q202 == "Se mantendría igual" ~ 1,
    Q202 == "Bajaría" ~ 2,
    Q202 == "No sé" ~ 3
  ))


#¿Considera que los productores abandonarían el sector cacao si esta enfermedad (Moniliasis del cacao) llega al país?
data = data %>%
  mutate(abandonar_sectorcacao = case_when(
    Q203 == "Si" ~ 1,
    Q203 == "Solo parcialmente" ~ 2,
    Q203 == "No" ~ 3
  ))


# ¿Considera que se afectaría la calidad del cacao si esta enfermedad (Moniliasis del cacao) llega al país?
data = data %>%
  mutate(calidad_cacao = case_when(
    Q204 == "Si" ~ 1,
    Q204 == "No sé" ~ 2,
    Q204 == "No" ~ 3
  ))

dfSummary(data[,c("Q202","Q203","Q204")])
dfSummary(data[,c("llegada_moniliasis_opinion","abandonar_sectorcacao","calidad_cacao")])





#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#--------------------EL FUTURO DE SU FINCA CACAOTAL----------------------------


# ¿Planea realizar inversiones de mejora en su cacaotal?
# we create a dummy variable "mejoras_cacaotal" where; 1 = Yes; 0 = No.
data$mejoras_cacaotal_dummy <- ifelse(data$Q205 == "Si", 1,
                                       ifelse(data$Q205 == "No", 0, NA))
dfSummary(data[,"mejoras_cacaotal_dummy"])





# Summary of ¿Cuáles son sus planes de inversión? Dummy variable, Si=1, No=0
data <- data %>%
  rename(`planinversion_rehabfinca` = Q206_1,
         `planinversion_renovfinca` = Q206_2,
         `planinversion_riego` = Q206_3,
         `planinversion_agroforestal` = Q206_4,
         `planinversion_otro` = Q206_5)

data <- data %>%
  mutate(across(starts_with("planinversion"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("planinversion_rehabfinca_dummy","planinversion_renovfinca_dummy",
                  "planinversion_riego_dummy","planinversion_agroforestal_dummy",
                  "planinversion_otro_dummy")])


# ¿Estaría dispuesto a participar en la socialización y validación de los resultados de este estudio?
data <- data %>%
  rename(`futura_participacion` = Q207)

#¿Desea que los resultados de este estudio le lleguen a su correo o WhatsApp?
data <- data %>%
  rename(`enviar_resultados_estudio` = Q208)

# Favor facilitar su correo electrónico y/o WhatsApp: - Correo electrónico
data <- data %>%
  rename(`correo_electronico` = Q209_1)

# Favor facilitar su correo electrónico y/o WhatsApp: - WhatsApp
data <- data %>%
  rename(`whatsapp` = Q209_2)

                  
                  
# This is the last step to our data cleaning
data_repdominicana <- subset(data, select = -c(StartDate, EndDate,	Status,	IPAddress, RecordedDate,	ResponseId,	RecipientLastName,	
                                               RecipientFirstName,	RecipientEmail,	ExternalReference, LocationLatitude,	LocationLongitude,	
                                               DistributionChannel,	UserLanguage, Q140,	Q141_1,	Q142, Q144_1,	Q145_1,	
                                               Q146_1,	Q147, Q149_7,	Q150_7,	Q151,	Q152_1, Q154_1, Q156_1,	Q156_3, Q159, Q162_1,	Q162_2,	Q163, Q166,
                                               Q168_1, Q169, Q171_1,	Q172_2,	Q173_2,	Q174_2, Q178,	Q179_1, Q188,	Q189_1,	Q190_2,	Q191_3, Q196, Q198_1,	
                                               Q198_2,	Q199_11,	Q200,	Q201,	Q202,	Q203,	Q204,	Q205))




# Saving the file as excel
write.xlsx(data_repdominicana, file = "C:/Users/gar14685/OneDrive - Texas Tech University/WORK/CATIE/data_output/data_rd.xlsx")



