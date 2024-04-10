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
library(conflicted)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(vctrs)
library(summarytools)
library(lubridate)



# set working directory
setwd("C:/Users/gar14685/OneDrive - Texas Tech University/WORK/CATIE/data/PR")
getwd()

data <- read_excel("data_apil7_2024_PR.xlsx")[-1,]


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
         telefono = Q3)



# Summary of Productores encuestados
dfSummary(data[,"consentimiento"])




# Summary of ¿En cuál municipio está su finca de cacao?
# we renamed the column as "municipio" for clarity
data <- data %>%
  rename(municipio = Q4)

print(sort(table(data$municipio), decreasing = TRUE))




# Summary of ¿En que barrio está su finca de cacao?
# We create a new column named "barrio" for clarity
data <- data %>%
  tidyr::unite("barrio", Q5:Q82, sep = " ", na.rm = TRUE, remove = TRUE)
print(table(data$barrio))






# Rename columns for Altitud (pies sobre el nivel del mar)
data <- data %>%
  rename(altura = Q83_3)




# Summary of Género del productor
# we create a new dummy variables  "genero", where Mujer =1 and Hombre = 0
dfSummary(data[,"Q84"])

data$genero <- ifelse(data$Q84 == "Mujer", 1,
                            ifelse(data$Q84 == "Hombre", 0, NA))
dfSummary(data[,"genero"])



# Summary of Edad (años)
# we convert age to numeric (non-numeric values will become NA) and saved on a new column as "edad"
data$edad <- as.numeric(as.character(data$Q85_1))
summary(data$edad)



# Summary of Estado civil
dfSummary(data[,"Q86"])
data$estado_civil <- ifelse(data$Q86 == "Casado", 1,
                            ifelse(data$Q86 == "Soltero", 2,
                                   ifelse(data$Q86 == "Unión Libre", 3, NA)))
dfSummary(data[,"estado_civil"])




# Summary of ¿Cuál es su nivel de educación escolar?
# we renamed the column as "edu"
data <- data %>%
  rename(edu = Q87)
dfSummary(data[,"edu"])



# Summary of ¿Cuántas personas viven en su casa?
# we convert it to numeric (non-numeric values will become NA) and saved on a new column as "hogar_size"
data$hogar_size <- as.numeric(as.character(data$Q88_1))
summary(data$hogar_size)



# Summary of ¿Cuántas personas dependen de sus ingresos?
# we convert it to numeric (non-numeric values will become NA) and saved on a new column as "dependientes"
data$dependientes <- as.numeric(as.character(data$Q89_1))
summary(data$dependientes)



# Summary of ¿Cuántos años tiene de experiencia produciendo cacao?
# we convert it to numeric (non-numeric values will become NA) and saved on a new column as "experiencia"
data$experiencia <- as.numeric(as.character(data$Q90_1))
summary(data$experiencia)




# Summary of Cercanía del cacaotal:
# we create a new  variables  "cercania_cacaotal", 
data <- data %>%
  rename(cercania_cacaotal = Q91)
dfSummary(data[,"cercania_cacaotal"])






# Summary of Problemas con viento
# We create a dummy variable "viento," where if viento is a problem: 1 = Yes; 0 = No.
data <- data %>%
  mutate(viento = ifelse(Q92 == "Sí", 1, 0))

dfSummary(data[,"Q92"])
dfSummary(data[,"viento"])



# Summary of Seleccione el rango del área total del cacaotal en producción (cuerdas)
# we renamed the column as "area_rango"
data <- data %>%
  rename(area_rango = Q93)
dfSummary(data[,"area_rango"])




# Summary of Area de la Finca, each column has the exact number of cuerdas per category
data$cuerdas_1_80 <- as.numeric(as.character(data$Q94_7))
data$cuerdas_81_160 <- as.numeric(as.character(data$Q95_7))
data$cuerdas_160 <- as.numeric(str_extract(data$Q96_13, "\\d+"))

# We create a new column named "area_finca" to have a continuous measure of total finca area (tareas)
data <- data %>%
  tidyr::unite("area_finca_str", cuerdas_1_80,cuerdas_81_160,cuerdas_160, sep = " ", na.rm = TRUE, remove = TRUE)
data$area_finca <- as.numeric(as.character(data$area_finca_str))
summary(data$area_finca)
dfSummary(data[,"area_finca"])



# Summary of edad aproximada de su plantación de cacao en años
# we renamed the column as "edad_plantacion"
data$edad_plantacion <- as.numeric(as.character(data$Q97_1))
summary(data$edad_plantacion)
dfSummary(data[,"edad_plantacion"])



# Summary of ¿Cuál es el marco de plantación (planta x hileras) de su cacaotal en pies? and Dummy variable, Si=1; No=0
data <- data %>%
  rename(`marco6x6` = Q98_1,
         `marco7x7` = Q98_2,
         `marco8x7` = Q98_3,
         `marco8x8` = Q98_4,
         `marco8x10` = Q98_5,
         `marco10x10` = Q98_6,
         `marco10x12` = Q98_7,
         `marco12x12` = Q98_8,
         `marco_otro` = Q98_10,
         `marco_notengo_nosé` = Q98_12,
         `marco_otro_texto` = Q98_10_TEXT)

data <- data %>%
  mutate(across(starts_with("marco"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("marco6x6_dummy","marco7x7_dummy","marco8x7_dummy","marco8x8_dummy","marco8x10_dummy",
                  "marco10x10_dummy","marco10x12_dummy","marco12x12_dummy","marco_otro_dummy",
                  "marco_notengo_nosé_dummy","marco_otro_texto_dummy")])




# Summary of Si no tiene un marco de plantación definido, cuántas plantas por cuerda tiene en promedio: - Plantas por cuerda
# Display this question, If ¿Cuál es el marco de plantación (planta x hileras) de su cacaotal en pies?: - Selected Choice - No tengo o no lo sé
# we renamed the column as "plantas_tarea"
data$plantas_porcuerda <- as.numeric(as.character(data$Q99_1))
summary(data$plantas_porcuerda)
dfSummary(data[,"plantas_porcuerda"])



# Summary of ¿Cómo es la pendiente del terreno en promedio? and Dummy variables; Si=1; No=0
data <- data %>%
  rename(`pendiente_plano` = Q100_1,
         `pendiente_suaveligera` = Q100_2,
         `pendiente_media` = Q100_3,
         `pendiente_meidafuerte` = Q100_4,
         `pendiente_fuerte` = Q100_5,
         `pendiente_fuertemuyfuerte` = Q100_6,
         `pendiente_muyfuerte` = Q100_7)

data <- data %>%
  mutate(across(starts_with("pendiente"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("pendiente_plano_dummy","pendiente_suaveligera_dummy",
                  "pendiente_media_dummy","pendiente_meidafuerte_dummy",
                  "pendiente_fuerte_dummy","pendiente_fuertemuyfuerte_dummy",
                  "pendiente_muyfuerte_dummy")])





# Summary of Por favor, indíquenos su producción de cacao (ingresar cantidad en números) y seleccione la unidad de medida - Producción Anual
data <- data %>%
  rename(`produccion_unit_anual` = Q101_1,
         `produccion_anual` = Q101_1_TEXT,
         `precio_unit_venta` = Q102_1,
         `precio_venta` = Q102_1_TEXT)

dfSummary(data[,c("produccion_unit_anual","produccion_anual",
                  "precio_unit_venta","precio_venta")])



# Summary of ¿Cómo vende el cacao? Dummy variables, Si=1; No=0
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
data <- data %>%
  rename(`venta_baba` = Q103_1,
         `venta_fermentado` = Q103_2,
         `venta_seco` = Q103_3,
         `venta_procesado` = Q103_4,
         `venta_mazorca` = Q103_5)

data <- data %>%
  mutate(across(starts_with("venta"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("venta_baba_dummy","venta_fermentado_dummy","venta_seco_dummy",
                  "venta_procesado_dummy","venta_mazorca_dummy")])





# Summary of ¿A quién vende el cacao? Dummy variables, Si=1; No=0
data <- data %>%
  rename(`compra_JeanMarie` = Q104_1,
         `compra_HaciendaChocolat` = Q104_2,
         `compra_SemilaCacao360` = Q104_3,
         `compra_Montadero` = Q104_4,
         `compra_CortesHermanos` = Q104_5,
         `compra_PuertoRicoBeantoBar` = Q104_6,
         `compra_BajariChocolate` = Q104_7,
         `compra_LoizaDark` = Q104_14,
         `compra_otro` = Q104_12,
         `compra_Ninguno` = Q104_11,
         `compra_otro_texto` = Q104_12_TEXT)

data <- data %>%
  mutate(across(starts_with("compra"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))


dfSummary(data[,c("compra_JeanMarie_dummy", "compra_HaciendaChocolat_dummy",
                  "compra_SemilaCacao360_dummy", "compra_Montadero_dummy",
                  "compra_PuertoRicoBeantoBar_dummy", "compra_CortesHermanos_dummy",
                  "compra_BajariChocolate_dummy", "compra_LoizaDark_dummy",
                  "compra_Ninguno_dummy", "compra_otro_dummy",
                  "compra_otro_texto_dummy")])




# Summary of ¿Cómo clasificaría su conocimiento en la producción y manejo del cacao? 
# we renamed the column as "conocimiento_cacao"
# (1) es Deficiente, (5) es Promedio, y (9) Excelente
data <- data %>%
  rename(`conocimiento_cacao` = Q105)
dfSummary(data[,"conocimiento_cacao"])





# Indique el nivel de conocimiento que tiene, sobre las siguientes enfermedades y plagas
#Poco o nada= 1, Medio =2, Alto =3
data <- data %>%
  rename(`conocimiento_mazorcanegra` = Q159_1,
         `conocimiento_antracnosis` = Q159_2,
         `conocimiento_buba` = Q159_3,
         `conocimiento_rosellinia` = Q159_4,
         `conocimiento_rata` = Q159_5,
         `conocimiento_aves` = Q159_6,
         `conocimiento_phyllophaga` = Q159_7,
         `conocimiento_chinchecacao` = Q159_8,
         `conocimiento_otro` = Q159_9,
         `conocimiento_otro_texto` = Q159_9_TEXT)

dfSummary(data[,c("conocimiento_mazorcanegra","conocimiento_antracnosis",
                  "conocimiento_buba","conocimiento_rosellinia","conocimiento_rata",
                  "conocimiento_aves","conocimiento_phyllophaga","conocimiento_chinchecacao",
                  "conocimiento_otro","conocimiento_otro_texto")])






# Summary of Seleccione los principales problemas que tiene en la plantación de cacao. Dummy variables, Si=1; No=0
data <- data %>%
  rename(`problema_mazorcanegra` = Q106_1,
         `problema_antracnosis` = Q106_2,
         `problema_buba` = Q106_3,
         `problema_rata` = Q106_9,
         `problema_manoobra` = Q106_10,
         `problema_edadplantacion` = Q106_11,
         `problema_financiamiento` = Q106_12,
         `problema_fertilidadsuelo` = Q106_13,
         `problema_costoinsumos` = Q106_14,
         `problema_excesosombra` = Q106_15,
         `problema_productividadcacao` = Q106_16,
         `problema_asistenciatecnica` = Q106_17,
         `problema_sequias` = Q106_20,
         `problema_inundacion` = Q106_21,
         `problema_tormentas` = Q106_22,
         `problema_otro` = Q106_18,
         `problema_otro_texto` = Q106_18_TEXT)

data <- data %>%
  mutate(across(starts_with("problema"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))


dfSummary(data[,c("problema_mazorcanegra_dummy","problema_antracnosis_dummy","problema_buba_dummy",
                  "problema_rata_dummy","problema_manoobra_dummy","problema_edadplantacion_dummy",
                  "problema_financiamiento_dummy","problema_fertilidadsuelo_dummy",
                  "problema_costoinsumos_dummy","problema_excesosombra_dummy",
                  "problema_productividadcacao_dummy","problema_asistenciatecnica_dummy",
                  "problema_sequias_dummy","problema_inundacion_dummy","problema_tormentas_dummy",
                  "problema_otro_dummy","problema_otro_texto_dummy")])





# Enumere los tres problemas más importantes. Variable --> Variable continua
#Seleccionando 1 para el problema más importante, 2 para el segundo más importante, y 3 para el tercer problema más importante.
data <- data %>%
  rename(`importancia_mazorcanegra` = Q107_1,
         `importancia_antracnosis` = Q107_2,
         `importancia_buba` = Q107_3,
         `importancia_rata` = Q107_4,
         `importancia_manoobra` = Q107_5,
         `importancia_edadplantacion` = Q107_6,
         `importancia_financiamiento` = Q107_7,
         `importancia_fertilidadsuelo` = Q107_8,
         `importancia_costoinsumos` = Q107_9,
         `importancia_excesosombra` = Q107_10,
         `importancia_productividadcacao` = Q107_11,
         `importancia_asistenciatecnica` = Q107_12,
         `importancia_sequias` = Q107_13,
         `importancia_inundacion` = Q107_14,
         `importancia_tormentas` = Q107_15,
         `importancia_otro` = Q107_16,
         `importancia_otro_texto` = Q107_16_TEXT)

dfSummary(data[,c("importancia_mazorcanegra","importancia_antracnosis","importancia_buba",
                  "importancia_rata","importancia_manoobra","importancia_edadplantacion",
                  "importancia_financiamiento","importancia_fertilidadsuelo","importancia_costoinsumos",
                  "importancia_excesosombra","importancia_productividadcacao","importancia_asistenciatecnica",
                  "importancia_sequias","importancia_inundacion","importancia_tormentas",
                  "importancia_otro","importancia_otro_texto")])






# Summary of ¿Qué tipo de material genético tiene y en qué porcentaje?
data$material_hibrido <- as.numeric(as.character(data$Q108_1))
data$material_clon_internacional <- as.numeric(as.character(data$Q108_2))
data$material_clon_tars <- as.numeric(as.character(data$Q108_4))

dfSummary(data[,c("material_hibrido","material_clon_internacional","material_clon_tars")])






# Summary of ¿Tiene sistema de riego para el cultivo de cacao?
# We create a dummy variable "riego" where, tiene riego: 1 = Si; 0 = No.
data <- data %>%
  mutate(riego = ifelse(Q111 == "Si", 1, 0))
dfSummary(data[,"riego"])
dfSummary(data[,"Q111"])






# Summary of ¿En qué condición posee la finca?
# we renamed the column as "tenencia_finca"
data <- data %>%
  rename(tenencia_finca = Q112)
dfSummary(data[,"tenencia_finca"])





# Summary of ¿Cuánto paga anualmente por cuerda, en promedio? (USD$)
# we renamed the column as "renta_tarea"
data$renta_tarea <- as.numeric(as.character(data$Q113_1))
summary(data$renta_tarea)





# Summary of ¿El cacaotal está combinado con otras especies, ejemplo, frutales, maderables, servicios, etc.)?
# we create a column "plantacion_combinada" where; if cacaotal esta combinado con otras especia: 1 = Yes; 0 = No.
data <- data %>%
  mutate(plantacion_combinada = ifelse(Q114 == "Si", 1, 0))
dfSummary(data[,"plantacion_combinada"])







# Summary of ¿Tiene área dentro de su cacaotal con exceso de sombra? Dummy variable, 1 = Yes; 0 = No.
data <- data %>%
  mutate(sombra_problema = ifelse(Q115 == "Si", 1, 0))
dfSummary(data[,"sombra_problema"])






# Summary of Por favor, indique el tipo de especies que tiene:  Dummy variable, Si=1, No=0
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
data <- data %>%
  rename(`especies_maderables` = Q116_1,
         `especies_servicio` = Q116_2)

data <- data %>%
  mutate(across(starts_with("especies"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("especies_maderables_dummy","especies_servicio_dummy")])





# Summary of Por favor, indique la cantidad aproximada que tiene en su finca por cuerda
data$maderables_porcuerda <- as.numeric(as.character(data$Q117_1))
data$servicios_porcuerda <- as.numeric(as.character(data$Q118_2))

dfSummary(data[,c("maderables_porcuerda","servicios_porcuerda")])

summary(data$maderables_porcuerda)
summary(data$servicios_porcuerda)







#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# ESTRUCTURA DE COSTOS, GASTOS Y ACTIVOS EN EL CACAOTAL


# Summary of Conoce el costo de producción por cada quintal de cacao producido?
# we renamed the column as "conocimiento_costoprod" where; if conoce el costo de produccion por quintal: 1 = Si; 0 = No.
data <- data %>%
  mutate(conocimiento_costoprod = ifelse(Q122 == "Si", 1, 0))
dfSummary(data[,"conocimiento_costoprod"])





# Summary of Si lo conoce, en cuánto estima el costo de producir un quintal de cacao
# we renamed the column as "costo_quintal"
non_numeric_values <- data$Q123_1[!grepl("^\\d+$", data$Q123_1)]
unique(non_numeric_values)
# Replace commas and convert to numeric
data$Q123_1_cleaned <- gsub(",", "", data$Q123_1)
data$costo_quintal <- as.numeric(data$Q123_1_cleaned)

# We checked for any remaining non-numeric values
non_numeric_costoquintal <- data$costo_quintal[!is.na(data$costo_quintal) & !is.finite(data$costo_quintal)]
unique(non_numeric_costoquintal)

# Print summary of el costo de producir 1 quintal de cacao (RD$/QQ)
summary(data$costo_quintal)






# Summary of Por favor, indique que tipo de mano de obra utiliza para el manejo de su plantación de cacao.  Dummy variable, Si=1, No=0
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
data <- data %>%
  rename(`manoobra_temporal` = Q124_1,
         `manoobra_permanente` = Q124_2,
         `manoobra_familiar` = Q124_20)

data <- data %>%
  mutate(across(starts_with("manoobra"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("manoobra_temporal_dummy","manoobra_permanente_dummy","manoobra_familiar_dummy")])





# Summary of Cómo maneja su plantación
# we created two new dummy variables (not one, because options are not mutually exclusive), 
# "organico" and "convencional" where; if plantacion es: 1 = Yes; 0 = No.
data <- data %>%
  rename(`manejo_organico` = Q126_1,
         `manejo_convencional` = Q126_2)

data <- data %>%
  mutate(across(starts_with("manejo"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,"manejo_organico_dummy"])






# Summary of Por favor indique si utiliza alguno de los siguientes insumos. Dummy variable, Si=1, No=0.
data <- data %>%
  rename(`insumo_ferti_organico` = Q127_1,
         `insumo_ferti_quimico` = Q127_2,
         `insumo_insecti_organico` = Q127_20,
         `insumo_insecti_quimico` = Q127_21,
         `insumo_fungi_organico` = Q127_22,
         `insumo_fungi_quimico` = Q127_23,
         `insumo_herbicida` = Q127_24,
         `insumo_alambre` = Q127_25,
         `insumo_postes` = Q127_26,
         `insumo_grapas` = Q127_27,
         `insumo_sacos` = Q127_28,
         `insumo_combustible_secado` = Q127_29,
         `insumo_combustible_otro` = Q127_30,
         `insumo_otros` = Q127_32,
         `insumo_otros_texto` = Q127_32_TEXT)

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
  rename(`herramienta_machete` = Q130_1,
         `herramienta_cuchilla` = Q130_2,
         `herramienta_podadora` = Q130_20,
         `herramienta_motosierra` = Q130_21,
         `herramienta_serrucho` = Q130_22,
         `herramienta_guillotina` = Q130_23,
         `herramienta_tanque` = Q130_24,
         `herramienta_pala` = Q130_25,
         `herramienta_rastrillo` = Q130_26,
         `herramienta_pico` = Q130_27,
         `herramienta_coas` = Q130_28,
         `herramienta_martillo` = Q130_29,
         `herramienta_otra` = Q130_34,
         `herramienta_otra_texto` = Q130_34_TEXT)

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
data$deuda_vigente_dummy <- ifelse(data$Q132 == "Si", 1,
                                      ifelse(data$Q132 == "No", 0, NA))
dfSummary(data[,"deuda_vigente_dummy"])





# Follow-up question
# Summary of ¿Cuánto es el monto de la deuda vinculada a la plantación de cacao?
# we renamed the column as "monto_deuda"
non_numeric_values <- data$Q133_1[!grepl("^\\d+$", data$Q133_1)]
unique(non_numeric_values)

# Replace commas and convert to numeric
data$Q133_1_cleaned <- gsub(",", "", data$Q133_1)
data$monto_deuda <- as.numeric(data$Q133_1_cleaned)

dfSummary(data[,"monto_deuda"])
summary(data$monto_deuda)





# Summary of ¿Cuál es el % interés? 
# we renamed the column as "interes_anual"
data$interes_anual <- as.numeric(as.character(data$Q134_2))
summary(data$interes_anual)




# Summary of ¿De cuánto es el plazo en años para pagar la deuda?
# we renamed the column as "plazo_deuda" en años
data$plazo_deuda <- as.numeric(as.character(data$Q135_3))
summary(data$plazo_deuda) #años





# Summary of Con qué entidad tiene el préstamo: Dummy variable: Si=1, No=0.
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
data <- data %>%
  rename(`prestamo_puertoricofarmcredit` = Q136_1,
         `prestamo_farmserviceagency` = Q136_2,
         `prestamo_intermediario` = Q136_3,
         `prestamo_cooperativas` = Q136_4,
         `prestamo_bancosprivados` = Q136_5)

data <- data %>%
  mutate(across(starts_with("prestamo"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("prestamo_puertoricofarmcredit_dummy","prestamo_farmserviceagency_dummy",
                  "prestamo_intermediario_dummy","prestamo_cooperativas_dummy",
                  "prestamo_bancosprivados_dummy")])






# Summary of ¿Cuál es el valor promedio por tarea de las fincas/terreno en los alrededores?
# we renamed the column as "valor_finca"
data <- data %>%
  rename(valor_finca = Q137_4)
dfSummary(data[,"valor_finca"])





# Summary of Indique las actividades que realiza en su finca
data <- data %>%
  rename(`actividad_cosechacacao` = Q138_1,
         `actividad_desmonte_mazorcasenfermas` = Q138_2,
         `actividad_podacacao` = Q138_20,
         `actividad_deschuponado` = Q138_21,
         `actividad_abonamiento` = Q138_22,
         `actividad_aplicac_insecti` = Q138_23,
         `actividad_aplicac_fungi` = Q138_24,
         `actividad_deshierbe` = Q138_25,
         `actividad_resiembra` = Q138_26,
         `actividad_controlratas` = Q138_27,
         `actividad_renovacion` = Q138_28,
         `actividad_rehabilitacion_plantasviejas` = Q138_29,
         `actividad_deshije_musaceas` = Q138_35,
         `actividad_deshoje_musaceas` = Q138_36,
         `actividad_cosecha_musaceas` = Q138_37,
         `actividad_cosecha_otrosfrutales` = Q138_38,
         `actividad_fertiliz_otrosfrutales` = Q138_39,
         `actividad_poda_sombra` = Q138_40,
         `actividad_poda_frutales` = Q138_41,
         `actividad_poda_maderables` = Q138_34,
         `actividad_otra` = Q138_45)

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
                  "actividad_poda_frutales_dummy","actividad_poda_maderables_dummy","actividad_otra_dummy")])







# Summary of ¿Tiene otra actividad agropecuaria, adicional a su sistema agroforestal de cacao? Dummy variable
# we create a dummy variable "otra_actividad" where; Si=1; No=0.
data$actividad_extra_dummy <- ifelse(data$Q140 == "Si", 1,
                                  ifelse(data$Q140 == "No", 0, NA))
dfSummary(data[,"actividad_extra_dummy"])
                  
                  



# Follow-up question
# Summary of Seleccione las actividades extras que aplican:
data <- data %>%
  rename(`extra_agroturismo` = Q141_1,
         `extra_ganaderia` = Q141_2,
         `extra_avicultura` = Q141_3,
         `extra_porcicultura` = Q141_4,
         `extra_aguacate` = Q141_5,
         `extra_cafe` = Q141_6,
         `extra_vegetales` = Q141_7,
         `extra_citricos` = Q141_8,
         `extra_raicestuberculos` = Q141_9,
         `extra_arroz` = Q141_10,
         `extra_coco` = Q141_12,
         `extra_mango` = Q141_13,
         `extra_musaceas` = Q141_14,
         `extra_otros` = Q141_15,
         `extra_otros_texto` = Q141_15_TEXT)

data <- data %>%
  mutate(across(starts_with("extra"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("extra_agroturismo_dummy","extra_ganaderia_dummy","extra_avicultura_dummy",
                  "extra_porcicultura_dummy","extra_aguacate_dummy","extra_cafe_dummy",
                  "extra_vegetales_dummy","extra_citricos_dummy","extra_raicestuberculos_dummy",
                  "extra_arroz_dummy","extra_coco_dummy", "extra_mango_dummy",
                  "extra_musaceas_dummy","extra_otros_dummy", "extra_otros_texto_dummy")])





# Summary of Seleccione las que aplican:
#Fuera de la finca, ¿Realiza usted y sus dependientes en el hogar otras actividades que
#generan ingresos fuera de la agricultura? (Ejemplo, empleado, comerciante, consultorías, etc.)

# USTED
# we create a dummy variable "otrasactividades_usted" where; 1 = Yes; 0 = No.
data$Q142_1 <- trimws(data$Q142_1)
data$otrasactividades_usted <- ifelse(data$Q142_1 == "Si", 1,
                                      ifelse(data$Q142_1 == "No", 0, NA))
dfSummary(data[,"otrasactividades_usted"])


#OTRO MIEMBRO DEL HOGAR
# we create a dummy variable "otrasactividades_otros" where; 1 = Yes; 0 = No.
data$Q142_2 <- trimws(data$Q142_2)
data$otrasactividades_otros <- ifelse(data$Q142_2 == "Si", 1,
                                      ifelse(data$Q142_2 == "No", 0, NA))
dfSummary(data[,"otrasactividades_otros"])


dfSummary(data[,c("otrasactividades_usted", "otrasactividades_otros")])
dfSummary(data[,c("Q142_1", "Q142_2")])





# ¿Qué porcentaje del ingreso familiar proviene de cacaotal (un estimado)?
data$ingresofamilia_cacao <- as.numeric(as.character(data$Q143_11))
summary(data$ingresofamilia_cacao)





#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# CONOCIMIENTO DE LA ENFERMEDAD MONILIASIS


# Conocimiento de la enfermedad moniliasis:
# we create a dummy variable "conocimiento_moniliasis" where; 1 = Yes; 0 = No.
data$conocimiento_moniliasis <- ifelse(data$Q144 == "Si", 1,
                                       ifelse(data$Q144 == "No", 0, NA))
dfSummary(data[,"conocimiento_moniliasis"])




# ¿Sabe cómo se controla?
# we create a dummy variable "conocimiento_control" where; 1 = Yes; 0 = No.
data$conocimiento_control <- ifelse(data$Q145 == "Si", 1,
                                       ifelse(data$Q145 == "No", 0, NA))
dfSummary(data[,"conocimiento_control"])




dfSummary(data[,c("conocimiento_moniliasis","conocimiento_control")])




#¿Qué piensa que podría pasar con la Producción Nacional si esta enfermedad (Moniliasis del cacao) llega al país?
data = data %>%
  mutate(llegada_moniliasis_opinion = case_when(
    Q146 == "Se mantendría igual" ~ 1,
    Q146 == "Bajaría" ~ 2,
    Q146 == "No sé" ~ 3
  ))


#¿Considera que los productores abandonarían el sector cacao si esta enfermedad (Moniliasis del cacao) llega al país?
data = data %>%
  mutate(abandonar_sectorcacao = case_when(
    Q147 == "Si" ~ 1,
    Q147 == "Solo parcialmente" ~ 2,
    Q147 == "No" ~ 3
  ))


#¿Piensa que podrían bajar los precios del cacao si esta enfermedad (Moniliasis del cacao) llega al país?
data = data %>%
  mutate(bajaprecios_sectorcacao = case_when(
    Q148 == "Si" ~ 1,
    Q148 == "No sé" ~ 2,
    Q148 == "No" ~ 3
  ))



# ¿Considera que se afectaría la calidad del cacao si esta enfermedad (Moniliasis del cacao) llega al país?
data = data %>%
  mutate(calidad_cacao = case_when(
    Q149 == "Si" ~ 1,
    Q149 == "No sé" ~ 2,
    Q149 == "No" ~ 3
  ))

dfSummary(data[,c("Q146","Q147","Q148","Q149")])
dfSummary(data[,c("llegada_moniliasis_opinion","abandonar_sectorcacao","bajaprecios_sectorcacao","calidad_cacao")])





#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#--------------------EL FUTURO DE SU FINCA CACAOTAL----------------------------


# ¿Planea realizar inversiones de mejora en su cacaotal?
# we create a dummy variable "mejoras_cacaotal" where; 1 = Yes; 0 = No.
data$mejoras_cacaotal_dummy <- ifelse(data$Q150 == "Si", 1,
                                       ifelse(data$Q150 == "No", 0, NA))
dfSummary(data[,"mejoras_cacaotal_dummy"])





# Summary of ¿Cuáles son sus planes de inversión? Dummy variable, Si=1, No=0
data <- data %>%
  rename(`planinversion_rehabfinca` = Q151_4,
         `planinversion_renovfinca` = Q151_5,
         `planinversion_riego` = Q151_6,
         `planinversion_agroforestal` = Q151_7,
         `planinversion_otro` = Q151_8)

data <- data %>%
  mutate(across(starts_with("planinversion"), ~ if_else(is.na(.) | . == "", 0, 1), .names = "{.col}_dummy"))

dfSummary(data[,c("planinversion_rehabfinca_dummy","planinversion_renovfinca_dummy",
                  "planinversion_riego_dummy","planinversion_agroforestal_dummy",
                  "planinversion_otro_dummy")])


# ¿Estaría dispuesto a participar en la socialización y validación de los resultados de este estudio?
data <- data %>%
  rename(`futura_participacion` = Q152)

#¿Desea que los resultados de este estudio le lleguen a su correo o WhatsApp?
data <- data %>%
  rename(`enviar_resultados_estudio` = Q153)

# Favor facilitar su correo electrónico y/o WhatsApp: - Correo electrónico
data <- data %>%
  rename(`correo_electronico` = Q154_1)

# Favor facilitar su correo electrónico y/o WhatsApp: - WhatsApp
data <- data %>%
  rename(`whatsapp` = Q154_2)

                  
                  
# This is the last step to our data cleaning
data_puertorico <- subset(data, select = -c(StartDate, EndDate,	Status,	IPAddress, RecordedDate,	ResponseId,	RecipientLastName,
                                            RecipientFirstName,	RecipientEmail,	ExternalReference, LocationLatitude,	LocationLongitude,	
                                            DistributionChannel,	UserLanguage, Q84,	Q85_1,	Q86,	Q88_1,	Q89_1,	Q90_1, Q92,	Q94_7,	
                                            Q95_7, Q97_1, Q99_1, Q108_1,	Q108_2,	Q108_4,Q111, Q113_1,	Q114,	Q115, Q117_1,	Q118_2,
                                            Q122, Q132, Q134_2,	Q135_3, Q140, Q142_1,	Q142_2,	Q143_11,	Q144,	Q145,	Q146,	Q147,	Q148,	Q149,	Q150))





# Saving the file as excel
write.xlsx(data_puertorico, file = "C:/Users/gar14685/OneDrive - Texas Tech University/WORK/CATIE/data_output/data_pr.xlsx")

