#------------------------------------------------------------------------------
# Project: ROAR Project
# Title: Dominican Republic - Summary statistics
# Date: November 2023
#------------------------------------------------------------------------------

install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("stringr")

library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)

survey_data <- read_excel("D:\\OneDrive - Texas Tech University\\WORK\\CATIE\\data\\DR\\Encuesta+a+productores+expertos+de+cacao_November+25,+2023_12.17.xlsx")
data <- survey_data[-1, ]


#Hola este es mi cambio
# SUMMARY STATISTICS
#-------------------------------------------------------------------------------

# Summary of Productores encuestados
print(table(data$Q1))
print(prop.table(table(data$Q1))*100)

# We will delete observation who did Not want to participate
data <- data[data$Q1 != "No", ]


# Summary of ¿En cuál provincia está su finca de cacao?
# we renamed the column as "provincia" for clarity
data <- data %>%
  rename(provincia = Q4)

sorted_province_table <- sort(table(data$provincia), decreasing = TRUE)
print(sorted_province_table)

# Calculate proportions
print(prop.table(sorted_province_table) * 100)



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

sorted_paraje_table <- sort(table(data$paraje), decreasing = TRUE)
print(sorted_paraje_table)

# Calculate proportions
print(prop.table(sorted_paraje_table) * 100)



# Summary of Género del productor
# we renamed the column as "genero"
data <- data %>%
  rename(genero = Q140)

print(table(data$genero))
# Calculate proportions
print(prop.table(table(data$genero))*100)



# Summary of Edad (años)
# we convert age to numeric (non-numeric values will become NA) and saved on a new column as "edad"
data$edad <- as.numeric(as.character(data$Q141_1))
summary(data$edad)



# Summary of Estado civil
print(table(data$Q142))
print(prop.table(table(data$Q142))*100)



# Summary of ¿Cuál es su nivel de educación escolar?
# we renamed the column as "edu"
data <- data %>%
  rename(edu = Q143)

# Sort the table from high to low
sorted_edu_table <- sort(table(data$edu), decreasing = TRUE)
print(sorted_edu_table)

# Calculate proportions
print(prop.table(sorted_edu_table) * 100)



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

print(table(data$viento))
# Calculate proportions
print(prop.table(table(data$viento))*100)



# Summary of Seleccione el rango del área total del cacaotal en producción (tareas)
# we renamed the column as "area_rango"
data <- data %>%
  rename(area_rango = Q148)

print(table(data$area_rango))
# Calculate proportions
print(prop.table(table(data$area_rango))*100)



# Summary of Area de la Finca
data$tareas_1_80 <- as.numeric(as.character(data$Q149_7))
data$tareas_81_160 <- as.numeric(as.character(data$Q150_7))
data$tareas_160 <- as.numeric(str_extract(data$Q151, "\\d+"))

# We create a new column named "area_finca"
data <- data %>%
  tidyr::unite("area_finca_str", tareas_1_80,tareas_81_160,tareas_160, sep = " ", na.rm = TRUE, remove = TRUE)

data$area_finca <- as.numeric(as.character(data$area_finca_str))
print(table(data$area_finca))



# Summary of edad aproximada de su plantación de cacao en años
# we renamed the column as "edad_plantacion"
data$edad_plantacion <- as.numeric(as.character(data$Q152_1))
summary(data$edad_plantacion)



# Summary of ¿Cuál es el marco de plantación (planta x hileras) de su cacaotal?
print(table(data$Q153_1))       #2 x 2
print(table(data$Q153_2))       #2.5 x 2
print(table(data$Q153_3))       #2.5 x 2.5
print(table(data$Q153_4))       #2.5 x 3 
print(table(data$Q153_5))       #3 x 3 
print(table(data$Q153_6))       #3 x 3.5 
print(table(data$Q153_7))       #3.5 x 3.5
print(table(data$Q153_8))       #3.5 x 4 
print(table(data$Q153_9))       #4 x 4 
print(table(data$Q153_10))      #Otro, indique:
print(table(data$Q153_11))      #No tengo o no lo sé 
print(table(data$Q153_10_TEXT)) #Otro, indique:
    

                                      
# Summary of Si no tiene un marco de plantación definido, cuántas plantas por tarea tiene en promedio
# Display this question, If ¿Cuál es el marco de plantación (planta x hileras) de su cacaotal?: = No tengo o no lo sé
# we renamed the column as "plantas_tarea"
data$plantas_tarea <- as.numeric(as.character(data$Q154_1))
summary(data$plantas_tarea)



# Summary of ¿Cómo es la pendiente del terreno en promedio?
print(table(data$Q155_1)) #Completamente Plano
print(table(data$Q155_2)) #Inclinaciones suave y ligeras 
print(table(data$Q155_3)) #Inclinaciones media 
print(table(data$Q155_4)) #Inclinaciones media a fuerte
print(table(data$Q155_5)) #Inclinaciones fuertes 
print(table(data$Q155_6)) #Inclinaciones fuertes a muy fuerte
print(table(data$Q155_7)) #Pendientes muy fuerte 



# Summary of Producción Anual (quintales)
# we renamed the column as "quintales_year"
data$quintales_year <- as.numeric(as.character(data$Q156_1))
summary(data$quintales_year)
	


# Summary of Precio de venta RD$/quintal
# we renamed the column as "precio_quintal"
data$precio_quintal <- as.numeric(as.character(data$Q156_3))
summary(data$precio_quintal)



# Summary of ¿Cómo vende el cacao?
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
# Count responses for each option
# Convert character to numeric
data$Q157_1_numeric <- as.numeric(data$Q157_1 == "Baba")
data$Q157_2_numeric <- as.numeric(data$Q157_2 == "Fermentado")
data$Q157_3_numeric <- as.numeric(data$Q157_3 == "Seco")
data$Q157_4_numeric <- as.numeric(data$Q157_4 == "Procesado en licor / chocolate")

baba_count <- sum(data$Q157_1_numeric, na.rm = TRUE)
fermentado_count <- sum(data$Q157_2_numeric, na.rm = TRUE)
seco_count <- sum(data$Q157_3_numeric, na.rm = TRUE)
procesado_count <- sum(data$Q157_4_numeric, na.rm = TRUE)

# Calculate total number of respondents
# This might count only those who answered any of these questions
responded <-  (!is.na(data$Q157_1) & data$Q157_1 %in% c("Baba")) |
              (!is.na(data$Q157_2) & data$Q157_2 %in% c("Fermentado")) |
              (!is.na(data$Q157_3) & data$Q157_3 %in% c("Seco")) |
              (!is.na(data$Q157_4) & data$Q157_4 %in% c("Procesado en licor / chocolate"))

total_respondents <- sum(responded)

# Calculate percentages
baba_percent <- (baba_count / total_respondents) * 100
fermentado_percent <- (fermentado_count / total_respondents) * 100
seco_percent <- (seco_count / total_respondents) * 100
procesado_percent <- (procesado_count / total_respondents) * 100

# Print results
print(paste("Baba:", baba_percent, "%"))
print(paste("Fermentado:", fermentado_percent, "%"))
print(paste("Seco:", seco_percent, "%"))
print(paste("Procesado en licor / chocolate:", procesado_percent, "%"))





# Summary of ¿A quién vende el cacao?
print(table(data$Q158_1))
print(table(data$Q158_2))
print(table(data$Q158_3))
print(table(data$Q158_4))
print(table(data$Q158_5))
print(table(data$Q158_6))
print(table(data$Q158_7))
print(table(data$Q158_8))
print(table(data$Q158_9))
print(table(data$Q158_10))
print(table(data$Q158_11))
print(table(data$Q158_12))
print(table(data$Q158_12_TEXT))



# Summary of ¿Cómo clasificaría su conocimiento en la producción y manejo del cacao? 
# we renamed the column as "conocimiento_cacao"
# (1) es Deficiente, (5) es Promedio, y (9) Excelente
data$conocimiento_cacao <- as.numeric(as.character(data$Q159))
summary(data$conocimiento_cacao) #continuous scale
print(prop.table(table(data$Q159))*100) #discrete scale 



# Summary of Seleccione los principales problemas que tiene en la plantación de cacao
print(table(data$Q160_1)) #Mazorca negra 
print(table(data$Q160_2)) #Antracnosis
print(table(data$Q160_3)) #Buba del cacao
print(table(data$Q160_4)) #Pérdidas por rata 
print(table(data$Q160_5)) #Disponibilidad mano de obra 
print(table(data$Q160_6)) #Edad avanzada de la plantación
print(table(data$Q160_7)) #Acceso a financiamiento 
print(table(data$Q160_8)) #Fertilidad del suelo
print(table(data$Q160_9)) #Costos de insumos de producción
print(table(data$Q160_10))#Exceso de sombra 
print(table(data$Q160_11))#Productividad del cacao 
print(table(data$Q160_12))#Frecuencia de asistencia técnica
print(table(data$Q160_13))#Otras:
print(table(data$Q160_13_TEXT))#Otras:



# Summary of ¿Qué tipo de material genético tiene y en qué porcentaje?
# The baseline is Plantas híbridas (semilla) out of 100% 
# For exmaple, 90% means, 90% are plantas híbridas and only 10% clones
data$hibridas <- as.numeric(as.character(data$Q162_1))
data$clones <- as.numeric(as.character(data$Q162_2))
summary(data$hibridas) #Plantas híbridas (semilla) vs clones



# Summary of ¿Conoce que tipo de plantas/clones tiene en su finca?
# We create a dummy variable "conocimiento_matgenetico" where: 
#if the producer conoce que tipo de plantas/clones tiene en su finca: 1 = Yes; 0 = No.
data <- data %>%
  mutate(conocimiento_matgenetico = ifelse(Q163 == "Si", 1, 0))

print(table(data$conocimiento_matgenetico))
# Calculate proportions
print(prop.table(table(data$conocimiento_matgenetico))*100)



# Summary of ¿Tiene sistema de riego para el cultivo de cacao?
# We create a dummy variable "riego" where, tiene riego: 1 = Yes; 0 = No.
data <- data %>%
  mutate(riego = ifelse(Q166 == "Si", 1, 0))

print(table(data$riego))
# Calculate proportions
print(prop.table(table(data$riego))*100)



# Summary of ¿En qué condición posee la finca?
# we renamed the column as "tenencia_finca"
data <- data %>%
  rename(tenencia_finca = Q167)

print(table(data$tenencia_finca))
# Calculate proportions
print(prop.table(table(data$tenencia_finca))*100)



# Summary of ¿Cuánto paga anualmente por tarea, en promedio?
# we renamed the column as "renta_tarea"
data$renta_tarea <- as.numeric(as.character(data$Q168_1))
summary(data$renta_tarea)



# Summary of ¿El cacaotal está combinado con otras especies, ejemplo, frutales, maderables, servicios, etc.)?
# we renamed the column as "plantacion_combinada" where; if cacaotal esta combinado con otras especia: 1 = Yes; 0 = No.
data <- data %>%
  mutate(plantacion_combinada = ifelse(Q169 == "Si", 1, 0))

print(table(data$plantacion_combinada))
# Calculate proportions
print(prop.table(table(data$plantacion_combinada))*100)




# Summary of Por favor, indique el tipo de especies que tiene:
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
# Count responses for each option
# Convert character to numeric
data$Q170_1_numeric <- as.numeric(grepl("Árboles maderables", data$Q170_1))
data$Q170_2_numeric <- as.numeric(grepl("Árboles de servicios", data$Q170_2))
data$Q170_3_numeric <- as.numeric(grepl("Frutales", data$Q170_3))
data$Q170_4_numeric <- as.numeric(grepl("Musáceas", data$Q170_4))

maderables_count <- sum(data$Q170_1_numeric, na.rm = TRUE)
servicios_count <- sum(data$Q170_2_numeric, na.rm = TRUE)
frutales_count <- sum(data$Q170_3_numeric, na.rm = TRUE)
musaceas_count <- sum(data$Q170_4_numeric, na.rm = TRUE)


# Calculate total number of respondents
# This might count only those who answered any of these questions
responded <- (!is.na(data$Q170_1) & grepl("Árboles maderables", data$Q170_1)) |
  (!is.na(data$Q170_2) & grepl("Árboles de servicios", data$Q170_2)) |
  (!is.na(data$Q170_3) & grepl("Frutales", data$Q170_3)) |
  (!is.na(data$Q170_4) & grepl("Musáceas", data$Q170_4))

total_respondents <- sum(responded)

# Calculate percentages
maderables_percent <- (maderables_count / total_respondents) * 100
servicios_percent <- (servicios_count / total_respondents) * 100
frutales_percent <- (frutales_count / total_respondents) * 100
musaceas_percent <- (musaceas_count / total_respondents) * 100

# Print results
print(paste("Arboles maderables:", maderables_percent, "%"))
print(paste("Arboles de servicio:", servicios_percent, "%"))
print(paste("Frutales:", frutales_percent, "%"))
print(paste("Musaceas, otras", musaceas_percent, "%"))


# Summary of Por favor, indique la cantidad aproximada que tiene en su finca por tarea
# we renamed the column as "maderables_tarea"
# we renamed the column as "servicios_tarea"
# we renamed the column as "frutales_tarea"
# we renamed the column as "musaceas_tarea"
data$maderables_tarea <- as.numeric(as.character(data$Q171_1))
data$servicios_tarea <- as.numeric(as.character(data$Q172_2))
data$frutales_tarea <- as.numeric(as.character(data$Q173_2))
data$musaceas_tarea <- as.numeric(as.character(data$Q174_2))

summary(data$maderables_tarea)
summary(data$servicios_tarea)
summary(data$frutales_tarea)
summary(data$musaceas_tarea)



#--------------------------------------------------------------------------------

# Estructura de costos, gastos y activos en el cacaotal

# Summary of Conoce el costo de producción por cada quintal de cacao producido?
# we renamed the column as "conocimiento_costoprod" where; if conoce el costo de produccion por quintal: 1 = Yes; 0 = No.
data <- data %>%
  mutate(conocimiento_costoprod = ifelse(Q178 == "Si", 1, 0))

print(table(data$conocimiento_costoprod))
# Calculate proportions
print(prop.table(table(data$conocimiento_costoprod))*100)





# Summary of Si lo conoce, en cuánto estima el costo de producir un quintal de cacao
# we renamed the column as "costo_quintal"
non_numeric_values <- data$Q179_1[!grepl("^\\d+$", data$Q179_1)]
unique(non_numeric_values)
# Replace commas and convert to numeric
data$Q179_1_cleaned <- gsub(",", "", data$Q179_1)
data$costo_quintal <- as.numeric(data$Q179_1_cleaned)

# We checked for any remaining non-numeric values
non_numeric_values_cleaned <- data$costo_quintal[!is.na(data$costo_quintal) & !is.finite(data$costo_quintal)]
unique(non_numeric_values_cleaned)

# Print summary of el costo de producir 1 quintal de cacao
print(summary(data$costo_quintal))





# Summary of Por favor, indique que tipo de mano de obra utiliza para el manejo de su plantación de cacao.
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
# Count responses for each option
# Convert character to numeric
data$Q180_1_numeric <- as.numeric(grepl("temporal", data$Q180_1, ignore.case = TRUE))
data$Q180_2_numeric <- as.numeric(grepl("permanente", data$Q180_2, ignore.case = TRUE))
data$Q180_3_numeric <- as.numeric(grepl("familiar", data$Q180_3, ignore.case = TRUE))

# Calculate counts
motemporal_count <- sum(data$Q180_1_numeric, na.rm = TRUE)
mopermanente_count <- sum(data$Q180_2_numeric, na.rm = TRUE)
mofamiliar_count <- sum(data$Q180_3_numeric, na.rm = TRUE)

# Calculate total number of respondents
# This might count only those who answered any of these questions
# Simplified search for testing
responded <- (!is.na(data$Q180_1) & grepl("temporal", data$Q180_1, ignore.case = TRUE)) |
  (!is.na(data$Q180_2) & grepl("permanente", data$Q180_2, ignore.case = TRUE)) |
  (!is.na(data$Q180_3) & grepl("familiar", data$Q180_3, ignore.case = TRUE))

total_respondents <- sum(responded)

# Calculate percentages
motemporal_percent <- (motemporal_count / total_respondents) * 100
mopermanente_percent <- (mopermanente_count / total_respondents) * 100
mofamiliar_percent <- (mofamiliar_count / total_respondents) * 100

# Print results
print(paste("Mano de obra temporal (al cacao)", motemporal_percent, "%"))
print(paste("Mano de obra permanente (al cacao)", mopermanente_percent, "%"))
print(paste("Mano de obra familiar (al cacao)", mofamiliar_percent, "%"))




# Summary of Cómo maneja su plantación
# we created two new dummy variables (not one, because options are not mutually exclusive), 
# "organico" and "convencional" where; if plantacion es: 1 = Yes; 0 = No.
data <- data %>%
  mutate(
    organico = ifelse(Q182_1 == "Orgánico", 1, 0),
    convencional = ifelse(Q182_1 == "Convencional", 1, 0)
  )


# summary stats for manejo de plantacion
# Convert character to numeric
data$Q182_1_numeric <- as.numeric(grepl("Orgánico", data$Q182_1))
data$Q182_2_numeric <- as.numeric(grepl("Convencional", data$Q182_2))

organic_count <- sum(data$Q182_1_numeric, na.rm = TRUE)
conventional_count <- sum(data$Q182_2_numeric, na.rm = TRUE)

# Calculate total number of respondents
# This might count only those who answered any of these questions
responded <- (!is.na(data$Q182_1) & grepl("Orgánico", data$Q182_1)) |
  (!is.na(data$Q182_2) & grepl("Convencional", data$Q182_2))

total_respondents <- sum(responded)

# Calculate percentages
organic_percent <- (organic_count / total_respondents) * 100
conventional_percent <- (conventional_count / total_respondents) * 100

# Print results
print(paste("Orgánico", organic_percent, "%"))
print(paste("Convencional", conventional_percent, "%"))



# Summary of Por favor indique si utiliza alguno de los siguientes insumos
print(table(data$Q183_1)) #Fertilizantes orgánicos
print(table(data$Q183_2)) #Fertilizante químico
print(table(data$Q183_3)) #Insecticida orgánico
print(table(data$Q183_4)) #Insecticida químico 
print(table(data$Q183_5)) #Fungicida orgánico 
print(table(data$Q183_6)) #Fungicida químico 
print(table(data$Q183_7)) #Herbicida  
print(table(data$Q183_8)) #Alambre para reparar cercas
print(table(data$Q183_9)) #Postes para reparar cercas
print(table(data$Q183_10))#Grapas para reparar cercas
print(table(data$Q183_11))#Sacos para empacar el cacao
print(table(data$Q183_12))#Combustible para secar el cacao
print(table(data$Q183_13))#Combustible (gasolina/Gasoil) para actividades relacionadas al cacao (ej, transporte del cacao en baba, seco, insumo). 
print(table(data$Q183_14))#Otros insumos:
print(table(data$Q183_14_TEXT))#Otros insumos:


# Summary of Por favor, indique las herramientas que usa en la actividad productiva
print(table(data$Q186_1)) #Machetes
print(table(data$Q186_2)) #Cuchilla de poda o cosecha
print(table(data$Q186_3)) #Podadora de altura
print(table(data$Q186_4)) #Motosierra 
print(table(data$Q186_5)) #Serrucho  
print(table(data$Q186_6)) #Guillotina  
print(table(data$Q186_7)) #Tanques   
print(table(data$Q186_8)) #Palas 
print(table(data$Q186_9)) #Rastrillos
print(table(data$Q186_10))#Picos 
print(table(data$Q186_11))#Coas 
print(table(data$Q186_12))#Martillos 
print(table(data$Q186_13))#Otras herramientas: 
print(table(data$Q186_13_TEXT))#Otras herramientas:




#--------------------------------------------------------------------------------

# Gastos financieros

# Summary of ¿Tiene préstamos vigentes vinculados a la plantación de cacao
#(ejemplo, fomento, rehabilitación, renovación, transporte, etc.? Nota: Si tiene préstamos que no
#están relacionados al cacao, no aplican.
# we create a dummy variable "prestamos_vigentes" where; 1 = Yes; 0 = No.
data <- data %>%
  mutate(prestamos_vigentes = ifelse(Q188 == "Si", 1, 0))

print(table(data$prestamos_vigentes))
# Calculate proportions
print(prop.table(table(data$prestamos_vigentes))*100)


# Follow-up question
# Summary of ¿Cuánto es el monto de la deuda vinculada a la plantación de cacao?
# we renamed the column as "monto_deuda"
non_numeric_values <- data$Q189_1[!grepl("^\\d+$", data$Q189_1)]
unique(non_numeric_values)
# Replace commas and convert to numeric
data$Q189_1_cleaned <- gsub(",", "", data$Q189_1)
data$monto_deuda <- as.numeric(data$Q189_1_cleaned)
sum(is.na(data$monto_deuda))

# Print summary of el monto deuda
print(summary(data$monto_deuda))




# Summary of ¿Cuál es el % interés? (Desplace la flecha hasta seleccionar su respuesta)
# we renamed the column as "interes_anual"
data$interes_anual <- as.numeric(as.character(data$Q190_2))
summary(data$interes_anual)




# Summary of ¿De cuánto es el plazo en años para pagar la deuda?
# we renamed the column as "plazo_deuda" en años
data$plazo_deuda <- as.numeric(as.character(data$Q191_3))
summary(data$plazo_deuda) #años





# Summary of Con qué entidad tiene el préstamo:
# The procedure for estimating summary statistics for this question is slightly different
# because the options are not mutually exclusive, meaning respondents could choose more than one option.
# Count responses for each option
# Convert character to numeric
data$Q192_1_numeric <- as.numeric(grepl("BAGRICOLA", data$Q192_1, ignore.case = TRUE))
data$Q192_2_numeric <- as.numeric(grepl("Casa exportadora", data$Q192_2, ignore.case = TRUE))
data$Q192_3_numeric <- as.numeric(grepl("Intermediario", data$Q192_3, ignore.case = TRUE))
data$Q192_4_numeric <- as.numeric(grepl("Cooperativas", data$Q192_4, ignore.case = TRUE))
data$Q192_5_numeric <- as.numeric(grepl("Bancos privados", data$Q192_5, ignore.case = TRUE))

# Calculate counts
bagricola_count <- sum(data$Q192_1_numeric, na.rm = TRUE)
casa_count <- sum(data$Q192_2_numeric, na.rm = TRUE)
intermediario_count <- sum(data$Q192_3_numeric, na.rm = TRUE)
coop_count <- sum(data$Q192_4_numeric, na.rm = TRUE)
privado_count <- sum(data$Q192_5_numeric, na.rm = TRUE)

# Calculate total number of respondents
# This might count only those who answered any of these questions
# Simplified search for testing
responded <- (!is.na(data$Q192_1) & grepl("BAGRICOLA", data$Q192_1, ignore.case = TRUE)) |
  (!is.na(data$Q192_2) & grepl("Casa exportadora", data$Q192_2, ignore.case = TRUE)) |
  (!is.na(data$Q192_3) & grepl("Intermediario", data$Q192_3, ignore.case = TRUE)) |
  (!is.na(data$Q192_4) & grepl("Cooperativas", data$Q192_4, ignore.case = TRUE)) |
  (!is.na(data$Q192_5) & grepl("Bancos privados", data$Q192_5, ignore.case = TRUE))

total_respondents <- sum(responded)

# Calculate percentages
bagricola_percent <- (bagricola_count / total_respondents) * 100
casa_percent <- (casa_count / total_respondents) * 100
intermediario_percent <- (intermediario_count / total_respondents) * 100
coop_percent <- (coop_count / total_respondents) * 100
privado_percent <- (privado_count / total_respondents) * 100

# Print results
print(paste("BAGRICOLA", bagricola_percent, "%"))
print(paste("Casa exportadora", casa_percent, "%"))
print(paste("Intermediario", intermediario_percent, "%"))
print(paste("Cooperativas", coop_percent, "%"))
print(paste("Bancos privados", privado_percent, "%"))




# Summary of ¿Cuál es el valor promedio por tarea de las fincas/terreno en los alrededores?
# we renamed the column as "valor_finca"
data <- data %>%
  rename(valor_finca = Q193)

print(table(data$valor_finca))
# Calculate proportions
print(prop.table(table(data$valor_finca))*100)




# Summary of Por favor, indique las herramientas que usa en la actividad productiva
print(table(data$Q194_1)) #Cosecha del cacao
print(table(data$Q194_2)) #Desmonte mazorcas enfermas
print(table(data$Q194_3)) #Poda del cacao 
print(table(data$Q194_4)) #Deschuponado 
print(table(data$Q194_5)) #Abonamiento o fertilización   
print(table(data$Q194_6)) #Aplicaciones de Insecticidas  
print(table(data$Q194_7)) #Aplicaciones de Fungicidas   
print(table(data$Q194_8)) #Deshierbe manual 
print(table(data$Q194_9)) #Resiembras
print(table(data$Q194_10))#Control de ratas
print(table(data$Q194_11))#Renovación de plantas viejas
print(table(data$Q194_12))#Rehabilitación de plantas viejas
print(table(data$Q194_13))#Deshije de musáceas 
print(table(data$Q194_14))#Deshoje de musáceas 
print(table(data$Q194_15))#Cosecha musáceas 
print(table(data$Q194_16))#Cosecha de otros frutales 
print(table(data$Q194_17))#Fertilización de frutales 
print(table(data$Q194_18))#Poda de árboles de sombra 
print(table(data$Q194_19))#Poda de árboles frutales 
print(table(data$Q194_20))#Poda de árboles maderables 




# Summary of ¿Tiene otra actividad agropecuaria, adicional a su sistema agroforestal de cacao?
# we create a dummy variable "otra_actividad" where; 1 = Yes; 0 = No.
data <- data %>%
  mutate(otra_actividad = ifelse(Q196 == "Si", 1, 0))

print(table(data$otra_actividad))
# Calculate proportions
print(prop.table(table(data$otra_actividad))*100)


# Follow-up question
# Summary of Seleccione las que aplican:
print(table(data$Q197_1)) #Agroturismo 
print(table(data$Q197_2)) #Ganadería
print(table(data$Q197_3)) #Avicultura 
print(table(data$Q197_4)) #Porcicultura
print(table(data$Q197_5)) #Aguacate   
print(table(data$Q197_6)) #Café  
print(table(data$Q197_7)) #Vegetales  
print(table(data$Q197_8)) #Cítricos 
print(table(data$Q197_9)) #Raíces y tubérculos 
print(table(data$Q197_10))#Arroz
print(table(data$Q197_11))#Palma aceitera
print(table(data$Q197_12))#Coco 
print(table(data$Q197_13))#Mango 
print(table(data$Q197_14))#Musáceas 
print(table(data$Q197_15))# Otros: 
print(table(data$Q197_15_TEXT))#Otros: 



# Summary of Seleccione las que aplican:
#Fuera de la finca, ¿Realiza usted y sus dependientes en el hogar otras actividades que
#generan ingresos fuera de la agricultura? (Ejemplo, empleado, comerciante, consultorías, etc.)






#------------------------------------------------------------------------------

# Conocimiento de la enfermedad moniliasis


