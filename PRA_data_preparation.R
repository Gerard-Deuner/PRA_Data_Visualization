###########################
# Preparació de les Dades #
###########################

# Carregar llibreries
library(dplyr)
library(DescTools) 

# Llegir les dades
df <- read.table("data/Mental_Health_Lifestyle_Dataset.csv", sep = ",", header = TRUE, row.names = NULL)

# Mostrar les dades
head(df)

# Agrupar les edats en grups
df$AgeGroup <- cut(df$Age,
                   breaks = c(seq(0, 70, by=20), Inf),
                   labels = c("10-19", "20-39", "40-59", "60-69"),
                   right = FALSE)


# Comprovar el resultat
table(df$AgeGroup)

# Convertir Exercise.Level a numèrica ordinal
df$Exercise.Level <- factor(df$Exercise.Level,
                            levels = c("Low", "Medium", "High"),
                            ordered = TRUE)
df$Exercise.Level <- as.numeric(df$Exercise.Level)

# Guardar les dades processades 
write.table(df, "data/Mental_Health_Lifestyle_Dataset_Processed.csv", sep = ",", row.names = FALSE)
