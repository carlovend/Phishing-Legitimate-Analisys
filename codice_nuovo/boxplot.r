# 📌 Installare e caricare i pacchetti necessari
list.of.packages <- c("ggplot2", "dplyr", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cran.rstudio.com/")

library(ggplot2)
library(dplyr)
library(reshape2)

# 📌 Caricare il dataset
file_path <- "Phishing_Legitimate_full.csv"
dataset <- read.csv(file_path)

# 📌 Selezionare le feature e la variabile target
features_selected <- c("PctNullSelfRedirectHyperlinks", "PctExtHyperlinks", 
                        "UrlLength", "HostnameLength", "PathLevel", "NumDash", 
                        "FrequentDomainNameMismatch", "CLASS_LABEL")

data_selected <- dataset[, features_selected]

# 📌 Gestire i valori mancanti
data_selected <- na.omit(data_selected)

# 📌 Convertire la variabile target in fattore
data_selected$CLASS_LABEL <- factor(data_selected$CLASS_LABEL, 
                                    levels = c(0, 1), 
                                    labels = c("Legittimo", "Phishing"))

# 📌 1. Boxplot delle Feature Continue Divisi per Classe
continuous_features <- c("PctNullSelfRedirectHyperlinks", "PctExtHyperlinks", 
                         "UrlLength", "HostnameLength", "PathLevel", "NumDash")

pdf("Boxplot_Feature_Continue.pdf", width = 10, height = 8)
for (feature in continuous_features) {
  p <- ggplot(data_selected, aes_string(x = "CLASS_LABEL", y = feature, fill = "CLASS_LABEL")) +
    geom_boxplot() +
    labs(title = paste("Boxplot di", feature, "per Classe"), 
         x = "Classe", 
         y = feature) +
    theme_minimal()
  print(p)
}
dev.off()

cat("✅ Boxplot delle feature continue salvati in 'Boxplot_Feature_Continue.pdf'.\n")

# 📌 2. Boxplot per il Confronto di Feature Importanti
important_features <- c("PctNullSelfRedirectHyperlinks", 
                        "FrequentDomainNameMismatch", 
                        "HostnameLength")

pdf("Boxplot_Feature_Importanti.pdf", width = 10, height = 8)
for (feature in important_features) {
  p <- ggplot(data_selected, aes_string(x = "CLASS_LABEL", y = feature, fill = "CLASS_LABEL")) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
    labs(title = paste("Distribuzione di", feature, "per Classe"), 
         x = "Classe", 
         y = feature) +
    theme_minimal()
  print(p)
}
dev.off()

cat("✅ Boxplot delle feature importanti salvati in 'Boxplot_Feature_Importanti.pdf'.\n")

# 📌 3. Boxplot Multipli in un Unico Grafico
melted_data <- melt(data_selected, id.vars = "CLASS_LABEL", 
                    measure.vars = continuous_features)

pdf("Boxplot_Multipli.pdf", width = 12, height = 8)
ggplot(melted_data, aes(x = variable, y = value, fill = CLASS_LABEL)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribuzione delle Variabili per Classe", 
       x = "Variabili", 
       y = "Valore") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

cat("✅ Boxplot multipli salvati in 'Boxplot_Multipli.pdf'.\n")
