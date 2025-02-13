# 📌 Installare e caricare pacchetti necessari
list.of.packages <- c("ggplot2", "dplyr", "gridExtra", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cran.rstudio.com/")

library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)  # Per riorganizzare i dati in formato lungo

# 📌 Caricare i dataset
file_real <- "Phishing_Legitimate_full.csv"
file_synthetic <- "Corrected_Dataset.csv"

dataset_real <- read.csv(file_real)
dataset_synthetic <- read.csv(file_synthetic)

# 📌 Selezionare le feature di interesse
features_selected <- c("PctExtNullSelfRedirectHyperlinksRT", 
                       "FrequentDomainNameMismatch", 
                       "NumDash", 
                       "PctNullSelfRedirectHyperlinks", 
                       "PctExtHyperlinks", 
                       "PathLevel", 
                       "UrlLength", 
                       "HostnameLength", 
                       "EmbeddedBrandName")

# 📌 Creare un dataframe per il confronto
comparison_df <- data.frame(Feature = features_selected,
                            Valore_Reale = sapply(features_selected, function(col) mean(na.omit(dataset_real[[col]]))),
                            Valore_Sintetico = sapply(features_selected, function(col) mean(na.omit(dataset_synthetic[[col]]))))

# 📌 Convertire i dati in formato lungo per i grafici
comparison_melted <- melt(comparison_df, id.vars = "Feature", variable.name = "Tipo", value.name = "Valore")

# 📌 1️⃣ Barplot per confronto diretto tra dati reali e sintetici
barplot_comparison <- ggplot(comparison_melted, aes(x = Feature, y = Valore, fill = Tipo)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Confronto tra Dati Reali e Sintetici",
         x = "Feature", y = "Valore Medio") +
    scale_fill_manual(values = c("blue", "red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 📌 2️⃣ Scatterplot per confronto punto a punto
scatterplot_comparison <- ggplot(comparison_df, aes(x = Valore_Reale, y = Valore_Sintetico)) +
    geom_point(color = "blue", size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Scatterplot: Dati Reali vs Sintetici",
         x = "Valore Medio Reale", y = "Valore Medio Sintetico") +
    theme_minimal()

# 📌 Salvare i grafici in PDF
pdf("Confronto_Dati_Reali_vs_Sintetici.pdf", width = 12, height = 6)
grid.arrange(barplot_comparison, scatterplot_comparison, ncol = 2)
dev.off()

# 📌 Salvare i dati di confronto in CSV
write.csv(comparison_df, "Confronto_Valori_Reali_vs_Sintetici.csv", row.names = FALSE)

# 📌 Stampare i risultati numerici
print(comparison_df)
