
library(ggplot2)
library(reshape2)
library(corrplot)
library(ggcorrplot)

x <- read.csv("/Users/leopoldotodisco/Desktop/progetto-SAD/Phishing-Legitimate-Analisys/codice_nuovo/Phishing_Legitimate_full.csv")
dataset <- read.csv("/Users/leopoldotodisco/Desktop/progetto-SAD/Phishing-Legitimate-Analisys/codice_nuovo/capitolo 7/synthetic_dataset.csv")

# Definizione delle feature da analizzare
features <- c("PctExtNullSelfRedirectHyperlinksRT", 
              "FrequentDomainNameMismatch", 
              "NumDash", 
              "SubmitInfoToEmail", 
              "PctNullSelfRedirectHyperlinks")

# Creare un dataframe per i risultati
results <- data.frame(Feature = character(),
                      Covarianza = numeric(),
                      Correlazione = numeric(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

# Loop sulle feature per calcolare statistiche
for (feature in features) {
  # Verificare che la feature esista nel dataset
  if (!(feature %in% names(dataset))) {
    print(paste("Feature", feature, "non trovata nel dataset."))
    next
  }

  # Calcolo della Covarianza Campionaria
  cov_value <- cov(dataset[[feature]], dataset$CLASS_LABEL)

  # Calcolo del Coefficiente di Correlazione (Pearson)
  cor_value <- cor(dataset[[feature]], dataset$CLASS_LABEL, method = "pearson")

  # Test di significatività statistica
  test_cor <- cor.test(dataset[[feature]], dataset$CLASS_LABEL, method = "pearson")
  p_val <- test_cor$p.value

  # Salvare i risultati
  results <- rbind(results, data.frame(Feature = feature, 
                                       Covarianza = cov_value, 
                                       Correlazione = cor_value,
                                       p_value = p_val))
  
  # Creare scatterplot per ogni feature rispetto a CLASS_LABEL e salvare il file correttamente
  pdf_file <- paste0("Scatterplot_", feature, "_vs_LABEL.pdf")
  pdf(pdf_file, width = 8, height = 6)
  print(
    ggplot(dataset, aes(x = dataset[[feature]], y = factor(CLASS_LABEL))) +
      geom_jitter(alpha = 0.3, color = "blue", width = 0.1) +
      labs(title = paste("Scatterplot tra", feature, "e CLASS_LABEL"),
           x = feature,
           y = "CLASS_LABEL (0 = Legittimo, 1 = Phishing)") +
      theme_classic()
  )
  dev.off()
}

# Stampare i risultati
print(results)

# Salvare i risultati in un CSV
write.csv(results, "Risultati_Covarianza_Correlazione.csv", row.names = FALSE)

cor_matrix <- cor(dataset[, features], method = "pearson")

# Stampare la matrice di correlazione
print(cor_matrix)

# Salvare la matrice di correlazione in un file CSV
write.csv(cor_matrix, "Matrice_Correlazione.csv", row.names = TRUE)

# Selezione delle feature di interesse
features <- c("PctExtNullSelfRedirectHyperlinksRT", 
              "FrequentDomainNameMismatch", 
              "NumDash", 
              "SubmitInfoToEmail", 
              "PctNullSelfRedirectHyperlinks", 
              "CLASS_LABEL")

# Creare la matrice di correlazione
cor_matrix <- cor(dataset[, features], method = "pearson")

# Creare la heatmap della matrice di correlazione con ggcorrplot
pdf("synthetic_Matrice_Correlazione_Heatmap.pdf", width = 8, height = 6)
ggcorrplot(cor_matrix, 
           method = "square",  # Formato quadrato per migliore leggibilità
           type = "full",  # Mantiene l'intera matrice visibile
           lab = TRUE,  # Mostra i valori numerici nelle celle
           lab_size = 3,  
           colors = c("#D73027", "white", "#4575B4"),  # Rosso-Bianco-Blu (come l'originale)
           title = "Matrice di Correlazione tra le Variabili", 
           ggtheme = theme_minimal()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  # Ruota le etichette per migliorare leggibilità
dev.off()