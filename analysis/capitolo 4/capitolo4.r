# Caricare i pacchetti necessari
library(ggplot2)
install.packages("ggcorrplot", repos="https://cran.rstudio.com/")
library(ggcorrplot)

library(reshape2)


file_path <- "Phishing_Legitimate_full.csv"
dataset <- read.csv(file_path)

features <- c("PctExtNullSelfRedirectHyperlinksRT", 
              "FrequentDomainNameMismatch", 
              "NumDash", 
              "SubmitInfoToEmail", 
              "PctNullSelfRedirectHyperlinks")

results <- data.frame(Feature = character(),
                      Covarianza = numeric(),
                      Correlazione = numeric(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

for (feature in features) {

  if (!(feature %in% names(dataset))) {
    print(paste("Feature", feature, "non trovata nel dataset."))
    next
  }


  cov_value <- cov(dataset[[feature]], dataset$CLASS_LABEL)


  cor_value <- cor(dataset[[feature]], dataset$CLASS_LABEL, method = "pearson")

  
  test_cor <- cor.test(dataset[[feature]], dataset$CLASS_LABEL, method = "pearson")
  p_val <- test_cor$p.value

  
  results <- rbind(results, data.frame(Feature = feature, 
                                       Covarianza = cov_value, 
                                       Correlazione = cor_value,
                                       p_value = p_val))
  
  
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


print(results)


write.csv(results, "Risultati_Covarianza_Correlazione.csv", row.names = FALSE)

cor_matrix <- cor(dataset[, features], method = "pearson")


print(cor_matrix)


write.csv(cor_matrix, "Matrice_Correlazione.csv", row.names = TRUE)


features <- c("PctExtNullSelfRedirectHyperlinksRT", 
              "FrequentDomainNameMismatch", 
              "NumDash", 
              "SubmitInfoToEmail", 
              "PctNullSelfRedirectHyperlinks", 
              "CLASS_LABEL")


cor_matrix <- cor(dataset[, features], method = "pearson")


pdf("Matrice_Correlazione_Heatmap.pdf", width = 8, height = 6)
ggcorrplot(cor_matrix, 
           method = "square",  
           type = "full", 
           lab = TRUE,  
           lab_size = 3,  
           colors = c("#D73027", "white", "#4575B4"),
           title = "Matrice di Correlazione tra le Variabili", 
           ggtheme = theme_minimal()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  
dev.off()