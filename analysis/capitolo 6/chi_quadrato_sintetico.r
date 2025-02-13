library(ggplot2)


data <- read.csv("/Users/leopoldotodisco/Desktop/progetto-SAD/Phishing-Legitimate-Analisys/codice_nuovo/capitolo 7/synthetic_dataset.csv")

summary(data$UrlLength)
data$UrlLength_class <- cut(data$UrlLength, 
                            breaks = c(19.9, 132, 248, 370, 499), 
                            labels = c("20-132", "133-248", "249-370", "371-499"),
                            right = TRUE)

# CHI QUADRATO
# H₀: la variabile segue una distribuzione uniforme.
# H₁: la variabile non segue una distribuzione uniforme.
  counts <- table(data$UrlLength_class)
  
  expected_counts <- rep(sum(counts) / length(counts), length(counts))
  
  chi_test <- chisq.test(counts, p = rep(1/length(counts), length(counts)))
  
  print(chi_test)
  
  if(chi_test$p.value > 0.05) {
    cat("\nConclusione: Non ci sono prove sufficienti per affermare che la distribuzione NON sia uniforme. \n")
    cat("Si accetta l'ipotesi alternativa: la variabile segue una distribuzione uniforme.\n")
  } else {
    cat("\nConclusione: Ci sono prove sufficienti per affermare che la distribuzione NON è uniforme. \n")
    cat("Si accetta l'ipotesi nulla: la variabile non segue una distribuzione uniforme.\n")
  }
  
# SENZA CURVA TEORICA
  ggplot(data, aes(x = UrlLength_class)) +
    geom_bar(fill = "lightblue", color = "black") +
    labs(title = "Distribuzione della variabile UrlLength ",
         x = "Classi di UrlLength",
         y = "Frequenza") +
    theme_minimal()


# CON TEORICA UNIFORME
# Calcolare la frequenza teorica uniforme
num_classi <- length(unique(data$UrlLength_class))  # Numero di classi
totale_osservazioni <- nrow(data)                   # Numero totale di osservazioni
frequenza_teorica <- totale_osservazioni / num_classi  # Frequenza teorica uniforme per classe

# Creare il grafico con la linea uniforme teorica
ggplot(data, aes(x = UrlLength_class)) +
  geom_bar(fill = "lightblue", color = "black") +  # Istogramma dei dati reali
  geom_hline(yintercept = frequenza_teorica,       # Linea uniforme teorica
             color = "red", 
             linetype = "dashed", 
             size = 1.2) +
  labs(title = "Distribuzione della variabile UrlLength ",
       x = "Classi di UrlLength",
       y = "Frequenza") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))    # Centrare il titolo
