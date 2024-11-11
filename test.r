library(ggplot2)

dataset <- read.csv("./Phishing_Legitimate_full.csv")
# Frequenza assoluta per CLASS_LABEL
freq_abs <- table(dataset$CLASS_LABEL)

# Frequenza relativa per CLASS_LABEL
freq_rel <- prop.table(freq_abs)

# Stampa le frequenze assolute e relative
print("Frequenza Assoluta:")
print(freq_abs)
print("Frequenza Relativa:")
print(freq_rel)

# Visualizzazione grafica della distribuzione di CLASS_LABEL
barplot(freq_abs, main = "Distribuzione di Phishing vs Legittimo",
        xlab = "Classe (0 = Legittimo, 1 = Phishing)", 
        ylab = "Frequenza", col = c("lightgreen", "lightcoral"),
        names.arg = c("Legittimo", "Phishing"))


# creaimo classi in base a se c'è o meno https nell'url e vediamo in correlazione come cambia la classe
# Filtra i dati e calcola i conteggi per ciascuna combinazione di classLabel e noHttps
counts <- table(dataset$CLASS_LABEL, dataset$NoHttps)

# Assegna nomi descrittivi ai conteggi per una migliore leggibilità
class1_noHttps0 <- counts["1", "0"]
class1_noHttps1 <- counts["1", "1"]
class0_noHttps0 <- counts["0", "0"]
class0_noHttps1 <- counts["0", "1"]

# Crea un data frame per i risultati
result <- data.frame(
  Category = c("Phishing - noHttps 0", "Phishing - noHttps 1",
               "Leggittimo - noHttps 0", "Leggittimo - noHttps 1"),
  Count = c(class1_noHttps0, class1_noHttps1, class0_noHttps0, class0_noHttps1)
)

# Stampa i risultati
print(result)

# Genera un grafico a barre con ggplot2
ggplot(result, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Variazione della classe in base alla dicitura Https",
       x = "Categoria",
       y = "Conteggio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_brewer(palette = "Set3")