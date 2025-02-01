library(ggplot2)
dataset <- read.csv("./Phishing_Legitimate_full.csv")

# Calcola i limiti degli assi
x_limits <- range(dataset$UrlLength, na.rm = TRUE)
y_limits <- range(hist(dataset$UrlLength, plot = FALSE)$counts, na.rm = TRUE)

# Istogramma con limiti degli assi specificati
hstr <- hist(dataset$UrlLength, 
             freq = TRUE, 
             main = "Distribuzione delle lunghezze degli URL", 
             col = 1:7, 
             xlab = "Lunghezza dell'URL", 
             ylab = "Frequenza assoluta",
             xlim = x_limits,
             ylim = c(0, y_limits[2]))

# La funzione hstr inoltre crea anche una lista di informazioni legati al grafico
cat("Istogramma relativa alla frequenza assoluta: \n")
print(str(hstr))
print(hstr$density)

# Kernel Density Plot per la lunghezza degli URL
# Kernel Density Plot
ggplot(dataset, aes(x = UrlLength)) + 
  geom_density(fill = "skyblue", alpha = 0.5) + 
  labs(title = "Kernel Density Plot della Lunghezza degli URL",
       x = "Lunghezza dell'URL", y = "Densità") +
  theme_minimal()

  ggplot(dataset, aes(x = NumQueryComponents)) + 
  geom_density(fill = "skyblue", alpha = 0.5) + 
  labs(title = "Kernel Density Plot per numero di componenti delle query ",
       x = "Numero Param", y = "Densità") +
  theme_minimal()

ggplot(dataset, aes(x = UrlLength, color = factor(dataset$`CLASS_LABEL`))) + 
  geom_density() +
  labs(title = "Kernel Density Plot per Classe", 
       x = "Lunghezza dell'URL", y = "Densità", color = "Classe") +
  theme_minimal()


  # Generazione di grafici di densità per vari kernel
kernels <- c("rectangular", "triangular", "epanechnikov", "cosine", "optcosine")

# Loop per ogni kernel
for (k in kernels) {
  dens <- density(dataset$UrlLength, kernel = k)  # Calcola la densità
  plot(dens, main = paste("Kernel Density Plot -", k), 
       xlab = "Lunghezza URL", 
       ylab = "Densità", 
       col = "blue", 
       lwd = 2)
  grid()
}


# Loop per ogni kernel
for (k in kernels) {
  dens <- density(dataset$NumQueryComponents, kernel = k)  # Calcola la densità
  plot(dens, main = paste("Kernel Density Plot -", k), 
       xlab = "Numero Param", 
       ylab = "Densità", 
       col = "blue", 
       lwd = 2)
  grid()
}

ggplot(dataset, aes(x = factor(dataset$`CLASS_LABEL`), y = UrlLength, fill = factor(dataset$`CLASS_LABEL`))) +
  geom_boxplot() +
  labs(title = "Boxplot della Lunghezza degli URL per Classe", 
       x = "Classe (Phishing/Legittimo)", 
       y = "Lunghezza degli URL") +
  theme_minimal()

# Boxplot logaritmico della lunghezza degli URL per classe
ggplot(dataset, aes(x = factor(CLASS_LABEL), y = log1p(UrlLength), fill = factor(CLASS_LABEL))) +
  geom_boxplot() +
  labs(title = "Boxplot Logaritmico della Lunghezza degli URL per Classe", 
       x = "Classe (Phishing/Legittimo)", 
       y = "Log(Lunghezza degli URL)") +
  theme_minimal()






# Creazione del boxplot ad intaglio
ggplot(dataset, aes(x = factor(CLASS_LABEL), y = UrlLength, fill = factor(CLASS_LABEL))) +
  geom_boxplot(varwidth = TRUE, notch = TRUE) +  # notch per l'intaglio
  labs(title = "Boxplot ad Intaglio della Lunghezza degli URL per Classe",
       x = "Classe (Phishing/Legittimo)", 
       y = "Lunghezza degli URL") +
  theme_minimal()


ggplot(dataset, aes(x = UrlLength, y = NumQueryComponents, color = factor(CLASS_LABEL))) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatterplot: Lunghezza degli URL vs Numero di Parametri",
       x = "Lunghezza degli URL",
       y = "Numero di Parametri nelle Query",
       color = "Classe (Phishing/Legittimo)") +
  theme_minimal()

ggplot(dataset, aes(x = log1p(UrlLength), y = log1p(NumQueryComponents), color = factor(CLASS_LABEL))) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatterplot (Scala Log): Lunghezza degli URL vs Numero di Parametri",
       x = "Log(Lunghezza degli URL)",
       y = "Log(Numero di Parametri)",
       color = "Classe (Phishing/Legittimo)") +
  theme_minimal()

ggplot(dataset, aes(x = UrlLength, y = NumQueryComponents, color = factor(CLASS_LABEL))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # Linea di regressione
  labs(title = "Scatterplot con Trend Line: Lunghezza degli URL vs Numero di Parametri",
       x = "Lunghezza degli URL",
       y = "Numero di Parametri nelle Query",
       color = "Classe (Phishing/Legittimo)") +
  theme_minimal()

ggplot(dataset, aes(x = UrlLength, y = NumQueryComponents)) +
  geom_point(alpha = 0.7, color = "blue") +
  facet_wrap(~ CLASS_LABEL, ncol = 2) +
  labs(title = "Scatter Plot per Classe: Lunghezza URL vs Numero di Parametri",
       x = "Lunghezza dell'URL", 
       y = "Numero di Parametri nelle Query") +
  theme_minimal()




# Creazione di intervalli di lunghezze degli URL
breaks <- c(0, 50, 100, 150, 200, max(dataset$UrlLength, na.rm = TRUE))
labels <- c("0-50", "51-100", "101-150", "151-200", ">200")
dataset$LengthCategory <- cut(dataset$UrlLength, breaks = breaks, labels = labels, right = FALSE)

# Tabella di frequenza per ciascuna categoria
length_category_counts <- table(dataset$LengthCategory)

# Ordinamento e calcolo proporzioni per ogni categoria
ord <- sort(length_category_counts, decreasing = TRUE)
propOrd <- prop.table(ord)

# Creazione del primo diagramma di Pareto (frequenza delle categorie)
x <- barplot(propOrd, 
             ylim = c(0, 1.05), 
             main = "Diagramma di Pareto per Categorie di Lunghezza degli URL", 
             col = 1:length(labels), 
             las = 2, 
             xlab = "Intervalli di Lunghezza", 
             ylab = "Proporzione")
lines(x, cumsum(propOrd), type = "b", pch = 16, col = "red")
text(x - 0.2, cumsum(propOrd) + 0.03, 
     paste(format(cumsum(propOrd) * 100, digits = 2), "%"),
     col = "blue")

# Diagrammi per ciascun intervallo di lunghezza
par(mfrow = c(2, 3))  # Finestra di output 2x3 per più grafici
for (category in levels(dataset$LengthCategory)) {
  # Filtra i dati per categoria
  subset_data <- subset(dataset, LengthCategory == category)
  
  # Frequenza dei valori per la categoria
  subset_counts <- table(subset_data$UrlLength)
  
  # Ordinamento e calcolo proporzioni
  subset_ord <- sort(subset_counts, decreasing = TRUE)
  subset_propOrd <- prop.table(subset_ord)
  
  # Diagramma di Pareto per la categoria
  barplot(subset_propOrd, 
          ylim = c(0, 1.05), 
          main = paste("Pareto - Categoria:", category), 
          col = rainbow(length(subset_ord)), 
          las = 2, 
          xlab = "Lunghezza URL", 
          ylab = "Proporzione")
  lines(cumsum(subset_propOrd), type = "b", pch = 16, col = "red")
}
par(mfrow = c(1, 1))  # Ripristina il layout grafico
