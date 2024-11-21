library(ggplot2)
library(knitr)
library(kableExtra)

dataset <- read.csv("./Phishing_Legitimate_full.csv")

#Clustering gerarchico

# Visualizzazione del dendrogramma
# Campiona 30 osservazioni dal dataset per avere meno caos sull'asse delle X
set.seed(123)  # Per rendere il campionamento riproducibile
sampled_data <- dataset[sample(1:nrow(dataset), 30), ]

# Calcolo della matrice di distanza
distanceMatrix <- dist(sampled_data$UrlLength, method = "euclidean")

# Clustering gerarchico
tree <- hclust(distanceMatrix, method = "complete")

# Visualizzazione del dendrogramma
plot(tree, 
     main = "Dendrogramma Clustering Gerarchico (Campione)", 
     xlab = "Osservazioni (Campione)", 
     ylab = "Distanza", 
     sub = "Metodo: Complete Linkage, Distanza Euclidea")

# Screeplot
plot(tree$height, seq(length(tree$height), 1, by = -1), 
     type = "b", 
     main = "Screeplot", 
     xlab = "Distanza di aggregazione", 
     ylab = "Numero di cluster", 
     col = "red", 
     xaxt = "n")

# Aggiungi etichette personalizzate all'asse X
axis(1, at = seq(0, max(tree$height), by = max(tree$height) / 5), 
     labels = round(seq(0, max(tree$height), by = max(tree$height) / 5), 2))

# Numero di cluster consigliati dallo screeplot
k <- 3  # Puoi cambiare questo numero in base al tuo screeplot

# Disegna rettangoli sul dendrogramma per evidenziare i cluster
plot(tree, 
     main = "Dendrogramma con Cluster Evidenziati", 
     xlab = "Osservazioni", 
     ylab = "Distanza", 
     sub = "Metodo: Complete Linkage, Distanza Euclidea")
rect.hclust(tree, k = k, border = "red")  # Disegna rettangoli


#Calcolo delle misure di non omogeneità statistiche
#Calcolo trH
X1 <- data.frame(sampled_data$UrlLength)  # Considera la variabile campionata
rownames(X1) <- paste0("Obs_", 1:nrow(X1))  # Assegna nomi alle righe per chiarezza
# Calcolo della misura trH
trH <- (nrow(X1) - 1) * sum(apply(X1, 2, var))
# Stampa del risultato
cat("Misura di non omogeneità statistica (trH):", trH, "\n")

#Calcolo i trH per i singoli cluster
# Taglio del dendrogramma in 3 cluster
k <- 3  # Numero di cluster
taglio <- cutree(tree, k = k)  # Assegna ogni osservazione a un cluster
numTaglio <- table(taglio)  # Numero di osservazioni per cluster

# Calcolo della varianza intra-cluster
taglioList <- list(taglio)
agvar <- aggregate(sampled_data$UrlLength, taglioList, var)[, -1]  # Varianza per cluster

# Calcolo dei trH per ciascun cluster
trH1 <- (numTaglio[[1]] - 1) * sum(agvar[1])
trH2 <- (numTaglio[[2]] - 1) * sum(agvar[2])
trH3 <- (numTaglio[[3]] - 1) * sum(agvar[3])

# Somma dei trH intra-cluster
sumTrH <- trH1 + trH2 + trH3

# Calcolo del trH totale
X1 <- data.frame(sampled_data$UrlLength)
rownames(X1) <- paste0("Obs_", 1:nrow(X1))
trH <- (nrow(X1) - 1) * sum(apply(X1, 2, var))

# Calcolo del trB (eterogeneità tra cluster)
trB <- trH - sumTrH

# Rapporto trB / trH (percentuale di eterogeneità spiegata dai cluster)
rapporto <- trB / trH

# Stampa dei risultati
cat("\n")
cat("Misura trH totale:", trH, "\n")
cat("Somma trH intra-cluster:", sumTrH, "\n")
cat("Misura trB:", trB, "\n")
cat("Rapporto trB / trH (eterogeneità spiegata):", round(rapporto * 100, 2), "%\n")
cat("\n")


##DISTANZA DI CEBYCEV

# Calcolo della matrice di distanza
distanceMatrix <- dist(sampled_data$UrlLength, method = "maximum")

# Clustering gerarchico
tree <- hclust(distanceMatrix, method = "complete")

# Visualizzazione del dendrogramma
plot(tree, 
     main = "Dendrogramma Clustering Gerarchico (Campione) ", 
     xlab = "Osservazioni (Campione)", 
     ylab = "Distanza di cebycev", 
     sub = "Metodo: Complete Linkage, Distanza di Cebycev")

# Screeplot
plot(tree$height, seq(length(tree$height), 1, by = -1), 
     type = "b", 
     main = "Screeplot (distanza di Cebycev)", 
     xlab = "Distanza di aggregazione", 
     ylab = "Numero di cluster", 
     col = "red", 
     xaxt = "n")

# Aggiungi etichette personalizzate all'asse X
axis(1, at = seq(0, max(tree$height), by = max(tree$height) / 5), 
     labels = round(seq(0, max(tree$height), by = max(tree$height) / 5), 2))

# Numero di cluster consigliati dallo screeplot
k <- 3  # Puoi cambiare questo numero in base al tuo screeplot

# Disegna rettangoli sul dendrogramma per evidenziare i cluster
plot(tree, 
     main = "Dendrogramma con Cluster Evidenziati", 
     xlab = "Osservazioni", 
     ylab = "Distanza", 
     sub = "Metodo: Complete Linkage, Distanza di Cebycev")
rect.hclust(tree, k = k, border = "red")  # Disegna rettangoli


#Calcolo delle misure di non omogeneità statistiche
#Calcolo trH
X1 <- data.frame(sampled_data$UrlLength)  # Considera la variabile campionata
rownames(X1) <- paste0("Obs_", 1:nrow(X1))  # Assegna nomi alle righe per chiarezza
# Calcolo della misura trH
trH <- (nrow(X1) - 1) * sum(apply(X1, 2, var))
# Stampa del risultato
cat("Misura di non omogeneità statistica (trH):", trH, "\n")

#Calcolo i trH per i singoli cluster
# Taglio del dendrogramma in 3 cluster
k <- 3  # Numero di cluster
taglio <- cutree(tree, k = k)  # Assegna ogni osservazione a un cluster
numTaglio <- table(taglio)  # Numero di osservazioni per cluster

# Calcolo della varianza intra-cluster
taglioList <- list(taglio)
agvar <- aggregate(sampled_data$UrlLength, taglioList, var)[, -1]  # Varianza per cluster

# Calcolo dei trH per ciascun cluster
trH1 <- (numTaglio[[1]] - 1) * sum(agvar[1])
trH2 <- (numTaglio[[2]] - 1) * sum(agvar[2])
trH3 <- (numTaglio[[3]] - 1) * sum(agvar[3])

# Somma dei trH intra-cluster
sumTrH <- trH1 + trH2 + trH3

# Calcolo del trH totale
X1 <- data.frame(sampled_data$UrlLength)
rownames(X1) <- paste0("Obs_", 1:nrow(X1))
trH <- (nrow(X1) - 1) * sum(apply(X1, 2, var))

# Calcolo del trB (eterogeneità tra cluster)
trB <- trH - sumTrH

# Rapporto trB / trH (percentuale di eterogeneità spiegata dai cluster)
rapporto <- trB / trH

# Stampa dei risultati
cat("\n")
cat("Misura trH totale:", trH, "\n")
cat("Somma trH intra-cluster:", sumTrH, "\n")
cat("Misura trB:", trB, "\n")
cat("Rapporto trB / trH (eterogeneità spiegata):", round(rapporto * 100, 2), "%\n")
cat("\n")

## Distanza di Canberra

# Calcolo della matrice di distanza
distanceMatrix <- dist(sampled_data$UrlLength, method = "canberra")

# Clustering gerarchico
tree <- hclust(distanceMatrix, method = "complete")

# Visualizzazione del dendrogramma
plot(tree, 
     main = "Dendrogramma Clustering Gerarchico (Campione) ", 
     xlab = "Osservazioni (Campione)", 
     ylab = "Distanza di cebycev", 
     sub = "Metodo: Complete Linkage, Distanza Canberra")

# Screeplot
plot(tree$height, seq(length(tree$height), 1, by = -1), 
     type = "b", 
     main = "Screeplot (distanza di Canberra)", 
     xlab = "Distanza di aggregazione", 
     ylab = "Numero di cluster", 
     col = "red", 
     xaxt = "n")

# Aggiungi etichette personalizzate all'asse X
axis(1, at = seq(0, max(tree$height), by = max(tree$height) / 5), 
     labels = round(seq(0, max(tree$height), by = max(tree$height) / 5), 2))

# Numero di cluster consigliati dallo screeplot
k <- 3  # Puoi cambiare questo numero in base al tuo screeplot

# Disegna rettangoli sul dendrogramma per evidenziare i cluster
plot(tree, 
     main = "Dendrogramma con Cluster Evidenziati", 
     xlab = "Osservazioni", 
     ylab = "Distanza", 
     sub = "Metodo: Complete Linkage, Distanza di Canberra")
rect.hclust(tree, k = k, border = "red")  # Disegna rettangoli




## KMEANS

# Calcolo dell'inertia per diversi valori di k
set.seed(123)  # Per la riproducibilità
wss <- sapply(1:10, function(k) {
  kmeans(X1, centers = k, nstart = 10)$tot.withinss
})

# Grafico del metodo del gomito
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     main = "Metodo del Gomito",
     xlab = "Numero di Cluster (k)",
     ylab = "Somma totale delle deviazioni intra-cluster (WSS)")



# Metodo del k-means
# Usa il campione già selezionato
X1 <- data.frame(UrlLength = sampled_data$UrlLength)
rownames(X1) <- paste0("Obs_", 1:nrow(X1))  # Assegna nomi alle righe per chiarezza

# Esegui il clustering k-means con 3 cluster
set.seed(123)  # Per rendere il clustering riproducibile
km <- kmeans(X1, centers = 3, iter.max = 20, nstart = 1)

# Visualizza i risultati
cat("Cluster assegnati:\n", km$cluster, "\n")

# Plot dei cluster
plot(X1$UrlLength, col = km$cluster, pch = 19, 
     main = "Metodo non gerarchico del K-means",
     xlab = "Osservazioni", ylab = "UrlLength")
legend("topright", legend = unique(km$cluster), col = unique(km$cluster), pch = 19)


# Preparazione dei dati per il barplot
kmeansData <- data.frame(
  Osservazioni = paste0("Obs_", 1:nrow(sampled_data)),  # Nome delle osservazioni
  UrlLength = sampled_data$UrlLength,                 # Lunghezza degli URL
  Cluster = km$cluster                                # Cluster assegnati dal K-means
)

# Ordina i dati per lunghezza degli URL
kmeansData <- kmeansData[order(kmeansData$UrlLength), ]

# Definizione del grafico
par(mar = c(5, 6, 4, 1) + 1)  # Margini del grafico
options(scipen = 999)  # Disabilita la notazione scientifica

# Barplot
plotKMeans <- barplot(
  kmeansData$UrlLength, 
  names.arg = kmeansData$Osservazioni, 
  col = kmeansData$Cluster, 
  main = "Barplot dei Cluster Definiti dal Metodo K-means", 
  xlab = "", 
  ylab = "", 
  las = 2, 
  ylim = c(0, max(kmeansData$UrlLength) + 10),  # Adatta il limite superiore
  yaxt = "n"  # Disabilita temporaneamente le etichette sull'asse Y
)

# Etichette sull'asse Y
axis(2, at = seq(0, max(kmeansData$UrlLength), by = 20), 
     labels = seq(0, max(kmeansData$UrlLength), by = 20), las = 1)

# Aggiungi descrizioni agli assi
mtext("Lunghezza URL", side = 2, line = 5.3)
mtext("Osservazioni", side = 1, line = 4.5)

# Aggiungi etichette dei cluster sopra le barre
text(plotKMeans, kmeansData$UrlLength + 2, label = kmeansData$Cluster, cex = 0.8)



