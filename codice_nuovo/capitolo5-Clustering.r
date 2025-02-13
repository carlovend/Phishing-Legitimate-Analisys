# 📌 Caricare le librerie necessarie
library(ggplot2)
library(factoextra)
library(knitr)        # Per kable()
library(kableExtra)   # Per la formattazione della tabella
library(dendextend)
library(ggplot2)
library(cluster)
library(dendextend)
library(dplyr)

library(gridExtra)
dataset <- read.csv("./Phishing_Legitimate_full.csv")

# 📌 Selezione delle feature scelte per il clustering
features <- c("PctExtNullSelfRedirectHyperlinksRT", 
              "FrequentDomainNameMismatch", 
              "UrlLength", 
              "SubmitInfoToEmail", 
              "PctNullSelfRedirectHyperlinks", 
              "PctExtHyperlinks", 
              "InsecureForms", 
              "IframeOrFrame", 
              "PathLevel", 
              "AbnormalExtFormActionR", 
              "UrlLength", 
              "HostnameLength", 
              "NumQueryComponents", 
              "EmbeddedBrandName")

df_cluster <- dataset[, features]

# 📌 Standardizzazione delle variabili
df_scaled <- scale(df_cluster)

# 📌 Applicazione della PCA
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# 📌 Visualizzazione della varianza spiegata (Grafico Scree Plot)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))

# 📌 Analisi del contributo di ciascuna variabile alle prime componenti principali
fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE)

# 📌 Creiamo una tabella con il contributo delle variabili alle prime 3 componenti
cor_matrix <- as.data.frame(pca_result$rotation[, 1:3])  # Prime 3 componenti
cor_matrix$Feature <- rownames(cor_matrix)

# 📌 Visualizzazione della tabella con il contributo delle variabili
kable(cor_matrix, caption = "Contributo delle Variabili alle Prime 3 Componenti") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Variabili Scelte per il Clustering
# PctExtNullSelfRedirectHyperlinksRT
# FrequentDomainNameMismatch
#UrlLength
# PctNullSelfRedirectHyperlinks
# PctExtHyperlinks
# PathLevel
#UrlLength
# HostnameLength
# EmbeddedBrandName


# 📌 Selezione delle feature per il clustering
features_selected <- c("PctExtNullSelfRedirectHyperlinksRT", 
                       "FrequentDomainNameMismatch", 
                       "UrlLength", 
                       "PctNullSelfRedirectHyperlinks", 
                       "PctExtHyperlinks", 
                       "PathLevel", 
                       "UrlLength", 
                       "HostnameLength", 
                       "EmbeddedBrandName")

df_cluster <- dataset[, features_selected]

# 📌 Selezione delle feature per il clustering
features_selected <- c("PctExtNullSelfRedirectHyperlinksRT", 
                       "FrequentDomainNameMismatch", 
                       "NumDash", 
                       "PctNullSelfRedirectHyperlinks", 
                       "PctExtHyperlinks", 
                       "PathLevel", 
                       "UrlLength", 
                       "HostnameLength", 
                       "EmbeddedBrandName")

df_cluster <- dataset[, features_selected]

# 📌 Creiamo classi di UrlLength
df_cluster <- df_cluster %>%
  mutate(UrlLength_Class = cut(UrlLength, breaks = c(0, 40, 60, 80, 100, 200, Inf),
                               labels = c("0-40", "40-60", "60-80", "80-100", "100-200", "200+")))

# 📌 Aggregazione: Calcoliamo la media delle feature per ogni classe di UrlLength
df_cluster_grouped <- df_cluster %>%
  group_by(UrlLength_Class) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# 📌 Standardizziamo le feature dopo l'aggregazione
df_scaled <- scale(df_cluster_grouped[, -1])  # Escludiamo la colonna UrlLength_Class

# 📌 Calcolo della matrice di distanza
# usiamo manhattan perchè: 🔹 Manhattan: Riduce l’effetto degli outlier e tratta meglio le distribuzioni asimmetriche.
dist_matrix <- dist(df_scaled, method = "manhattan")

# 📌 Clustering Gerarchico con metodo di Ward
hc <- hclust(dist_matrix, method = "ward.D2")

# 📌 Modifichiamo l'asse X con le classi di UrlLength
hc$labels <- df_cluster_grouped$UrlLength_Class  # Usiamo le classi al posto degli individui

# 📌 Salvataggio del dendrogramma aggiornato
pdf("dendrogramma_aggregato.pdf", width = 10, height = 6)
plot(hc, main = "Dendrogramma Aggregato per Classi di UrlLength", 
     xlab = "Classi di UrlLength", ylab = "Distanza (Manhattan)", 
     cex = 0.9, hang = -1)
dev.off()

# 📌 Dendrogramma con Cluster Colorati
pdf("dendrogramma_colori_aggregato.pdf", width = 10, height = 6)
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 3)  # Cambia il numero di cluster se necessario
plot(dend, main = "Dendrogramma con Cluster Colorati - Classi di UrlLength")
dev.off()


# 📌 Selezione delle feature per il clustering
features_selected <- c("PctExtNullSelfRedirectHyperlinksRT", 
                       "FrequentDomainNameMismatch", 
                       "UrlLength", 
                       "PctNullSelfRedirectHyperlinks", 
                       "PctExtHyperlinks", 
                       "PathLevel", 
                       "UrlLength", 
                       "HostnameLength", 
                       "EmbeddedBrandName")

df_cluster <- dataset[, features_selected]

# 📌 Selezione delle feature per il clustering
features_selected <- c("PctExtNullSelfRedirectHyperlinksRT", 
                       "FrequentDomainNameMismatch", 
                       "NumDash", 
                       "PctNullSelfRedirectHyperlinks", 
                       "PctExtHyperlinks", 
                       "PathLevel", 
                       "UrlLength", 
                       "HostnameLength", 
                       "EmbeddedBrandName")

df_cluster <- dataset[, features_selected]

# 📌 Creiamo classi di UrlLength
df_cluster <- df_cluster %>%
  mutate(UrlLength_Class = cut(UrlLength, breaks = c(0, 40, 60, 80, 100, 200, Inf),
                               labels = c("0-40", "40-60", "60-80", "80-100", "100-200", "200+")))

# 📌 Aggregazione: Calcoliamo la media delle feature per ogni classe di UrlLength
df_cluster_grouped <- df_cluster %>%
  group_by(UrlLength_Class) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# 📌 Standardizziamo le feature dopo l'aggregazione
df_scaled <- scale(df_cluster_grouped[, -1])  # Escludiamo la colonna UrlLength_Class

# 📌 Calcolo della matrice di distanza
# usiamo manhattan perchè: 
dist_matrix <- dist(df_scaled, method = "canberra")

# 📌 Clustering Gerarchico con metodo di Ward
hc <- hclust(dist_matrix, method = "ward.D2")

# 📌 Modifichiamo l'asse X con le classi di UrlLength
hc$labels <- df_cluster_grouped$UrlLength_Class  # Usiamo le classi al posto degli individui

# 📌 Salvataggio del dendrogramma aggiornato
pdf("canberra_dendrogramma_aggregato.pdf", width = 10, height = 6)
plot(hc, main = "Dendrogramma Aggregato per Classi di UrlLength", 
     xlab = "Classi di UrlLength", ylab = "Distanza (canberra)", 
     cex = 0.9, hang = -1)
dev.off()

# 📌 Dendrogramma con Cluster Colorati
pdf("canberra_dendrogramma_colori_aggregato.pdf", width = 10, height = 6)
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 3)  # Cambia il numero di cluster se necessario
plot(dend, main = "Dendrogramma con Cluster Colorati - Classi di UrlLength")
dev.off()

# 📌 Caricare le librerie necessarie
library(ggplot2)
library(factoextra)

# 📌 Selezione delle feature finali per il clustering
features_selected <- c("PctExtNullSelfRedirectHyperlinksRT", 
                       "FrequentDomainNameMismatch", 
                       "NumDash", 
                       "PctNullSelfRedirectHyperlinks", 
                       "PctExtHyperlinks", 
                       "PathLevel", 
                       "UrlLength", 
                       "HostnameLength", 
                       "EmbeddedBrandName")

df_cluster <- dataset[, features_selected]
# 📌 Standardizzazione delle feature
df_scaled <- scale(df_cluster)

library(cluster)
library(factoextra)

# 📌 Creazione della matrice di distanza (usa Manhattan o Canberra)
dist_matrix <- dist(df_cluster, method = "manhattan")  # Oppure "canberra"

# 📌 Determinare il miglior K usando il Metodo del Gomito
wss_values <- numeric(10)  # Inizializziamo un vettore per salvare i costi per K=1:10
for (k in 2:10) {
  pam_result <- pam(dist_matrix, k = k)
  wss_values[k] <- pam_result$objective[2]  # L'elemento [2] è la somma delle dissimilarità
}

# 📌 Creare il plot del Metodo del Gomito
pdf("screeplot_pam.pdf", width = 7, height = 5)
plot(2:10, wss_values[2:10], type = "b", pch = 19, frame = FALSE,
     xlab = "Numero di Cluster K",
     ylab = "Costo totale (Sum of Dissimilarities)",
     main = "Scree Plot: Metodo del Gomito per PAM (K-Medoids)")
dev.off()
# NOTIAMO CHE IL MIGLIOR K è 7 quando non è scalato, altrimenti è 6
# 📌 Selezione delle feature finali per il clustering
features_selected <- c("PctExtNullSelfRedirectHyperlinksRT", 
                       "FrequentDomainNameMismatch", 
                       "NumDash", 
                       "PctNullSelfRedirectHyperlinks", 
                       "PctExtHyperlinks", 
                       "PathLevel", 
                       "UrlLength", 
                       "HostnameLength", 
                       "EmbeddedBrandName")

df_cluster <- dataset[, features_selected]
# a
library(cluster)
library(factoextra)
library(gridExtra)

# 📌 Calcola la matrice di distanza usando la distanza di Canberra
dist_matrix <- dist(df_cluster, method = "canberra")

# 📌 Clustering con PAM (K-Medoids) usando distanza di Canberra
set.seed(42)
pam_result <- pam(dist_matrix, k = 10)

# 📌 Aggiunta delle etichette dei cluster al dataset
dataset$Cluster_PAM_Canberra <- pam_result$clustering

# 📌 Visualizzazione dei cluster in 2D (usando PCA per ridurre le dimensioni)
pdf("pam_canberra_clusters.pdf", width = 7, height = 5)
fviz_cluster(list(data = df_cluster, cluster = pam_result$clustering), geom = "point") +
  ggtitle("Clustering PAM (K-Medoids) con distanza Canberra")
dev.off()

# 📌 Confronto tra i cluster e la classe target (phishing vs legittimo)
print(table(dataset$Cluster_PAM_Canberra, dataset$CLASS_LABEL))

# 📌 Analisi delle medie delle feature per ciascun cluster
cluster_summary <- aggregate(df_cluster, by = list(Cluster = dataset$Cluster_PAM_Canberra), FUN = mean)

# 📌 Creazione dell'immagine della tabella con gridExtra
png("pam_canberra_summary.png", width = 4000, height = 550, res = 150)
grid.table(cluster_summary)
dev.off()

# 📌 Ripetiamo il clustering con distanza di Manhattan
dist_matrix_manhattan <- dist(df_cluster, method = "manhattan")

set.seed(42)
pam_result_manhattan <- pam(dist_matrix_manhattan, k = 7)

# 📌 Aggiunta delle etichette al dataset
dataset$Cluster_PAM_Manhattan <- pam_result_manhattan$clustering

# 📌 Visualizzazione dei cluster in 2D (PCA)
pdf("pam_manhattan_clusters.pdf", width = 7, height = 5)
fviz_cluster(list(data = df_cluster, cluster = pam_result_manhattan$clustering), geom = "point") +
  ggtitle("Clustering PAM (K-Medoids) con distanza Manhattan")
dev.off()

# 📌 Confronto tra i cluster e la classe target
print(table(dataset$Cluster_PAM_Manhattan, dataset$CLASS_LABEL))

# 📌 Analisi delle medie delle feature per ciascun cluster con distanza Manhattan
cluster_summary_manhattan <- aggregate(df_cluster, by = list(Cluster = dataset$Cluster_PAM_Manhattan), FUN = mean)

# 📌 Creazione dell'immagine della tabella con gridExtra
png("pam_manhattan_summary.png", width = 4000, height = 550, res = 150)
grid.table(cluster_summary_manhattan)
dev.off()

# 📌 Visualizzare il contributo delle variabili alle componenti principali (PCA)
pca_result <- prcomp(df_scaled)

# 📌 Grafico del contributo delle variabili alle componenti principali
pdf("pca_variable_contributions.pdf", width = 7, height = 5)
fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE)
dev.off()