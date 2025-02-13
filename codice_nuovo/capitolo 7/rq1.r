dataset_originale <- read.csv("/Users/leopoldotodisco/Desktop/progetto-SAD/Phishing-Legitimate-Analisys/codice_nuovo/Phishing_Legitimate_full.csv")
dataset_sintetico <- read.csv("/Users/leopoldotodisco/Desktop/progetto-SAD/Phishing-Legitimate-Analisys/codice_nuovo/capitolo 7/synthetic_dataset.csv")

#INDICI CENTRALI DATASET_ORIGINALE
# Funzione per calcolare la moda campionaria
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}
# Variabile: UrlLength
mean_url <- mean(dataset_originale$UrlLength, na.rm = TRUE)     # Media
median_url <- median(dataset_originale$UrlLength, na.rm = TRUE) # Mediana
mode_url <- get_mode(dataset_originale$UrlLength)               # Moda campionaria
cat("#################################################################")
cat("################## DATASET ORIGINALE ########################")
cat("UrlLength:\n")
cat("Media:", mean_url, "\n")
cat("Mediana:", median_url, "\n")
cat("Moda:", mode_url, "\n\n")

# Variabile: NumDash
mean_url <- mean(dataset_originale$NumDash, na.rm = TRUE)     # Media
median_url <- median(dataset_originale$NumDash, na.rm = TRUE) # Mediana
mode_url <- get_mode(dataset_originale$NumDash)               # Moda campionaria

cat("NumDash:\n")
cat("Media:", mean_url, "\n")
cat("Mediana:", median_url, "\n")
cat("Moda:", mode_url, "\n\n")

# Variabile: NumQueryComponents
mean_query <- mean(dataset_originale$NumQueryComponents, na.rm = TRUE)     # Media
median_query <- median(dataset_originale$NumQueryComponents, na.rm = TRUE) # Mediana
mode_query <- get_mode(dataset_originale$NumQueryComponents)               # Moda campionaria

cat("NumQueryComponents:\n")
cat("Media:", mean_query, "\n")
cat("Mediana:", median_query, "\n")
cat("Moda:", mode_query, "\n")

cat("\n")
cat("#################################################################")

cat("################## DATASET SINTETICO ########################")

#INDICI CENTRALI DATASET_SINTETICO
# Funzione per calcolare la moda campionaria
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}
# Variabile: UrlLength
mean_url <- mean(dataset_sintetico$UrlLength, na.rm = TRUE)     # Media
median_url <- median(dataset_sintetico$UrlLength, na.rm = TRUE) # Mediana
mode_url <- get_mode(dataset_sintetico$UrlLength)               # Moda campionaria
cat("\nUrlLength:\n")
cat("Media:", mean_url, "\n")
cat("Mediana:", median_url, "\n")
cat("Moda:", mode_url, "\n\n")

# Variabile: NumDash
mean_url <- mean(dataset_sintetico$NumDash, na.rm = TRUE)     # Media
median_url <- median(dataset_sintetico$NumDash, na.rm = TRUE) # Mediana
mode_url <- get_mode(dataset_sintetico$NumDash)               # Moda campionaria

cat("NumDash:\n")
cat("Media:", mean_url, "\n")
cat("Mediana:", median_url, "\n")
cat("Moda:", mode_url, "\n\n")

# Variabile: NumQueryComponents
mean_query <- mean(dataset_sintetico$NumQueryComponents, na.rm = TRUE)     # Media
median_query <- median(dataset_sintetico$NumQueryComponents, na.rm = TRUE) # Mediana
mode_query <- get_mode(dataset_sintetico$NumQueryComponents)               # Moda campionaria

cat("NumQueryComponents:\n")
cat("Media:", mean_query, "\n")
cat("Mediana:", median_query, "\n")
cat("Moda:", mode_query, "\n")