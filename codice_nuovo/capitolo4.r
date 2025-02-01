library(ggplot2)
library(knitr)
library(kableExtra)


dataset <- read.csv("./Phishing_Legitimate_full.csv")

# Funzione di distribuzione empirica continua per NumDash#Calcoliamo la frequenza assoluta della variabile rispetto alle classi
# Suddivisione in classi per la variabile 'NumDash'
classi <- seq(0, 55, by=5)  # Puoi adattare i valori in base alla distribuzione dei dati

# Calcolo della frequenza assoluta rispetto alle classi
num_dash_fa <- table(cut(dataset$NumDash, breaks = classi, right = FALSE))

# Calcolo della frequenza relativa
num_dash_fr <- num_dash_fa / length(dataset$NumDash)

# Calcolo della somma cumulativa della frequenza relativa
Fcum <- cumsum(num_dash_fr)

# Aggiungi un valore iniziale a Fcum per allinearlo con classi
ordinata <- c(0, Fcum)

# Funzione che crea il grafico
plot(classi, ordinata, type = "b", axes = FALSE, 
     main = "Funzione di Distribuzione Empirica Continua 'NumDash'",
     col = "blue", ylim = c(0, 1), xlab = "Numero di -", ylab = "Frequenza Cumulativa")

# Indica l'ascissa
axis(1, at = classi)
# Indica l'ordinata
axis(2, at = seq(0, 1, by = 0.1), labels = format(seq(0, 1, by = 0.1), digits = 2))
box()

# Suddivisione in classi per la variabile 'NumQueryComponents'
classi_query <- classi <- c(seq(0, 20, by=2), 23)  # Ultimo valore chiuso a 23

# Calcolo della frequenza assoluta rispetto alle classi
query_fa <- table(cut(dataset$NumQueryComponents, breaks = classi_query, right = FALSE))

# Calcolo della frequenza relativa
query_fr <- query_fa / length(dataset$NumQueryComponents)

# Calcolo della somma cumulativa della frequenza relativa
Fcum_query <- cumsum(query_fr)

# Aggiungi un valore iniziale a Fcum_query per allinearlo con classi
ordinata_query <- c(0, Fcum_query)

# Funzione che crea il grafico
plot(classi_query, ordinata_query, type = "b", axes = FALSE, 
     main = "Funzione di Distribuzione Empirica Continua 'NumQueryComponents'",
     col = "green", ylim = c(0, 1), xlab = "Numero di Parametri Query", ylab = "Frequenza Cumulativa")

# Indica l'ascissa
axis(1, at = classi_query)

# Indica l'ordinata
axis(2, at = seq(0, 1, by = 0.1), labels = format(seq(0, 1, by = 0.1), digits = 2))
box()



## INDICI DI SINTESI

#INDICI CENTRALI
# Funzione per calcolare la moda campionaria
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}
# Variabile: UrlLength
mean_url <- mean(dataset$UrlLength, na.rm = TRUE)     # Media
median_url <- median(dataset$UrlLength, na.rm = TRUE) # Mediana
mode_url <- get_mode(dataset$UrlLength)               # Moda campionaria

cat("UrlLength:\n")
cat("Media:", mean_url, "\n")
cat("Mediana:", median_url, "\n")
cat("Moda:", mode_url, "\n\n")

# Variabile: NumDash
mean_url <- mean(dataset$NumDash, na.rm = TRUE)     # Media
median_url <- median(dataset$NumDash, na.rm = TRUE) # Mediana
mode_url <- get_mode(dataset$NumDash)               # Moda campionaria

cat("NumDash:\n")
cat("Media:", mean_url, "\n")
cat("Mediana:", median_url, "\n")
cat("Moda:", mode_url, "\n\n")

# Variabile: NumQueryComponents
mean_query <- mean(dataset$NumQueryComponents, na.rm = TRUE)     # Media
median_query <- median(dataset$NumQueryComponents, na.rm = TRUE) # Mediana
mode_query <- get_mode(dataset$NumQueryComponents)               # Moda campionaria

cat("NumQueryComponents:\n")
cat("Media:", mean_query, "\n")
cat("Mediana:", median_query, "\n")
cat("Moda:", mode_query, "\n")

cat("\n")
# Calcoli per NumDash
var_url <- var(dataset$NumDash, na.rm = TRUE)
sd_url <- sd(dataset$NumDash, na.rm = TRUE)
cv_url <- (sd_url / mean(dataset$NumDash, na.rm = TRUE)) * 100

# Calcoli per NumQueryComponents
var_query <- var(dataset$NumQueryComponents, na.rm = TRUE)
sd_query <- sd(dataset$NumQueryComponents, na.rm = TRUE)
cv_query <- (sd_query / mean(dataset$NumQueryComponents, na.rm = TRUE)) * 100


# Creazione della tabella
stats_table <- data.frame(
  Variabile = c("NumDash", "NumQueryComponents", "UrlLength"),
  Varianza = c(var(dataset$NumDash, na.rm = TRUE), 
              var(dataset$NumQueryComponents, na.rm = TRUE),
              var(dataset$UrlLength, na.rm = TRUE)
              ),
  `Deviazione Standard` = c(sd(dataset$NumDash, na.rm = TRUE), 
                            sd(dataset$NumQueryComponents, na.rm = TRUE),
                            sd(dataset$UrlLength, na.rm = TRUE)
                            ),
  `Coefficiente di Variazione (%)` = c(
    (sd(dataset$NumDash, na.rm = TRUE) / mean(dataset$NumDash, na.rm = TRUE)) * 100,
    (sd(dataset$NumQueryComponents, na.rm = TRUE) / mean(dataset$NumQueryComponents, na.rm = TRUE)) * 100,
    (sd(dataset$UrlLength, na.rm = TRUE) / mean(dataset$UrlLength, na.rm = TRUE)) * 100
  )
)

# Stampa tabella formattata
stats_table %>%
  kbl(digits = 2, caption = "Statistiche Riepilogative") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))


#INDICI DI SKEWNESS E CURTOSI
skw <-function (x){
    n<-length (x)
    m2 <-(n -1)*var(x)/n
    m3 <- (sum( (x-mean(x))^3))/n
    m3/(m2^1.5)
}
#Calcolo della skewness per UNum dash
cat("Skewness: \n")
cat("Skewness NumDash: \n")
print(skw(dataset$`NumDash`))

#Calcolo della skewness per num query comp
cat("Skewness: \n")
cat("Skewness NumQueryParams: \n")
print(skw(dataset$`NumQueryComponents`))

#Calcolo della skewness per url length
cat("Skewness: \n")
cat("Skewness URL LENGTH: \n")
print(skw(dataset$`UrlLength`))


#L'indice di curtosi permette di calcolare la densità dei dati intorno alla media campionaria. 
# In particolare riusciamo a calcolare quanto la funzione sia più o meno piccata rispetto ad una 
# funzione normale standard
curt <- function (x) {
n <- length (x )
m2 <-(n -1) * var ( x)/ n
m4 <- ( sum ( (x - mean (x )) ^4) ) /n
m4 / (m2 ^2) -3
}

#Calcolo della curtosi
cat("Curtosi: \n")
cat("NumDash: \n")
print(curt(dataset$`NumDash`))

cat("Curtosi: \n")
cat("NumQueryComponents: \n")
print(curt(dataset$`NumQueryComponents`))

cat("Curtosi: \n")
cat("URL LENGTH: \n")
print(curt(dataset$`UrlLength`))