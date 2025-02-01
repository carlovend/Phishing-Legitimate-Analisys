library(ggplot2)
library(knitr)
library(kableExtra)

dataset <- read.csv("./Phishing_Legitimate_full.csv")

## vediamo se UrlLength ha distribuzione normale
# Istogramma con curva normale sovrapposta
hist(dataset$UrlLength, breaks = 30, probability = TRUE,
     main = "Istogramma di UrlLength con curva normale", 
     xlab = "UrlLength", ylab = "Densità")

# Calcola i parametri della normale
mean_url <- mean(dataset$UrlLength, na.rm = TRUE)
sd_url <- sd(dataset$UrlLength, na.rm = TRUE)

# Aggiungi la curva normale
curve(dnorm(x, mean = mean_url, sd = sd_url), 
      col = "red", lwd = 2, add = TRUE)

# Test di Kolmogorov-Smirnov per stabilire se urlLength ha una distribuzione normale
# p-value < 2.2e-16: Il p-value molto piccolo indica che possiamo rifiutare l’ipotesi nulla. 
# L’ipotesi nulla ( H_0 ) è che i dati seguano una distribuzione normale. 
# Con un p-value così basso, abbiamo evidenza sufficiente per dire che UrlLength non segue una distribuzione normale.
ks_test <- ks.test(dataset$UrlLength, "pnorm", 
                   mean = mean_url, sd = sd_url)

cat("Kolmogorov-Smirnov Test:\n")
print(ks_test)



## vediamo se NumQueryComponents ha distribuzione normale
# Istogramma con curva normale sovrapposta
hist(dataset$NumQueryComponents , breaks = 30, probability = TRUE,
     main = "Istogramma di NumQueryComponents  con curva normale", 
     xlab = "NumQueryComponents ", ylab = "Densità")

# Calcola i parametri della normale
mean_numQueryParams <- mean(dataset$NumQueryComponents , na.rm = TRUE)
sd_numQueryParams <- sd(dataset$NumQueryComponents , na.rm = TRUE)

# Aggiungi la curva normale
curve(dnorm(x, mean = mean_numQueryParams, sd = sd_numQueryParams), 
      col = "red", lwd = 2, add = TRUE)

# Test di Kolmogorov-Smirnov per stabilire se NumQueryComponents ha una distribuzione normale
# anche qui p-value < 2.2e-16: Non abbiamo distribuzione normale. 
ks_test <- ks.test(dataset$NumQueryComponents, "pnorm", 
                   mean = mean_numQueryParams, sd = sd_numQueryParams)

cat("Kolmogorov-Smirnov Test:\n")
print(ks_test)


# Istogramma con curva esponenziale
hist(dataset$UrlLength, breaks = 30, probability = TRUE,
     main = "Istogramma di UrlLength con curva esponenziale", 
     xlab = "UrlLength", ylab = "Densità")

lambda_hat <- 1 / mean(dataset$UrlLength, na.rm = TRUE)
curve(dexp(x, rate = lambda_hat), col = "blue", lwd = 2, add = TRUE)


hist(dataset$NumQueryComponents, breaks = 30, probability = TRUE,
     main = "Istogramma di UrlLength con curva esponenziale", 
     xlab = "UrlLength", ylab = "Densità")

lambda_hat <- 1 / mean(dataset$NumQueryComponents, na.rm = TRUE)
curve(dexp(x, rate = lambda_hat), col = "blue", lwd = 2, add = TRUE)



# METODO DEI MOMENTI non ha senso poichè urllength e numqueryparams non hanno distribuzione normale


# Metodo della massima verosimiglianza
log_likelihood <- function(params) {
  mu <- params[1]
  sigma <- params[2]
  -sum(dnorm(dataset$UrlLength, mean = mu, sd = sigma, log = TRUE))
}
initial_guess <- c(mean(dataset$UrlLength, na.rm = TRUE), sd(dataset$UrlLength, na.rm = TRUE))
mle <- optim(initial_guess, log_likelihood, method = "BFGS")

cat("Stima (MLE):\n")
cat("Media:", mle$par[1], "\n")
cat("Deviazione standard:", mle$par[2], "\n")


