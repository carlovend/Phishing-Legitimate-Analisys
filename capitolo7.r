
data <- read.csv("Phishing_Legitimate_full.csv")

# Istogramma con curva normale
hist(data$UrlLength, prob = TRUE, main = "Distribuzione di UrlLength",
     xlab = "UrlLength", ylab = "Densità", col = "lightblue", border = "black")
lines(density(data$UrlLength), col = "red", lwd = 2)

# Parametri stimati
mu <- mean(data$UrlLength)
sigma <- sd(data$UrlLength)

# Aggiungi jitter ai dati
jittered_data <- jitter(data$UrlLength)

# Esegui nuovamente il Kolmogorov-Smirnov test
ks.test(jittered_data, "pnorm", mean = mu, sd = sigma)

qqnorm(data$UrlLength, main = "QQ-Plot di UrlLength")
qqline(data$UrlLength, col = "red", lwd = 2)
#Densità di probabilità UrlLength
plot(density(data$UrlLength), main = "Densità di UrlLength",
     xlab = "UrlLength", ylab = "Densità", col = "blue")

# Test di Wilcoxon su UrlLength rispetto a un valore di riferimento
wilcox.test(data$UrlLength, mu = 50)


# Parametri
mu <- mean(data$UrlLength)
sigma <- sd(data$UrlLength)

# Calcolo dei limiti
lower_limit <- mu - 3 * sigma
upper_limit <- mu + 3 * sigma

# Visualizza i limiti
print(paste("Limiti del 3σ:", lower_limit, "a", upper_limit))

# Identificazione degli outlier
outliers <- data$UrlLength[data$UrlLength < lower_limit | data$UrlLength > upper_limit]
print(outliers)

# Visualizza gli outlier sul grafico
hist(data$UrlLength, prob = TRUE, main = "Outlier secondo la Regola del 3σ",
     xlab = "UrlLength", ylab = "Densità", col = "lightblue", border = "black")
abline(v = lower_limit, col = "red", lwd = 2, lty = 2)
abline(v = upper_limit, col = "red", lwd = 2, lty = 2)
