# Caricamento del dataset
data <- read.csv("Phishing_Legitimate_full.csv")


# Calcolo del totale dei caratteri speciali
data$TotalSpecialChars <- data$NumDash + data$NumDashInHostname +
                          data$AtSymbol + data$TildeSymbol +
                          data$NumUnderscore + data$NumPercent +data$NumAmpersand + data$NumHash 
                          + data$NumNumericChars + data$NumLetters + data$NumDots

# Scatterplot
plot(data$UrlLength, data$TotalSpecialChars,
     main = "Relazione tra lunghezza URL e caratteri speciali totali",
     xlab = "Lunghezza URL",
     ylab = "Totale Caratteri Speciali",
     pch = 19, col = "blue")


# Covarianza
covariance <- cov(data$UrlLength, data$TotalSpecialChars, use = "complete.obs")
print(paste("Covarianza:", covariance))

# Correlazione
correlation <- cor(data$UrlLength, data$TotalSpecialChars, use = "complete.obs")
print(paste("Correlazione:", correlation))

# Boxplot di TotalSpecialChars per Class
boxplot(TotalSpecialChars ~ CLASS_LABEL, data = data,
        main = "Distribuzione dei Caratteri Speciali per Classificazione",
        xlab = "Classificazione (0 = Legittimo, 1 = Phishing)",
        ylab = "Totale Caratteri Speciali",
        col = c("skyblue", "pink"))

# Modello di regressione lineare
model <- lm(TotalSpecialChars ~ UrlLength, data = data)

# Sommario del modello
summary(model)

# Scatterplot con linea di regressione
plot(data$UrlLength, data$TotalSpecialChars,
     main = "Regressione Lineare: Url Length e Caratteri Speciali",
     xlab = "Lunghezza URL",
     ylab = "Totale Caratteri Speciali",
     pch = 19, col = "blue")
abline(model, col = "red", lwd = 2)  # Aggiunge la linea di regressione


# Creazione di intervalli per UrlLength
data$UrlLength_bins <- cut(data$UrlLength, breaks = seq(0, max(data$UrlLength), by = 10))

# Calcolo della media di TotalSpecialChars per ogni intervallo
mean_special_chars <- aggregate(TotalSpecialChars ~ UrlLength_bins, data = data, mean)

# Calcolo delle medie per ciascun intervallo (come nel caso del barplot)
plot(mean_special_chars$TotalSpecialChars, type = "o",
     main = "Trend della Media di Caratteri Speciali rispetto alla Lunghezza URL",
     xlab = "Intervalli di Lunghezza URL",
     ylab = "Media di Caratteri Speciali",
     col = "blue", lwd = 2)
polygon(c(1:length(mean_special_chars$TotalSpecialChars), length(mean_special_chars$TotalSpecialChars):1),
        c(mean_special_chars$TotalSpecialChars, rep(0, length(mean_special_chars$TotalSpecialChars))),
        col = "lightblue", border = NA)


# Calcolo dei residui
residuals <- resid(model)

# Media campionaria dei residui
mean_residuals <- mean(residuals)
print(paste("Media campionaria dei residui:", mean_residuals))

# Varianza campionaria dei residui
var_residuals <- var(residuals)
print(paste("Varianza campionaria dei residui:", var_residuals))

# Plot dei residui rispetto a UrlLength
plot(data$UrlLength, residuals,
     main = "Residui rispetto a UrlLength",
     xlab = "UrlLength",
     ylab = "Residui",
     pch = 19, col = "purple")
abline(h = 0, col = "red", lwd = 2)  # Linea orizzontale a zero


# Istogramma dei residui
hist(residuals, breaks = 30,
     main = "Distribuzione dei Residui",
     xlab = "Residui",
     ylab = "Frequenza",
     col = "lightblue", border = "black")
abline(v = mean_residuals, col = "red", lwd = 2, lty = 2)  # Linea per la media

# Plot dei residui ordinati
plot(residuals, type = "p",
     main = "Residui Ordinati per Osservazione",
     xlab = "Indice dell'Osservazione",
     ylab = "Residui",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Linea orizzontale a zero


# Valori previsti
fitted_values <- fitted(model)

# Plot residui vs valori previsti
plot(fitted_values, residuals,
     main = "Residui rispetto ai Valori Predetti",
     xlab = "Valori Predetti",
     ylab = "Residui",
     pch = 19, col = "green")
abline(h = 0, col = "red", lwd = 2)  # Linea orizzontale a zero


# Boxplot dei residui
boxplot(residuals,
        main = "Boxplot dei Residui",
        ylab = "Residui",
        col = "orange")
abline(h = 0, col = "red", lwd = 2, lty = 2)  # Linea orizzontale a zero


# Combina grafici
par(mfrow = c(2, 2))  # 2x2 griglia di grafici

# 1. Residui rispetto a UrlLength
plot(data$UrlLength, residuals,
     main = "Residui rispetto a UrlLength",
     xlab = "UrlLength",
     ylab = "Residui",
     pch = 19, col = "purple")
abline(h = 0, col = "red", lwd = 2)

# 2. Istogramma dei residui
hist(residuals, breaks = 30,
     main = "Distribuzione dei Residui",
     xlab = "Residui",
     ylab = "Frequenza",
     col = "lightblue", border = "black")
abline(v = mean_residuals, col = "red", lwd = 2, lty = 2)

# 3. Residui ordinati
plot(residuals, type = "p",
     main = "Residui Ordinati per Osservazione",
     xlab = "Indice dell'Osservazione",
     ylab = "Residui",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# 4. Boxplot dei residui
boxplot(residuals,
        main = "Boxplot dei Residui",
        ylab = "Residui",
        col = "orange")
abline(h = 0, col = "red", lwd = 2, lty = 2)



# Valori previsti
fitted_values <- fitted(model)

# Plot residui vs valori previsti
plot(fitted_values, residuals,
     main = "Residui rispetto ai Valori Predetti",
     xlab = "Valori Predetti",
     ylab = "Residui",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)


# Modello di regressione multipla
model_multiple <- lm(TotalSpecialChars ~ UrlLength + NumDash + NumPercent + AtSymbol + NumUnderscore, data = data)

# Sommario del modello
summary(model_multiple)


# Plot Valori Predetti vs Valori Osservati
plot(data$TotalSpecialChars, fitted(model_multiple),
     main = "Valori Predetti vs Valori Osservati",
     xlab = "Valori Osservati (TotalSpecialChars)",
     ylab = "Valori Predetti",
     pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2)  # Linea ideale (y = x)


# Residui rispetto ai valori predetti
plot(fitted(model_multiple), resid(model_multiple),
     main = "Residui rispetto ai Valori Predetti (Modello Multiplo)",
     xlab = "Valori Predetti",
     ylab = "Residui",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# QQ-Plot per normalità dei residui
qqnorm(resid(model_multiple), main = "QQ-Plot dei Residui (Modello Multiplo)")
qqline(resid(model_multiple), col = "red", lwd = 2)


# Confronto tra modello semplice e multiplo
anova(model, model_multiple)


# Coefficienti del modello multiplo
coefficients <- coef(model_multiple)

# Grafico a barre dei coefficienti
barplot(coefficients, main = "Coefficiente Stimati (Modello Multiplo)",
        xlab = "Variabili",
        ylab = "Valore del Coefficiente",
        col = "skyblue", las = 2)


# Cook's Distance
cooksd <- cooks.distance(model_multiple)

# Plot di Cook's Distance
plot(cooksd, type = "h", col = "orange",
     main = "Cook's Distance",
     xlab = "Indice dell'Osservazione",
     ylab = "Cook's Distance")
abline(h = 4/(nrow(data) - length(model_multiple$coefficients)), col = "red", lwd = 2)



