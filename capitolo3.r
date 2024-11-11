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