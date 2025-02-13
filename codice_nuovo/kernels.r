library(ggplot2)
library(knitr)
library(kableExtra)


dataset <- read.csv("./Phishing_Legitimate_full.csv")

# Kernel Density Plot per la lunghezza degli URL
# Kernel Density Plot
ggplot(dataset, aes(x = UrlLength)) + 
  geom_density(fill = "skyblue", alpha = 0.5) + 
  labs(title = "Kernel Density Plot della Lunghezza degli URL",
       x = "Lunghezza dell'URL", y = "Densità") +
  theme_minimal()

  ggplot(dataset, aes(x = NumQueryComponents)) + 
  geom_density(fill = "skyblue", alpha = 0.5) + 
  labs(title = "Kernel Density Plot per numero di componenti delle query",
       x = "Numero Param", y = "Densità") +
  theme_minimal()

    ggplot(dataset, aes(x = NumDash)) + 
  geom_density(fill = "skyblue", alpha = 0.5) + 
  labs(title = "Kernel Density Plot per numero di dashes (-)",
       x = "NumDash", y = "Densità") +
  theme_minimal()

ggplot(dataset, aes(x = UrlLength, color = factor(dataset$`CLASS_LABEL`))) + 
  geom_density() +
  labs(title = "Kernel Density Plot per Classe (urlLength)", 
       x = "Lunghezza dell'URL", y = "Densità", color = "Classe") +
  theme_minimal()


ggplot(dataset, aes(x = UrlLength, color = factor(dataset$`CLASS_LABEL`))) + 
  geom_density() +
  labs(title = "Kernel Density Plot per Classe (numQueryParams)", 
       x = "NumQueryParams", y = "Densità", color = "Classe") +
  theme_minimal()

  ggplot(dataset, aes(x = NumDash, color = factor(dataset$`CLASS_LABEL`))) + 
  geom_density() +
  labs(title = "Kernel Density Plot per Classe (NumDash)", 
       x = "NumDash", y = "Densità", color = "Classe") +
  theme_minimal()
