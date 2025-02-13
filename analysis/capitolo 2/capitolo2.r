
library(ggplot2)


file_path <- "Phishing_Legitimate_full.csv"
dataset <- read.csv(file_path)

# Calcolare la frequenza assoluta e relativa per PctExtNullSelfRedirectHyperlinksRT
freq_abs <- table(dataset$PctExtNullSelfRedirectHyperlinksRT)
freq_rel <- prop.table(freq_abs)


freq_df <- data.frame(Intervallo = names(freq_abs), 
                      Frequenza_Assoluta = as.vector(freq_abs), 
                      Frequenza_Relativa = as.vector(freq_rel))


print("Frequenza Assoluta per PctExtNullSelfRedirectHyperlinksRT:")
print(freq_abs)
print("Frequenza Relativa per PctExtNullSelfRedirectHyperlinksRT:")
print(freq_rel)


ggplot(freq_df, aes(x = Intervallo, y = Frequenza_Assoluta, fill = Intervallo)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Distribuzione di PctExtNullSelfRedirectHyperlinksRT",
       x = "Intervallo", 
       y = "Frequenza Assoluta") +
  theme_classic(base_size = 14) +  # Sfondo bianco
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.position = "none")


freq_abs <- table(dataset$FrequentDomainNameMismatch)
freq_rel <- prop.table(freq_abs)


freq_df <- data.frame(Valore = names(freq_abs), 
                      Frequenza_Assoluta = as.vector(freq_abs), 
                      Frequenza_Relativa = as.vector(freq_rel))


print("Frequenza Assoluta per FrequentDomainNameMismatch:")
print(freq_abs)
print("Frequenza Relativa per FrequentDomainNameMismatch:")
print(freq_rel)


pdf("FrequentDomainNameMismatch_Distribuzione.pdf")


ggplot(freq_df, aes(x = Valore, y = Frequenza_Assoluta, fill = Valore)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Distribuzione di FrequentDomainNameMismatch",
       x = "Valore", 
       y = "Frequenza Assoluta") +
  theme_classic(base_size = 14) +  # Sfondo bianco
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.position = "none")

  
breaks <- c(0, 2, 5, 10, 20, max(dataset$NumDash, na.rm = TRUE))
labels <- c("0-2", "3-5", "6-10", "11-20", "21+")
dataset$NumDash_binned <- cut(dataset$NumDash, breaks = breaks, labels = labels, include.lowest = TRUE, ordered_result = TRUE)


freq_abs <- table(dataset$NumDash_binned)
freq_rel <- prop.table(freq_abs)


freq_df <- data.frame(Intervallo = names(freq_abs), 
                      Frequenza_Assoluta = as.vector(freq_abs), 
                      Frequenza_Relativa = as.vector(freq_rel))


freq_df$Intervallo <- factor(freq_df$Intervallo, levels = unique(freq_df$Intervallo), ordered = TRUE)


print("Frequenza Assoluta per NumDash (raggruppato):")
print(freq_abs)
print("Frequenza Relativa per NumDash (raggruppato):")
print(freq_rel)


pdf("NumDash_Distribuzione_Aggregata.pdf")


ggplot(freq_df, aes(x = Intervallo, y = Frequenza_Assoluta, fill = Intervallo)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Distribuzione di NumDash ",
       x = "Intervallo", 
       y = "Frequenza Assoluta") +
  theme_classic(base_size = 14) +  # Sfondo bianco
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "none")

  
freq_abs <- table(dataset$SubmitInfoToEmail)
freq_rel <- prop.table(freq_abs)


freq_df <- data.frame(Valore = names(freq_abs), 
                      Frequenza_Assoluta = as.vector(freq_abs), 
                      Frequenza_Relativa = as.vector(freq_rel))


print("Frequenza Assoluta per SubmitInfoToEmail:")
print(freq_abs)
print("Frequenza Relativa per SubmitInfoToEmail:")
print(freq_rel)


pdf("SubmitInfoToEmail_Distribuzione.pdf")


ggplot(freq_df, aes(x = Valore, y = Frequenza_Assoluta, fill = Valore)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Distribuzione di SubmitInfoToEmail",
       x = "Valore", 
       y = "Frequenza Assoluta") +
  theme_classic(base_size = 14) +  # Sfondo bianco
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.position = "none")

  
breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
labels <- c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1")
dataset$PctNullSelfRedirectHyperlinks_binned <- cut(dataset$PctNullSelfRedirectHyperlinks, breaks = breaks, labels = labels, include.lowest = TRUE, ordered_result = TRUE)


freq_abs <- table(dataset$PctNullSelfRedirectHyperlinks_binned)
freq_rel <- prop.table(freq_abs)

# Creare un dataframe con i risultati
freq_df <- data.frame(Intervallo = names(freq_abs), 
                      Frequenza_Assoluta = as.vector(freq_abs), 
                      Frequenza_Relativa = as.vector(freq_rel))


freq_df$Intervallo <- factor(freq_df$Intervallo, levels = unique(freq_df$Intervallo), ordered = TRUE)

# Stampare i risultati
print("Frequenza Assoluta per PctNullSelfRedirectHyperlinks (raggruppato):")
print(freq_abs)
print("Frequenza Relativa per PctNullSelfRedirectHyperlinks (raggruppato):")
print(freq_rel)


pdf("PctNullSelfRedirectHyperlinks_Distribuzione.pdf")


ggplot(freq_df, aes(x = Intervallo, y = Frequenza_Assoluta, fill = Intervallo)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Distribuzione di PctNullSelfRedirectHyperlinks (Raggruppato)",
       x = "Intervallo", 
       y = "Frequenza Assoluta") +
  theme_classic(base_size = 14) +  # Sfondo bianco
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "none")