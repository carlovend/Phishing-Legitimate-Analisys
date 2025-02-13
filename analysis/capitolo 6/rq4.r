# Installare e caricare pacchetti necessari
list.of.packages <- c("ggplot2", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cran.rstudio.com/")

library(ggplot2)
library(dplyr)

# Caricare i dataset
file_real <- "Phishing_Legitimate_full.csv"
file_synthetic <- "Corrected_Dataset.csv"

dataset_real <- read.csv(file_real)
dataset_synthetic <- read.csv(file_synthetic)

# Selezionare le feature più significative
features_selected <- c("PctExtNullSelfRedirectHyperlinksRT", 
                       "FrequentDomainNameMismatch", 
                       "NumDash", 
                       "SubmitInfoToEmail",
                       "PctNullSelfRedirectHyperlinks")

# Funzione per generare i Kernel Density Plot
plot_kernel_density <- function(real_data, synthetic_data, variable_name) {
  
  # Rimuovere NA
  real_data <- na.omit(real_data)
  synthetic_data <- na.omit(synthetic_data)
  
  # Creare dataframe combinato per il grafico
  data_combined <- data.frame(
      Valore = c(real_data, synthetic_data),
      Tipo = rep(c("Reale", "Sintetico"), c(length(real_data), length(synthetic_data)))
  )
  
  # Creare il Kernel Density Plot
  density_plot <- ggplot(data_combined, aes(x = Valore, fill = Tipo)) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Kernel Density Plot di", variable_name, ": Reale vs Sintetico"),
           x = variable_name, y = "Densità") +
      scale_fill_manual(values = c("blue", "red")) +
      theme_minimal()
  
  # Salvare il grafico
  ggsave(paste0("Kernel_Density_", variable_name, ".pdf"), plot = density_plot, width = 8, height = 6)
  
  cat("📊 Kernel Density Plot generato per:", variable_name, "\n")
}

# Applicare la funzione a tutte le feature selezionate
for (feature in features_selected) {
  plot_kernel_density(dataset_real[[feature]], dataset_synthetic[[feature]], feature)
}
