# 📌 Installare e caricare i pacchetti necessari
list.of.packages <- c("ggplot2", "caret", "dplyr", "MASS", "pROC", "iml")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cran.rstudio.com/")

library(ggplot2)
library(caret)
library(dplyr)
library(MASS)
library(pROC)
library(iml)

# 📌 Caricare il dataset
file_path <- "Phishing_Legitimate_full.csv"
dataset <- read.csv(file_path)

# 📌 Selezionare le feature e la variabile target
features_selected <- c("PctExtNullSelfRedirectHyperlinksRT", 
                        "FrequentDomainNameMismatch", 
                        "NumDash", 
                        "PctNullSelfRedirectHyperlinks", 
                        "PctExtHyperlinks", 
                        "PathLevel", 
                        "UrlLength", 
                        "HostnameLength", 
                        "CLASS_LABEL")  # Include CLASS_LABEL

data_selected <- dataset[, features_selected]

# 📌 1️⃣ Gestire i valori mancanti
data_selected <- na.omit(data_selected)

# 📌 2️⃣ Rimuovere feature ridondanti (bassa varianza)
low_variance <- nearZeroVar(data_selected, saveMetrics = TRUE)
low_variance_features <- rownames(low_variance[low_variance$nzv == TRUE, ])
data_selected <- data_selected[, !(names(data_selected) %in% low_variance_features)]

# 📌 3️⃣ Trasformazione delle variabili:
# 🔹 Verificare che le feature esistano prima della standardizzazione
continuous_features <- c("NumDots", "SubdomainLevel", "PathLevel", "UrlLength", 
                         "NumUnderscore", "NumPercent", "NumQueryComponents", "NumAmpersand", 
                         "NumNumericChars", "HostnameLength", "PathLength", "QueryLength", 
                         "PctExtHyperlinks", "PctExtResourceUrls", "PctNullSelfRedirectHyperlinks")

# 📌 Selezionare solo le variabili effettivamente presenti
continuous_features <- intersect(continuous_features, colnames(data_selected))

# 🔹 Standardizzare solo le feature presenti
data_selected[continuous_features] <- scale(data_selected[continuous_features])

# 🔹 Log-trasformare variabili altamente skewed (solo se esistono)
skewed_features <- c("NumDash", "UrlLength", "HostnameLength")  
skewed_features <- intersect(skewed_features, colnames(data_selected))
data_selected[skewed_features] <- log(data_selected[skewed_features] + 1)  # Evita log(0)

# 🔹 Convertire le variabili categoriche in fattori (solo se presenti)
categorical_features <- c("NumDashInHostname", "AtSymbol", "TildeSymbol", "NumHash", 
                          "NoHttps", "RandomString", "IpAddress", "DomainInSubdomains", 
                          "DomainInPaths", "HttpsInHostname", "DoubleSlashInPath", 
                          "NumSensitiveWords", "EmbeddedBrandName", "ExtFavicon", 
                          "InsecureForms", "RelativeFormAction", "ExtFormAction", 
                          "AbnormalFormAction", "FrequentDomainNameMismatch", 
                          "FakeLinkInStatusBar", "RightClickDisabled", "PopUpWindow", 
                          "SubmitInfoToEmail", "IframeOrFrame", "MissingTitle", 
                          "ImagesOnlyInForm", "SubdomainLevelRT", "UrlLengthRT", 
                          "PctExtResourceUrlsRT", "AbnormalExtFormActionR", 
                          "ExtMetaScriptLinkRT", "PctExtNullSelfRedirectHyperlinksRT")

categorical_features <- intersect(categorical_features, colnames(data_selected))
data_selected[categorical_features] <- lapply(data_selected[categorical_features], as.factor)

# 📌 Convertire la variabile target in fattore
data_selected$CLASS_LABEL <- as.factor(data_selected$CLASS_LABEL)

# 📌 Dividere il dataset in training (70%) e test (30%)
set.seed(123)
train_index <- createDataPartition(data_selected$CLASS_LABEL, p = 0.7, list = FALSE)
train_data <- data_selected[train_index, ]
test_data <- data_selected[-train_index, ]

# 📌 Creare il modello di regressione logistica
model <- glm(CLASS_LABEL ~ ., data = train_data, family = binomial)

# 📌 Feature Selection Automatica con AIC
model_best <- stepAIC(model, direction = "both", trace = FALSE)

# 📌 Stampare i risultati del modello ottimizzato
summary(model_best)

# 📌 Fare previsioni sul test set
predictions <- predict(model_best, test_data, type = "response")

# 📌 Convertire le probabilità in classi (0 o 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# 📌 Creare la matrice di confusione
conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$CLASS_LABEL)
print(conf_matrix)

# 📌 Calcolare l'accuratezza del modello
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("✅ Accuratezza del modello ottimizzato:", accuracy, "\n")

# 📌 6️⃣ Learning Curve: Analizziamo la performance con diverse dimensioni del dataset
train_sizes <- seq(0.1, 1.0, by = 0.1)  # Da 10% a 100% del training set
train_acc <- c()
test_acc <- c()

for (size in train_sizes) {
  set.seed(123)
  train_sample <- train_data[sample(1:nrow(train_data), size * nrow(train_data)), ]
  
  # Modello su subset del training set
  model_temp <- glm(CLASS_LABEL ~ ., data = train_sample, family = binomial)
  
  # Accuratezza sul training set
  train_preds <- predict(model_temp, train_sample, type = "response")
  train_classes <- ifelse(train_preds > 0.5, 1, 0)
  train_conf_matrix <- table(Predicted = train_classes, Actual = train_sample$CLASS_LABEL)
  train_acc <- c(train_acc, sum(diag(train_conf_matrix)) / sum(train_conf_matrix))
  
  # Accuratezza sul test set
  test_preds <- predict(model_temp, test_data, type = "response")
  test_classes <- ifelse(test_preds > 0.5, 1, 0)
  test_conf_matrix <- table(Predicted = test_classes, Actual = test_data$CLASS_LABEL)
  test_acc <- c(test_acc, sum(diag(test_conf_matrix)) / sum(test_conf_matrix))
}

# 📌 Creare il plot della Learning Curve
pdf("Learning_Curve.pdf", width = 8, height = 6)
plot(train_sizes, train_acc, type = "o", col = "blue", lwd = 2, ylim = c(0.5, 1),
     xlab = "Proporzione del Training Set", ylab = "Accuratezza", 
     main = "Learning Curve - Regressione Logistica")
lines(train_sizes, test_acc, type = "o", col = "red", lwd = 2)
legend("bottomright", legend = c("Training Accuracy", "Test Accuracy"), 
       col = c("blue", "red"), lwd = 2)
dev.off()

cat("✅ Learning Curve generata: controlla il file 'Learning_Curve.pdf'.\n")




# 📌 Applicare SHAP per l'interpretabilità
# Verificare e rimuovere eventuali NA nei dati di test
shap_input <- test_data[1:5, -which(names(test_data) == "CLASS_LABEL")]
if (any(is.na(shap_input))) {
  shap_input <- na.omit(shap_input)
  cat("Rimosse osservazioni con valori mancanti per l'analisi SHAP.\n")
}

# Creare il predittore per SHAP
predictor <- Predictor$new(model_best, 
                           data = train_data[, -which(names(train_data) == "CLASS_LABEL")], 
                           y = train_data$CLASS_LABEL, 
                           predict.function = function(model, newdata) {
                             pred <- predict(model, newdata, type = "response")
                             pred[is.na(pred)] <- 0  # Gestire i valori NA sostituendoli con 0
                             return(as.numeric(pred))
                           })

# Calcolare i valori SHAP per le osservazioni pulite
shap_values <- Shapley$new(predictor, x.interest = shap_input)

# Visualizzare i risultati SHAP
shap_values$plot()

# Calcolare e visualizzare l'importanza globale delle feature
feature_imp <- FeatureImp$new(predictor, loss = "ce")
feature_imp$plot()

# Salvare il grafico dei valori SHAP in PDF
pdf("SHAP_Values_Cleaned.pdf", width = 8, height = 6)
shap_values$plot()
dev.off()

cat("✅ Valori SHAP generati con dati puliti: controlla il file 'SHAP_Values_Cleaned.pdf'.\n")