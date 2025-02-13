# Phishing & Legitimate Analysis

Questo repository contiene il progetto di statistica e analisi dei dati di **Leopoldo Todisco** e **Carlo Venditto**.

Il progetto si concentra sull'analisi di un dataset relativo ai siti web di phishing, disponibile su Kaggle:  
[🔗 Phishing Dataset](https://www.kaggle.com/datasets/amj464/phishing)

---

## 📁 Struttura del Repository

Il repository è suddiviso in due sezioni principali:

### 1️⃣ `analysis/`  
Questa cartella contiene il codice **R** utilizzato per l'analisi del dataset e la produzione del nostro report.  
Ogni sottocartella corrisponde a un capitolo del report (a partire dal capitolo 2).

- **`capitolo2/`** – Analisi introduttiva e visualizzazione del dataset  
- **`capitolo3/`** – Statistica univariata (indici di sintesi, distribuzioni, etc.)  
- **`capitolo4/`** – Statistica bivariata (correlazioni e relazioni tra variabili)  
- **`capitolo5/`** – Creazione di un modello di regressione logistica  
- **`capitolo6/`** – Generazione di un dataset sintetico, analisi statistica con test del **Chi Quadrato**, studio della **distribuzione uniforme** e codice per rispondere alle quattro **Research Questions (RQ)**:  
  - **RQ1**: In che misura la media, la mediana e la moda delle variabili sintetiche si discostano da quelle osservate nei dati reali?  
  - **RQ2**: La varianza e la deviazione standard delle variabili sintetiche sono statisticamente indistinguibili da quelle dei dati originali?  
  - **RQ3**: In che misura la matrice di correlazione delle variabili sintetiche differisce da quella dei dati originali?  
  - **RQ4**: Le distribuzioni delle variabili sintetiche appartengono a distribuzioni note?

---

### 2️⃣ `data_generation/`  
Questa cartella contiene i notebook **Python** per la generazione di dati sintetici tramite un LLM-Powered Agent con Self-Refinement.  
L'implementazione è realizzata con **LangGraph** e utilizza le API di **OpenAI** per la creazione di un **agent** in grado di generare dati basati sulle caratteristiche del dataset originale.

---

📌 **Autori:**  
- **Leopoldo Todisco**  
- **Carlo Venditto**
