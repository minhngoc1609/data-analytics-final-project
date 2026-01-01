# Final Project

![Python](https://img.shields.io/badge/Language-Python-blue?logo=python)
![R Markdown](https://img.shields.io/badge/Rendered-R%20Markdown-orange?logo=r)
![Deep Learning](https://img.shields.io/badge/Model-LSTM%20|%20SVR%20|%20ARIMA-green)

## 📝 Abstract
Ethereum is the backbone of programmable blockchain, transcending finance through smart contracts. This project evaluates ETH price prediction models by integrating **global financial assets** (Oil, Gold, Nasdaq, USD) and **technological determinants** (Blockchain metrics). Using a two-stage approach—Random Forest for feature selection and LSTM for prediction—we demonstrate that incorporating external determinants significantly outperforms traditional time-series models like ARIMA and SVR.

---

## 🚀 Key Research Questions
1. **Factor Impact:** Which economic and technological factors significantly influence ETH/USD exchange rates?
2. **Model Comparison:** How does the hybrid LSTM model compare against SVR, ARIMA, and historical-only LSTM?
3. **Information Value:** Can economic and technological determinants provide better predictive power than historical prices alone?

---

## 🛠️ Methodology: The Two-Stage Approach

The study proposes a robust framework to investigate hidden information in multi-source data:

1.  **Stage 1: Feature Selection (Random Forest):** * Narrowing down a vast pool of predictors by weighing the significance of technological and economic aspects.
2.  **Stage 2: Prediction (LSTM Network):** * Incorporating selected potential predictors into a Long Short-Term Memory network to forecast exchange rates.

**Key Result:** The hybrid LSTM achieves superior performance, proving that external determinants are more critical for accuracy than relying solely on previous price trends.

---

## 📊 Data Preparation & Sources
The research analyzes data from **March 16, 2016, to March 14, 2023**.

### 1. Data Dictionary
| Category | Factors | Source |
| :--- | :--- | :--- |
| **Economic** | Crude Oil, Gold Price, NASDAQ, Exchange Rates (EUR/JPY/CNY to USD) | Investing.com |
| **Technological** | Transaction Value/Volume, Average Fee, Block Size/Time, Market Cap, Gas Limit | Bitinfocharts.com & Etherscan.io |
| **Market Sentiment** | Ethereum Mention Tweets, BTC Price | Twitter & Bitinfocharts |

---

## 📂 Project Structure
* **Chapter 1:** Introduction & Motivation
* **Chapter 2:** Data Preparation & Preprocessing
* **Chapter 3:** Methodology & Algorithm Design (RF + LSTM)
* **Chapter 4:** Comparative Results Analysis
* **Chapter 5:** Conclusion, Limitations & Future Work

---

## 📈 Model Performance Highlights
* **LSTM (Hybrid):** Best overall performance by capturing non-linear relationships in macroeconomic and blockchain data.
* **ARIMA:** Limited to linear patterns; struggled with high-volatility crypto data.
* **SVR:** Underperformed compared to deep learning in handling long-term dependencies.

---

## 🌐 Interactive Report
The full analysis, including code and visualizations rendered via R Markdown, can be found here:
👉 **[View Full Data Analytics Report](https://minhngoc1609.github.io/data-analytics-final-project/)**
