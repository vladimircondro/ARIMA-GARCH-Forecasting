# 📌 Load Library
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
library(fGarch)
library(forecast)
library(tseries)
library(FinTS)
library(lmtest)
library(MASS)
library(readxl)

# 📌 1️⃣ Ambil Data Harga Saham BBCA
getSymbols("BBCA.JK", from = "2021-05-25", to = "2025-03-28")

# Konversi harga close menjadi numeric
BBCA.JK$BBCA.JK.Close <- as.numeric(BBCA.JK$BBCA.JK.Close)

# Simpan harga close
data <- BBCA.JK$BBCA.JK.Close

# 📌 2️⃣ Splitting Data: 80% Training, 20% Testing
N <- length(data)
train_size <- round(0.95 * N)
test_size <- N - train_size

train_data <- data[1:train_size]
test_data <- data[(train_size + 1):N]

# Check stasioneritas terhadap varians
library(MASS)
z<-boxcox(lm(train_data ~ 1))
# Check nilai lambda yang pas
install.packages("MASS")
library(MASS)
z <- boxcox(lm(train_data ~ 1), lambda = seq(-2, 2, 0.1))  # Uji lambda dari -2 hingga 2
(lambda_opt <- z$x[which.max(z$y)])  # Ambil lambda dengan log-likelihood tertinggi
# Didapatkan nilai lambda optimumnya adalah 2
lambda_opt <- 2  # Nilai lambda optimum dari analisis Box-Cox
# Terapkan Transformasi Box-Cox
train_data_trans <- (train_data^lambda_opt - 1) / lambda_opt
# Plot hasil transformasi untuk melihat perubahan data
plot(train_data_trans, type="l", xlab="Waktu", ylab="Data Setelah Transformasi", main="Plot Data Setelah Transformasi Box-Cox")
hist(train_data_trans, main="Histogram Data Setelah Transformasi", xlab="Nilai Setelah Transformasi")
data_trans
# Check hasilnya
z<-boxcox(lm(train_data_trans ~ 1))
# 📌 4️⃣ Uji Stasioneritas Mean dengan ADF Test
adf.test(train_data_trans)

# Jika tidak stasioner, lakukan differencing
train_data_diff <- diff(train_data_trans, differences = 1)
train_data_diff <- na.omit(train_data_diff)

# Uji ADF setelah differencing
adf.test(train_data_diff)

# 📌 5️⃣ Identifikasi Model ARIMA
acf(train_data_diff)
pacf(train_data_diff)

# Estimasi Model ARIMA & Uji Signifikansi
Arima011 <- arima(train_data, order = c(0,1,1), include.mean = TRUE, method = "ML")
Arima012 <- arima(train_data, order = c(0,1,2), include.mean = TRUE, method = "ML")
Arima111 <- arima(train_data, order = c(1,1,1), include.mean = TRUE, method = "ML")
Arima112 <- arima(train_data, order = c(1,1,2), include.mean = TRUE, method = "ML")

# Uji Signifikansi Parameter
lmtest::coeftest(Arima011)
lmtest::coeftest(Arima012)
lmtest::coeftest(Arima111)
lmtest::coeftest(Arima112)
AIC(Arima011)
AIC(Arima012)
AIC(Arima111)
# 📌 6️⃣ Uji Diagnostik Residual ARIMA
Box.test(residuals(Arima011), lag = 20, type = "Ljung-Box")
jarque.bera.test(residuals(Arima011))
Box.test(residuals(Arima012), lag = 20, type = "Ljung-Box")
jarque.bera.test(residuals(Arima012))
Box.test(residuals(Arima111), lag = 20, type = "Ljung-Box")
jarque.bera.test(residuals(Arima111))
# 📌 7️⃣ Uji Heteroskedastisitas Residual ARIMA
ArchTest(Arima111$residuals^2)
# Karena terdapat ada gejala heteroskedastistisitas, maka perlu pemodelan lebih lanjut adalah
# menggunakan model GARCH
# Plot ACF dan PACF Residual Kuadrat
residuals <- Arima111$residuals
residuals
acf(residuals^2, main="ACF of Residuals")
pacf(residuals^2, main="PACF of Squared Residuals")
# 📌 8️⃣ Model GARCH dengan Residual ARIMA
res_arima <- residuals(Arima111)
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "norm"
)

m_garch1 <- ugarchfit(spec = spec_garch, data = res_arima)
show(m_garch1)
spec_garch2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,3)),
  mean.model = list(armaOrder = c(1,1,1), include.mean = FALSE),  # Hilangkan ARIMA di mean
  distribution.model = "norm"
)
m_garch2 <- ugarchfit(spec = spec_garch2, data = res_arima)
show(m_garch2)

spec_garch3 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0,3)),
  mean.model = list(armaOrder = c(1,1,1), include.mean = FALSE),  # Hilangkan ARIMA di mean
  distribution.model = "norm"
)
m_garch3 <- ugarchfit(spec = spec_garch3, data = res_arima)
show(m_garch3)

# 📌 9️⃣ Uji Efek Asimetri (Sign Bias Test)
std_resid <- residuals(m_garch, standardize = TRUE)
S_neg <- ifelse(std_resid < 0, 1, 0)
S_pos <- ifelse(std_resid > 0, 1, 0)

S_neg_lag <- c(NA, head(S_neg, -1))
S_pos_lag <- c(NA, head(S_pos, -1))

data_test <- na.omit(data.frame(std_resid_squared = std_resid^2, S_neg, S_pos, S_neg_lag, S_pos_lag))

model_null <- lm(std_resid_squared ~ 1, data = data_test)
SSR0 <- sum(residuals(model_null)^2)

sign_bias_model <- lm(std_resid_squared ~ S_neg + S_pos + S_neg_lag + S_pos_lag, data = data_test)
SSR1 <- sum(residuals(sign_bias_model)^2)

if (SSR0 > SSR1) {
  k <- length(coef(sign_bias_model))
  T <- nrow(data_test)
  Fhitung <- ((SSR0 - SSR1) / (k - 1)) / (SSR1 / (T - k))
  Ftabel <- qf(0.95, df1 = k - 1, df2 = T - k)
  
  print(paste("F-statistik:", round(Fhitung, 4)))
  print(paste("F-tabel:", round(Ftabel, 4)))
  
  if (Fhitung > Ftabel) {
    print("❗ Efek asimetri signifikan (Tolak H0)")
  } else {
    print("✅ Tidak ada efek asimetri (Gagal Tolak H0)")
  }
} else {
  print("⚠️ Model dengan sign bias lebih buruk daripada tanpa sign bias.")
}

# 📌 🔟 Prediksi ARIMA untuk Testing Set
forecast_arima <- forecast::forecast(Arima111, h = test_size)
pred_arima <- forecast_arima$mean

# 📌 1️⃣1️⃣ Prediksi Volatilitas GARCH
forecast_garch <- ugarchforecast(m_garch, n.ahead = test_size)
pred_volatility <- sigma(forecast_garch)

# 📌 1️⃣2️⃣ Gabungkan Prediksi ARIMA & GARCH
set.seed(42)
pred_arima_garch <- pred_arima + rnorm(test_size, mean = 0, sd = pred_volatility)

# 📌 1️⃣3️⃣ Evaluasi Model dengan MAPE
test_data <- as.numeric(test_data)
pred_arima_garch <- as.numeric(pred_arima_garch)

mape_arima_garch <- mean(abs((test_data - pred_arima_garch) / test_data)) * 100
print(paste("MAPE ARIMA-GARCH:", round(mape_arima_garch, 2), "%"))

# 📌 🔮 1️⃣4️⃣ Prediksi Harga Saham BBCA untuk 30 Hari Kedepan

horizon <- 30  # Jumlah hari ke depan yang ingin diprediksi

# 🔹 1️⃣ Prediksi ARIMA selama 30 hari ke depan
forecast_arima_30 <- forecast::forecast(Arima111, h = horizon)
pred_arima_30 <- forecast_arima_30$mean  # Hasil prediksi ARIMA

# 🔹 2️⃣ Prediksi volatilitas GARCH selama 30 hari ke depan
forecast_garch_30 <- ugarchforecast(m_garch1, n.ahead = horizon)
pred_volatility_30 <- sigma(forecast_garch_30)  # Standar deviasi dari GARCH

# 🔹 3️⃣ Gabungkan hasil ARIMA dan GARCH
set.seed(42)  # Untuk reprodusibilitas
pred_hybrid_30 <- pred_arima_30 + rnorm(horizon, mean = 0, sd = pred_volatility_30)

# 🔹 4️⃣ Cetak hasil prediksi
print("📊 Prediksi Harga Saham BBCA untuk 30 Hari Kedepan:")
print(pred_hybrid_30)

# Pastikan train_data dalam bentuk vektor numerik
train_data <- as.numeric(train_data)

# **Cek panjang data**
print(length(train_data))  # Cek panjang data historis
print(length(pred_hybrid_30))  # Cek panjang prediksi

# 📌 🔄 Plot Data Historis (Hitam)
plot(
  1:length(train_data), train_data, 
  type = "l", col = "black", lwd = 2, 
  main = "Prediksi Hybrid ARIMA-GARCH 30 Hari Kedepan",
  xlab = "Waktu", ylab = "Harga Saham",
  ylim = c(min(train_data, pred_hybrid_30, na.rm = TRUE), 
           max(train_data, pred_hybrid_30, na.rm = TRUE))
)

# 🔵 **Tambahkan Garis Prediksi (Biru, Lebih Halus)**
lines((length(train_data) + 1):(length(train_data) + 30), 
      pred_hybrid_30, col = "blue", lwd = 2, type = "l")


