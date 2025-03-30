# ARIMA-GARCH-Forecasting BBCA STOCK
📌 Overview
Proyek ini bertujuan untuk memodelkan dan memprediksi volatilitas harga saham BBCA menggunakan pendekatan ARIMA-GARCH. Kombinasi ARIMA untuk menangkap tren harga dan GARCH untuk volatilitas menghasilkan prediksi yang lebih akurat dalam menganalisis pergerakan pasar.
🔍 Insights yang Dihasilkan
MAPE Model ARIMA-GARCH: 9.07%
Prediksi 30 Hari Kedepan: Harga saham diprediksi mengalami fluktuasi dengan volatilitas yang signifikan.
📈 Analisis Utama
Model terbaik: ARIMA(1,1,1) + GARCH(1,1)
Uji Heteroskedastisitas: Terdapat efek heteroskedastisitas, sehingga pemodelan GARCH diperlukan.
Uji Asimetri: Efek asimetri signifikan, menunjukkan volatilitas pasar yang tidak seimbang.
🎯 Rekomendasi Strategi
1. Gunakan hasil prediksi untuk mendukung keputusan investasi.
2. Perhatikan volatilitas tinggi untuk strategi risk management.
3. Terapkan model hybrid lainnya seperti ARIMAX atau LSTM untuk peningkatan akurasi.
⚙️ Tools yang Digunakan
R (rugarch, forecast, quantmod) untuk analisis dan pemodelan data saham.

