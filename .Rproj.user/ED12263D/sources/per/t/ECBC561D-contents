library('tidyr')
library('ggplot2')
library('stringr')
library('skimr')
library('readr')
library('dplyr')
library('zoo')
library('caret')
library(randomForest)
library(nnet)
library(forecast)
library(e1071)
library(gridExtra)

#Read csv sep by ;
data <- read_delim(
  "Dataset.csv",
  delim = ";",
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)

data2 <- read_delim(
  "Dataset.csv",
  delim = ";",
  locale = locale(decimal_mark = ".", grouping_mark = ",")
)

data$btc_price <- data2$btc_price

data <- data[-1]

#Overview of data
skim_without_charts(data)

#Data cleaning
data_clean <- na.omit(data)
data_clean <- as.data.frame(lapply(data_clean, function(x) ifelse(x == "null", NA, x)))
cols_need_converted <- c("tweets","transaction_fee","transaction_value","number_of_transaction","blocksize", "blocktime", "marketcap")
data_clean[cols_need_converted] <- lapply(data_clean[cols_need_converted], function(x) gsub(",", ".", x))
data_clean[cols_need_converted] <- lapply(data_clean[cols_need_converted], as.numeric)
data_clean$date <- as.Date(data_clean$date, format = "%d/%m/%y")

skim_without_charts(data_clean)

#Forward & Backward fill to fill time series missing value
# Forward fill,then, backward fill 
forward_backward_fill <- function(data_clean, col_name) {
  # Forward fill: Điền giá trị thiếu bằng giá trị trước đó
  data_clean[[col_name]] <- na.locf(data_clean[[col_name]], na.rm = FALSE)
  
  # Backward fill: Điền giá trị thiếu bằng giá trị phía sau
  data_clean[[col_name]] <- na.locf(data_clean[[col_name]], fromLast = TRUE, na.rm = FALSE)
  
  return(data_clean)
}

for (i in 1:8) {
  data_clean <- forward_backward_fill(data_clean, colnames(data_clean)[i])
}

# Check the result
skim_without_charts(data_clean)

#Scaled Data
# Create normalize function Min-Max
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply for all columns
columns_to_scale <- c('tweets', 'transaction_fee', 'transaction_value', 
                      'number_of_transaction', 'blocksize', 'blocktime', 
                      'marketcap', 'gas_limit', 'cny_usd', 'eur_usd', 
                      'jpy_usd', 'gold', 'Oil', 'Nasdaq','btc_price')

scaled_data <- data_clean
scaled_data[columns_to_scale] <- lapply(scaled_data[columns_to_scale], normalize)

# Kiểm tra kết quả
head(scaled_data)

# Vẽ chart
scaled_data_long <- gather(scaled_data, key = "Variable", value = "Value")

# Vẽ các biểu đồ con cho mỗi cột dữ liệu
# Danh sách các cột cần vẽ
columns <- c("tweets", "transaction_fee", "transaction_value", "number_of_transaction", 
             "blocksize", "blocktime", "marketcap", "price", "gas_limit", "cny_usd", 
             "eur_usd", "jpy_usd", "gold", "Oil", "Nasdaq", "btc_price")

# Tạo danh sách các biểu đồ
plots <- list()

# Tạo biểu đồ cho mỗi cột trong danh sách columns
plots <- list()

for (col in columns) {
  p <- ggplot(scaled_data, aes(x = date, y = !!sym(col))) +  
    geom_line(color = "blue") +  
    labs(title = col, x = "Year", y = "Value") +
    ylim(0, 1) +  
    theme_minimal()
  plots[[col]] <- p
}

# Sắp xếp 2 cột: 8 biểu đồ trong cột đầu tiên và 7 biểu đồ trong cột thứ hai
grid.arrange(
  # 8 biểu đồ đầu tiên trong cột trái
  plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], plots[[8]], 
  # 7 biểu đồ tiếp theo trong cột phải
  plots[[9]], plots[[10]], plots[[11]], plots[[12]], plots[[13]], plots[[14]], plots[[15]],
  ncol = 2  # Sắp xếp thành 2 cột
)

#Split into train and test dataset
set.seed(123)
train_index <- createDataPartition(scaled_data$price, p = 0.8, list = FALSE)
train_data <- scaled_data[train_index, ]
test_data <- scaled_data[-train_index, ]
train_data <- train_data[, !names(train_data) %in% "date"]
test_data <- test_data[, !names(test_data) %in% "date"]


#Check the size of train and test dataset
dim(train_data)
dim(test_data)

# Feature selection với Random Forest trên train_data
rf_model <- randomForest(price ~ ., data = train_data, importance = TRUE)

# In ra các đặc trưng quan trọng
print(rf_model)
varImpPlot(rf_model)

# Build model ANN
ann_model <- nnet(price ~ ., data = train_data, size = 10, linout = TRUE, trace = FALSE)

# Dự đoán và kiểm tra kết quả
ann_predictions <- predict(ann_model, test_data)

mse_full <- mean((ann_predictions - test_data$price)^2)
cat("MSE of full model: ", mse_full, "\n")

# Hàm tính lại MSE khi loại bỏ một đặc trưng
calculate_mse <- function(exclude_col) {
  # Loại bỏ cột (đặc trưng) khỏi dữ liệu huấn luyện và kiểm tra
  train_data_mod <- train_data[, !names(train_data) %in% exclude_col]
  test_data_mod <- test_data[, !names(test_data) %in% exclude_col]
  
  # Huấn luyện lại mô hình với các đặc trưng đã loại bỏ
  ann_model_mod <- nnet(price ~ ., data = train_data_mod, size = 10, linout = TRUE, trace = FALSE)
  
  # Dự đoán với mô hình đã huấn luyện
  ann_predictions_mod <- predict(ann_model_mod, test_data_mod)
  
  # Tính MSE cho mô hình đã thay đổi
  mse_mod <- mean((ann_predictions_mod - test_data_mod$price)^2)
  return(mse_mod)
}

# Áp dụng hàm tính MSE khi loại bỏ từng đặc trưng
features <- setdiff(names(train_data), "price")  # Loại bỏ 'Price' khỏi danh sách đặc trưng
mse_results <- sapply(features, calculate_mse)

# Tính độ thay đổi MSE khi loại bỏ từng đặc trưng
delta_mse <- mse_results - mse_full
cat("Delta MSE for each feature removal: \n")
print(delta_mse)


# Giả sử bạn đã chuẩn bị data frame 'scaled_data' và các đặc trưng cần giữ lại
selected_features <- c('marketcap', 'gas_limit', 'eur_usd', 'btc_price', 'blocksize', 'transaction_fee', 'Nasdaq', 'cny_usd')

# Tạo tập huấn luyện và kiểm tra với các đặc trưng đã chọn
train_data_selected <- train_data[, c(selected_features, 'price')]  # Thêm cột 'price' vào tập huấn luyện
test_data_selected <- test_data[, c(selected_features, 'price')]  # Thêm cột 'price' vào tập kiểm tra

# Kiểm tra kết quả
head(train_data_selected)

# Define metric to assess the performance of each model (MAE, MAPE, DA)
rmse <- function(actual, predict) {
  sqrt(mean((predicted - actual)^2, na.rm = TRUE))
}
mae <- function(actual, predicted) {
  mean(abs(predicted - actual), na.rm = TRUE)
}
mape <- function(actual, predicted) {
  mean(abs((predicted - actual) / actual), na.rm = TRUE) * 100
}
directional_accuracy <- function(actual, predicted) {
  actual_diff <- diff(actual)
  pred_diff   <- diff(predicted)
  mean(sign(actual_diff) == sign(pred_diff), na.rm = TRUE) * 100
}

# Initialize result storage
results <- data.frame()

n_iter <- 10
for (i in 1:n_iter) {
  actual <- test_data$price  # Gán actual một lần trước vòng lặp
  
  # --- ARIMA ---
  arima_model <- auto.arima(train_data$price)
  arima_pred <- forecast(arima_model, h = length(actual))$mean
  
  rmse_arima <- sqrt(mean((arima_pred - actual)^2))
  mae_arima <- mean(abs(arima_pred - actual))
  mape_arima <- mean(abs((arima_pred - actual) / actual)) * 100
  da_arima <- mean(sign(diff(arima_pred)) == sign(diff(actual)), na.rm = TRUE) * 100
  
  results <- rbind(results, data.frame(
    Model = "ARIMA",
    RMSE = rmse_arima,
    MAE = mae_arima,
    MAPE = mape_arima,
    DA = da_arima
  ))
  
  # --- SVR ---
  # Tạo dữ liệu train_svr từ giá trị lag-1
  train_svr <- data.frame(
    y = train_data$price[-1],  # Mục tiêu: giá trị tiếp theo
    x = train_data$price[-nrow(train_data)]  # Đặc trưng: giá trị trước đó (lag-1)
  )
  
  # Tạo dữ liệu test_svr cho SVR (lag-1)
  test_svr <- data.frame(
    x = c(tail(train_data$price, 1), test_data$price[-nrow(test_data)])  # Đặc trưng cho dữ liệu kiểm tra
  )
  
  # Xây dựng mô hình SVR
  svr_model <- svm(y ~ x, data = train_svr)
  
  # Dự đoán với mô hình SVR
  svr_pred <- predict(svr_model, test_svr)
  
  # Tính toán các chỉ số đánh giá
  rmse_svr <- sqrt(mean((svr_pred - actual)^2))
  mae_svr <- mean(abs(svr_pred - actual))
  mape_svr <- mean(abs((svr_pred - actual) / actual)) * 100
  da_svr <- mean(sign(diff(svr_pred)) == sign(diff(actual)), na.rm = TRUE) * 100
  
  results <- rbind(results, data.frame(
    Model = "SVR",
    RMSE = rmse_svr,
    MAE = mae_svr,
    MAPE = mape_svr,
    DA = da_svr
  ))
}

# Tính toán thống kê tóm tắt
summary_stats <- results %>%
  group_by(Model) %>%
  summarise(
    RMSE_mean = mean(RMSE), RMSE_sd = sd(RMSE), RMSE_min = min(RMSE), RMSE_max = max(RMSE),
    MAE_mean = mean(MAE), MAE_sd = sd(MAE), MAE_min = min(MAE), MAE_max = max(MAE),
    MAPE_mean = mean(MAPE), MAPE_sd = sd(MAPE), MAPE_min = min(MAPE), MAPE_max = max(MAPE),
    DA_mean = mean(DA), DA_sd = sd(DA), DA_min = min(DA), DA_max = max(DA)
  )

# Hiển thị kết quả
print(results)  # In kết quả
print(summary_stats)  # In thống kê tóm tắt


# Hiển thị kết quả
print(results)  # Sửa kết quả thành 'results' thay vì 'results_df'
print(summary_stats)


# LSTM (old price)
# Nhập số liệu vào dataframe
results_lstm <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE", "DA"),
  Mean = c(1406.583845, 871.601923, 83.410101, 43.948919),
  Std = c(0.748246, 1.036438, 0.361255, 4.136689),
  Min = c(1405.648853, 870.345017, 83.022347, 37.524558),
  Max = c(1407.789478, 873.249721, 83.972149, 49.705305)
)

# Xem kết quả dataframe
print(results_lstm)

# 5 LSTM (selected)
# Nhập số liệu vào dataframe cho LSTM (Selected Features)
results_lstm_selected <- data.frame(
  Metric = c("RMSE", "MAE", "MAPE", "DA"),
  Mean = c(148.702267, 84.479687, 20.548105, 53.770833),
  Std = c(4.786485, 2.807826, 2.626956, 2.828444),
  Min = c(142.910592, 80.586956, 17.526311, 47.500000),
  Max = c(157.051510, 89.503912, 26.371179, 56.875000)
)

# Xem kết quả dataframe
print(results_lstm_selected)

# Store RMSE
RMSE_table <- data.frame(
  Model = c("ARIMA", "SVR", "LSTM (old_price)", "LSTM (selected_feature)"),  
  RMSE_mean = c(
    summary_stats$RMSE_mean[summary_stats$Model == "ARIMA"],
    summary_stats$RMSE_mean[summary_stats$Model == "SVR"],
    1406.583845,  # RMSE_mean của LSTM (old_price)
    148.702267    # RMSE_mean của LSTM (selected_feature)
  ),
  RMSE_sd = c(
    summary_stats$RMSE_sd[summary_stats$Model == "ARIMA"], 
    summary_stats$RMSE_sd[summary_stats$Model == "SVR"], 
    0.748246,  # SD của LSTM (old_price)
    4.786485   # SD của LSTM (selected_feature)
  ),
  RMSE_min = c(
    summary_stats$RMSE_min[summary_stats$Model == "ARIMA"], 
    summary_stats$RMSE_min[summary_stats$Model == "SVR"], 
    1405.648853,  # RMSE_min của LSTM (old_price)
    142.910592    # RMSE_min của LSTM (selected_feature)
  ),
  RMSE_max = c(
    summary_stats$RMSE_max[summary_stats$Model == "ARIMA"], 
    summary_stats$RMSE_max[summary_stats$Model == "SVR"], 
    1407.789478,  # RMSE_max của LSTM (old_price)
    157.051510    # RMSE_max của LSTM (selected_feature)
  )
)

# Hiển thị kết quả
print(RMSE_table)

# Tạo bảng dài (long format) 
RMSE_table_long <- gather(RMSE_table, key = "Metric", value = "Value", RMSE_mean, RMSE_sd, RMSE_min, RMSE_max)

# Vẽ boxplot
ggplot(RMSE_table_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of RMSE Metrics for Different Models",
       x = "Model",
       y = "RMSE Value") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "pink"))


# Store MAPE
MAPE_table <- data.frame(
  Model = c("ARIMA", "SVR", "LSTM (old_price)", "LSTM (selected_feature)"),  
  MAPE_mean = c(
    summary_stats$MAPE_mean[summary_stats$Model == "ARIMA"],
    summary_stats$MAPE_mean[summary_stats$Model == "SVR"],
    83.410101 ,  # MAPE_mean của LSTM (old_price)
    20.548105     # MAPE_mean của LSTM (selected_feature)
  ),
  MAPE_sd = c(
    summary_stats$MAPE_sd[summary_stats$Model == "ARIMA"], 
    summary_stats$MAPE_sd[summary_stats$Model == "SVR"], 
    0.361255,  # SD của LSTM (old_price)
    2.626956   # SD của LSTM (selected_feature)
  ),
  MAPE_min = c(
    summary_stats$MAPE_min[summary_stats$Model == "ARIMA"], 
    summary_stats$MAPE_min[summary_stats$Model == "SVR"], 
    83.022347,  # MAPE_min của LSTM (old_price)
    17.526311    # MAPE_min của LSTM (selected_feature)
  ),
  MAPE_max = c(
    summary_stats$MAPE_max[summary_stats$Model == "ARIMA"], 
    summary_stats$MAPE_max[summary_stats$Model == "SVR"], 
    83.972149,  # MAPE_max của LSTM (old_price)
    26.371179    # MAPE_max của LSTM (selected_feature)
  )
)

# Hiển thị kết quả
print(MAPE_table)

# Tạo bảng dài (long format) 
MAPE_table_long <- gather(MAPE_table, key = "Metric", value = "Value", MAPE_mean, MAPE_sd, MAPE_min, MAPE_max)

# Vẽ boxplot
ggplot(MAPE_table_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of MAPE Metrics for Different Models",
       x = "Model",
       y = "MAPE Value") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "pink"))

# Store MAE
MAE_table <- data.frame(
  Model = c("ARIMA", "SVR", "LSTM (old_price)", "LSTM (selected_feature)"),  
  MAE_mean = c(
    summary_stats$MAE_mean[summary_stats$Model == "ARIMA"],
    summary_stats$MAE_mean[summary_stats$Model == "SVR"],
    871.60192,  # MAE_mean của LSTM (old_price)
    84.479687   # MAE_mean của LSTM (selected_feature)
  ),
  MAE_sd = c(
    summary_stats$MAE_sd[summary_stats$Model == "ARIMA"], 
    summary_stats$MAE_sd[summary_stats$Model == "SVR"], 
    1.036438,  # SD của LSTM (old_price)
    2.807826   # SD của LSTM (selected_feature)
  ),
  MAE_min = c(
    summary_stats$MAE_min[summary_stats$Model == "ARIMA"], 
    summary_stats$MAE_min[summary_stats$Model == "SVR"], 
    870.345017,  # MAE_min của LSTM (old_price)
    80.586956    # MAE_min của LSTM (selected_feature)
  ),
  MAE_max = c(
    summary_stats$MAE_max[summary_stats$Model == "ARIMA"], 
    summary_stats$MAE_max[summary_stats$Model == "SVR"], 
    873.249721,  # MAE_max của LSTM (old_price)
    89.503912    # MAE_max của LSTM (selected_feature)
  )
)

# Hiển thị kết quả
print(MAE_table)

# Tạo bảng dài (long format) 
library(tidyr)

MAE_table_long <- gather(MAE_table, key = "Metric", value = "Value", MAE_mean, MAE_sd, MAE_min, MAE_max)

# Vẽ bar chart so sánh các mô hình với MAE
library(ggplot2)

ggplot(MAE_table_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of MAE Metrics for Different Models",
       x = "Model",
       y = "MAE Value") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "pink"))


# Store DA
DA_table <- data.frame(
  Model = c("ARIMA", "SVR", "LSTM (old_price)", "LSTM (selected_feature)"),  
  DA_mean = c(
    summary_stats$DA_mean[summary_stats$Model == "ARIMA"],
    summary_stats$DA_mean[summary_stats$Model == "SVR"],
    43.948919,  # DA_mean của LSTM (old_price)
    53.770833   # DA_mean của LSTM (selected_feature)
  ),
  DA_sd = c(
    summary_stats$DA_sd[summary_stats$Model == "ARIMA"], 
    summary_stats$DA_sd[summary_stats$Model == "SVR"], 
    4.136689,  # SD của LSTM (old_price)
    2.828444   # SD của LSTM (selected_feature)
  ),
  DA_min = c(
    summary_stats$DA_min[summary_stats$Model == "ARIMA"], 
    summary_stats$DA_min[summary_stats$Model == "SVR"], 
    37.524558,  # DA_min của LSTM (old_price)
    47.500000    # DA_min của LSTM (selected_feature)
  ),
  DA_max = c(
    summary_stats$DA_max[summary_stats$Model == "ARIMA"], 
    summary_stats$DA_max[summary_stats$Model == "SVR"], 
    49.705305,  # DA_max của LSTM (old_price)
    56.875000    # DA_max của LSTM (selected_feature)
  )
)

# Hiển thị kết quả
print(DA_table)

# Tạo bảng dài (long format) 
library(tidyr)

DA_table_long <- gather(DA_table, key = "Metric", value = "Value", DA_mean, DA_sd, DA_min, DA_max)

# Vẽ bar chart so sánh các mô hình với DA
library(ggplot2)

ggplot(DA_table_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of DA Metrics for Different Models",
       x = "Model",
       y = "DA Value") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "pink"))


update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::reinstall_tinytex()

