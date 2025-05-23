#2,1.2
library(readr) 
gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                          locale(encoding = "UTF-8"))
print("Первые строки загруженных данных:")
print(head(df_expenses))
print("Структура данных:")
print(str(df_expenses))
print("Размерность данных (количество строк, количество столбцов):")
print(dim(df_expenses))

#2.1.3
library(dplyr)
gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                          locale(encoding = "UTF-8"))
turbine_numeric_features_ru <- c(
  'AmbientTemperature_C',
  'AmbientPressure_kPa',
  'RelativeHumidity_percent',
  'FuelFlowRate_kg_s',
  'CompressorInletTemp_C',
  'CompressorEfficiency_percent',
  'TurbineEfficiency_percent',
  'GasTurbineAge_years'
)
data_for_clustering <- gas_turbine_data %>%
  select(all_of(turbine_numeric_features_ru))
scaled_data <- scale(data_for_clustering)
scaled_df <- as.data.frame(scaled_data)
print("Описательная статистика масштабированных данных:")
print(summary(scaled_df))

#2.2.1
library(ggplot2)
library(tidyr)
library(dplyr)

gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                               locale(encoding = "UTF-8"))

turbine_numerical_features <- c(
  'AmbientTemperature_C',
  'AmbientPressure_kPa',
  'RelativeHumidity_percent',
  'FuelFlowRate_kg_s',
  'CompressorInletTemp_C',
  'CompressorEfficiency_percent',
  'TurbineEfficiency_percent',
  'GasTurbineAge_years',
  'PowerOutput_MW'
)

df_turbine_long <- gas_turbine_data %>%
  select(all_of(turbine_numerical_features)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Параметр",
    values_to = "Значение"
  )

ggplot(df_turbine_long, aes(x = Значение)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Параметр, scales = "free") +
  theme_minimal(base_size = 10) +
  labs(
    title = "Распределение числовых параметров газовой турбины",
    x = "Значение параметра",
    y = "Частота"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    strip.text = element_text(size = 8)
  )

# 2.2.1(boxplot)
library(ggplot2)
gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                               locale(encoding = "UTF-8"))
ggplot(gas_turbine_data, aes(x = MaintenanceCycle, y = PowerOutput_MW, fill = MaintenanceCycle)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Распределение выработки электроэнергии по стадиям ТО (Боксплоты)",
    x = "Стадия цикла ТО",
    y = "Выработка электроэнергии (МВт)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# 2.2.2(Кореляционный анализ)
library(ggplot2)
library(dplyr)
library(reshape2)
gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                               locale(encoding = "UTF-8"))
turbine_numerical_features_for_corr <- gas_turbine_data %>%
  select(where(is.numeric))

correlation_matrix <- cor(turbine_numerical_features_for_corr, use = "complete.obs")

melted_corr_matrix <- melt(correlation_matrix)

ggplot(melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0,
                       limit = c(-1, 1),
                       space = "Lab",
                       name="Корреляция\nПирсона") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_fixed() +
  labs(
    title = "Тепловая карта корреляций параметров газовой турбины",
    x = "",
    y = ""
  ) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3)

#2.3.1(Описатлеьная статистика)
library(dplyr)
gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                               locale(encoding = "UTF-8"))
expense_categories_ru <- c('AmbientTemperature_C',
                           'AmbientPressure_kPa',
                           'RelativeHumidity_percent',
                           'FuelFlowRate_kg_s',
                           'CompressorInletTemp_C',
                           'CompressorEfficiency_percent',
                           'TurbineEfficiency_percent',
                           'GasTurbineAge_years',
                           'PowerOutput_MW')
gas_turbine_data %>%
select(all_of(expense_categories_ru)) %>%
summary()

#2.3.2
gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                               locale(encoding = "UTF-8"))
gas_turbine_data$FuelFlowRate_kg_s <- factor(gas_turbine_data$FuelFlowRate_kg_s,
                                            levels = c("Early", "Mid", "Late"))

anova_model <- aov(PowerOutput_MW ~ FuelFlowRate_kg_s, data = gas_turbine_data)

summary_anova <- summary(anova_model)
print(summary_anova)

if (summary_anova[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_results <- TukeyHSD(anova_model)
  print(tukey_results)
  # plot(tukey_results) # Раскомментируйте для графика
} else {
  print("ANOVA не показала статистически значимых различий между группами, пост-хок тест не требуется.")
}

#2.4.2 (Метод локтя и силуэтов)
library(factoextra)
library(ggplot2)

gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                               locale(encoding = "UTF-8"))
turbine_numeric_features_for_clustering <- c(
  'AmbientTemperature_C',
  'AmbientPressure_kPa',
  'RelativeHumidity_percent',
  'FuelFlowRate_kg_s',
  'CompressorInletTemp_C',
  'CompressorEfficiency_percent',
  'TurbineEfficiency_percent',
  'GasTurbineAge_years'
)

data_for_clustering <- gas_turbine_data %>%
  select(all_of(turbine_numeric_features_ru))
scaled_data <- scale(data_for_clustering)
scaled_df <- as.data.frame(scaled_data)

plot_elbow <- fviz_nbclust(scaled_data, kmeans, method = "wss") +
  labs(title = "Метод локтя для определения оптимального числа кластеров (газовая турбина)",
       subtitle = "Сумма квадратов внутри кластеров (WCSS)",
       x = "Количество кластеров k",
       y = "Общая сумма квадратов внутри кластеров") +
  theme_minimal()
print(plot_elbow)

plot_silhouette <- fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
  labs(title = "Метод силуэта для определения оптимального числа кластеров (газовая турбина)",
       subtitle = "Средняя ширина силуэта",
       x = "Количество кластеров k",
       y = "Средняя ширина силуэта") +
  theme_minimal()
print(plot_silhouette)

# Тест кластеризации
gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                               locale(encoding = "UTF-8"))
turbine_numeric_features_for_clustering <- c(
  'AmbientTemperature_C',
  'AmbientPressure_kPa',
  'RelativeHumidity_percent',
  'FuelFlowRate_kg_s',
  'CompressorInletTemp_C',
  'CompressorEfficiency_percent',
  'TurbineEfficiency_percent',
  'GasTurbineAge_years'
)

data_for_clustering <- gas_turbine_data %>%
  select(all_of(turbine_numeric_features_ru))
scaled_data <- scale(data_for_clustering)
scaled_df <- as.data.frame(scaled_data)

k_chosen <- 3
set.seed(123) # Можете использовать любое число
kmeans_result <- kmeans(scaled_data, centers = k_chosen, nstart = 25)
gas_turbine_data_with_clusters <- gas_turbine_data
gas_turbine_data_with_clusters$cluster <- as.factor(kmeans_result$cluster)

anova_power_vs_clusters <- aov(PowerOutput_MW ~ cluster, data = gas_turbine_data_with_clusters)
summary_anova_power <- summary(anova_power_vs_clusters)
print("--- ANOVA: PowerOutput_MW по Кластерам ---")
print(summary_anova_power)

# Кластеризация
gas_turbine_data <- read_csv("gas_turbine_power_dataset.csv", locale =
                              locale(encoding = "UTF-8"))
turbine_numeric_features_for_clustering <- c(
  'AmbientTemperature_C',
  'AmbientPressure_kPa',
  'RelativeHumidity_percent',
  'FuelFlowRate_kg_s',
  'CompressorInletTemp_C',
  'CompressorEfficiency_percent',
  'TurbineEfficiency_percent',
  'GasTurbineAge_years'
)

data_for_clustering <- gas_turbine_data %>%
  select(all_of(turbine_numeric_features_ru))
scaled_data <- scale(data_for_clustering)
scaled_df <- as.data.frame(scaled_data)

set.seed(123)
optimal_k <- 3
kmeans_result <- kmeans(scaled_data, centers = optimal_k, nstart
                        = 25)
print("Результаты выполнения алгоритма k-means:")
print(kmeans_result)

# Датафрейм с метками
gas_turbine_data$cluster <- kmeans_result$cluster
gas_turbine_data$cluster <- as.factor(gas_turbine_data$cluster)
print("Первые строки исходных данных с добавленными метками
кластеров:")
print(head(gas_turbine_data))

# Визуализация
library(factoextra)
library(ggplot2)
plot_clusters_pca <- fviz_cluster(kmeans_result, data =
                                    scaled_data,
                                  palette = "viridis",
                                  ggtheme = theme_minimal(),
                                  main = "Визуализация кластеров потребителей
(PCA)")
print(plot_clusters_pca)