import pandas as pd
import numpy as np

# Задаем количество строк
n_samples = 150

# Устанавливаем seed для воспроизводимости
np.random.seed(42)

# Генерация данных
data = {
    'AmbientTemperature_C': np.random.uniform(5, 35, n_samples), # от 5 до 35 °C
    'AmbientPressure_kPa': np.random.uniform(98, 103, n_samples), # от 98 до 103 кПа
    'RelativeHumidity_percent': np.random.uniform(20, 90, n_samples), # от 20% до 90%
    'FuelFlowRate_kg_s': np.random.uniform(10, 30, n_samples), # от 10 до 30 кг/с
    'GasTurbineAge_years': np.random.randint(1, 21, n_samples) # от 1 до 20 лет
}

df = pd.DataFrame(data)

# CompressorInletTemp_C - может быть чуть выше AmbientTemperature_C из-за потерь или чуть ниже из-за охлаждения
df['CompressorInletTemp_C'] = df['AmbientTemperature_C'] + np.random.normal(0, 2, n_samples)
df['CompressorInletTemp_C'] = np.clip(df['CompressorInletTemp_C'], 0, 40) # Ограничим

# Эффективности компрессора и турбины зависят от возраста и случайных факторов
# Базовая эффективность + снижение с возрастом + шум
df['CompressorEfficiency_percent'] = 92 - (df['GasTurbineAge_years'] * 0.3) + np.random.normal(0, 1.5, n_samples)
df['CompressorEfficiency_percent'] = np.clip(df['CompressorEfficiency_percent'], 80, 95)

df['TurbineEfficiency_percent'] = 95 - (df['GasTurbineAge_years'] * 0.4) + np.random.normal(0, 1.5, n_samples)
df['TurbineEfficiency_percent'] = np.clip(df['TurbineEfficiency_percent'], 82, 98)

# MaintenanceCycle - категориальный
maintenance_options = ['Early', 'Mid', 'Late']
# Сделаем так, чтобы старые турбины чаще были в 'Late' цикле (условно)
probs = []
for age in df['GasTurbineAge_years']:
    if age < 7:
        probs.append([0.6, 0.3, 0.1]) # Early, Mid, Late
    elif age < 14:
        probs.append([0.3, 0.4, 0.3])
    else:
        probs.append([0.1, 0.3, 0.6])
df['MaintenanceCycle'] = [np.random.choice(maintenance_options, p=p) for p in probs]

base_power = df['FuelFlowRate_kg_s'] * 5.5 + 10

# Коррекции
temp_effect = (20 - df['CompressorInletTemp_C']) * 0.3  # Холоднее = лучше
pressure_effect = (df['AmbientPressure_kPa'] - 100) * 0.5 # Выше давление = лучше
humidity_effect = (50 - df['RelativeHumidity_percent']) * 0.05 # Суше = чуть лучше
comp_eff_effect = (df['CompressorEfficiency_percent'] - 88) * 0.8 # Выше КПД компр = лучше
turb_eff_effect = (df['TurbineEfficiency_percent'] - 90) * 1.0 # Выше КПД турб = лучше
age_effect = -df['GasTurbineAge_years'] * 0.1 # Старше = хуже

maintenance_modifier = pd.Series(index=df.index, dtype=float)
maintenance_modifier[df['MaintenanceCycle'] == 'Early'] = 5
maintenance_modifier[df['MaintenanceCycle'] == 'Mid'] = 0
maintenance_modifier[df['MaintenanceCycle'] == 'Late'] = -8

df['PowerOutput_MW'] = (base_power +
                        temp_effect +
                        pressure_effect +
                        humidity_effect +
                        comp_eff_effect +
                        turb_eff_effect +
                        age_effect +
                        maintenance_modifier +
                        np.random.normal(0, 3, n_samples)) # Случайный шум

# Ограничим выработку разумными пределами
df['PowerOutput_MW'] = np.clip(df['PowerOutput_MW'], 30, 200) # от 30 до 200 МВт

# Округление для читаемости
cols_to_round = {
    'AmbientTemperature_C': 1, 'AmbientPressure_kPa': 2, 'RelativeHumidity_percent': 1,
    'FuelFlowRate_kg_s': 2, 'CompressorInletTemp_C': 1,
    'CompressorEfficiency_percent': 2, 'TurbineEfficiency_percent': 2, 'PowerOutput_MW': 2
}
for col, precision in cols_to_round.items():
    df[col] = df[col].round(precision)

# Упорядочим колонки для логичности
column_order = [
    'AmbientTemperature_C', 'AmbientPressure_kPa', 'RelativeHumidity_percent',
    'FuelFlowRate_kg_s', 'CompressorInletTemp_C', 'CompressorEfficiency_percent',
    'TurbineEfficiency_percent', 'GasTurbineAge_years', 'MaintenanceCycle',
    'PowerOutput_MW'
]
df = df[column_order]

# Сохранение в CSV
csv_filename = "gas_turbine_power_dataset.csv"
df.to_csv(csv_filename, index=False, encoding='utf-8')
