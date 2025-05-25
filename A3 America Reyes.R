# ---
# Título: "Análisis de Rendimiento Académico"
# Autor: "América Itzel Reyes Alatorre"
# ---

# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Cargar datos
data <- read.csv("StudentsPerformance.csv")

# 2. Exploración inicial
summary(data) 
str(data)      

# 3. Manejo de valores faltantes 
sum(is.na(data))  
# Opcional: Eliminar filas con NAs 
data_clean <- na.omit(data)

# 4. Transformaciones con dplyr
data_clean <- data %>%
  rename(
    parental_education = `parental.level.of.education`, 
    lunch_type = lunch                                  
  ) %>%
  mutate(
    gender = as.factor(gender),
    parental_education = factor(parental_education,
                                levels = c("some high school", "high school", "some college",
                                           "associate's degree", "bachelor's degree", "master's degree")),
    lunch_type = as.factor(lunch_type), 
    avg_score = (math.score + reading.score + writing.score) / 3
  )

# 5. Estadísticos por grupo 
data_clean %>%
  group_by(gender) %>%
  summarise(
    mean_math = mean(math.score),
    median_math = median(math.score)
  )

data_clean %>%
  group_by(lunch_type) %>%
  summarise(
    mean_math = mean(math.score),
    mean_reading = mean(reading.score),
    mean_writing = mean(writing.score),
    count = n()
  )

# 6. Visualizaciones

ggplot(data_clean, aes(x = parental_education, y = math.score, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Rendimiento en matemáticas por género y educación de los padres",
    x = "Nivel educativo de los padres",
    y = "Puntaje de matemáticas",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_clean, aes(x = math.score, y = reading.score, color = lunch_type)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Relación entre puntajes de matemáticas y lectura",
    x = "Matemáticas",
    y = "Lectura",
    color = "Tipo de almuerzo"
  ) +
  scale_color_manual(values = c("red", "blue"))  

ggplot(data_clean, aes(x = lunch_type, y = math.score, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Rendimiento en matemáticas por tipo de almuerzo y género",
    x = "Tipo de almuerzo",
    y = "Puntaje de matemáticas",
    fill = "Género"
  ) +
  theme_minimal()

# 4. Conclusiones breves
# Los boxplots muestran que las mujeres tienden a obtener mejores puntajes en lectura, mientras que los hombres destacan en matemáticas. El nivel educativo de los padres y el almuerzo parece influir positivamente en el rendimiento.