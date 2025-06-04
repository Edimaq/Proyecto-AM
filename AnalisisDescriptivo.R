# Librerías usadas
library(readxl)     # Para leer Excel
library(dplyr)      # Manipulación de data frames
library(ggplot2)    # Gráficos
library(corrplot)   # Matriz de correlaciones
library(scales)     # Para formatear ejes (e.g. comas en miles)
library(tidyverse)
library(factoextra) # Para visualización de PCA



# ---------------------------------------------
# Leemos el archivo de datos, cambiar la ruta
file <-"C:\\Users\\edima\\Downloads\\dataset_Facebook.xlsx" #Cambiarlo para cada equipo
df <- data.frame(read_excel(file))

head(df)

# Convertimos variables categóricas a factores y renombramos algunas variables 
df2 <- df %>%
  rename(
    PLikes        = `Page.total.likes`,
    PMonth        = `Post.Month`,
    PWeekday      = `Post.Weekday`,
    PHour         = `Post.Hour`,
    Reach            = `Lifetime.Post.Total.Reach`,
    Impressions      = `Lifetime.Post.Total.Impressions`,
    EUsers     = `Lifetime.Engaged.Users`,
    PConsumers    = `Lifetime.Post.Consumers`,
    PConsumptions    = `Lifetime.Post.Consumptions`,
    PIPLP    = `Lifetime.Post.Impressions.by.people.who.have.liked.your.Page`,
    PRPLP    = `Lifetime.Post.reach.by.people.who.like.your.Page`,
    PPLE = `Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post`,
    Interactions    = `Total.Interactions`
  ) %>%
  mutate(
    Type      = as.factor(Type),
    Category  = as.factor(Category),
    Paid      = as.factor(Paid),
    PMonth   = as.integer(PMonth),
    PWeekday = as.integer(PWeekday),
    PHour    = as.integer(PHour)
  ) 


# ---------------------------------------------
# 3. Estadísticas descriptivas básicas
# ---------------------------------------------
summary(df2[,8:19])

# Distribución de "Interacciones" (Total Interactions)
ggplot(df2, aes(x = Interactions)) +
  geom_histogram(binwidth = 20, fill = "steelblue", color = "white", alpha = 0.5) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Histograma de Total de Interacciones en las Publicaciones",
    x = "Total de Interacciones (comentarios + likes + shares)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Interpretación:
# - Observamos que la mayoría de publicaciones concentran pocos decenas o centenas de interacciones.
# - Hay una cola larga hacia la derecha, indicando publicaciones excepcionalmente virales.

# Boxplot de “Interacciones” por tipo de publicación (Type)
ggplot(df2, aes(x = Type, y = Interactions, fill = Type)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1, alpha = 0.6) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Distribución de Interacciones según el Tipo de Publicación",
    x = "Tipo de Publicación",
    y = "Total de Interacciones"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Interpretación:
# - “Photo” tiende a tener mayor mediana de interacciones que “Link” o “Status”.
# - Hay outliers (publicaciones muy exitosas) en todos los tipos, pero más pronunciados en “Photo”.

# Boxplot de “Interacciones” por categoría (Category)
ggplot(df2, aes(x = Category, y = Interactions, fill = Category)) +
  geom_boxplot(outlier.color = "darkorange", outlier.size = 1, alpha = 0.6) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Distribución de Interacciones según la Categoría de la Página",
    x = "Categoría",
    y = "Total de Interacciones"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Interpretación:
# - Algunas categorías (e.g., categoría 2 o 3) pueden mostrar medianas de interacción ligeramente distintas.
# - Nuevamente, existen outliers que indican publicaciones con altísimo engagement.

# ---------------------------------------------
# 4. Análisis temporal: Meses, Días y Horas
# ---------------------------------------------
# Agrupar promedio de interacciones por mes
inter_by_month <- df2 %>%
  group_by(PMonth) %>%
  summarise(
    PromedioInteracciones = mean(Interactions),
    MedianaInteracciones  = median(Interactions),
    Conteo               = n()
  )

ggplot(inter_by_month, aes(x = factor(PMonth), y = PromedioInteracciones, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "Mayo", "Jun", "Jul","Ago","Sep","Oct","Nov","Dic"))+
  labs(title = "Promedio de Interacciones por Mes de Publicación",x = "Mes",
    y = "Promedio de Interacciones") +
  theme_minimal()

# Promedio de interacciones por día de la semana
inter_by_weekday <- df2 %>%
  group_by(PWeekday) %>%
  summarise(
    PromedioInteracciones = mean(Interactions),
    MedianaInteracciones  = median(Interactions),
    Conteo               = n()
  )

ggplot(inter_by_weekday, aes(x = factor(PWeekday), y = PromedioInteracciones, group = 1)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")) +
  labs(
    title = "Promedio de Interacciones por Día de la Semana",
    x = "Día de la Semana",
    y = "Promedio de Interacciones"
  ) +
  theme_minimal()

# Promedio de interacciones por hora del día
inter_by_hour <- df2 %>%
  group_by(PHour) %>%
  summarise(
    PromedioInteracciones = mean(Interactions, na.rm = TRUE),
    Conteo               = n()
  )

ggplot(inter_by_hour, aes(x = PHour, y = PromedioInteracciones)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Promedio de Interacciones por Hora de Publicación",
    x = "Hora del Día",
    y = "Promedio de Interacciones"
  ) +
  theme_minimal()


# ---------------------------------------------
# 5. Correlaciones entre variables cuantitativas
# ---------------------------------------------
# Seleccionar variables numéricas de interés
vars_num <- df2 %>%
  select(PLikes, Reach, Impressions, EUsers, PConsumers, 
         like, comment, share)

# Cambiar nombres de columnas para que sean más legibles en la matriz
colnames(vars_num) <- c("PageLikes", "Reach", "Impressions", "EUsers", 
                        "PConsumers", "Likes", "Comments", "Shares")

# Calcular matriz de correlación (omitimos filas con NA)
mat_cor <- cor(vars_num, use = "pairwise.complete.obs")

# Visualizar matriz de correlaciones
corrplot(mat_cor, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "white", number.cex = 0.7,
         title = "Matriz de Correlaciones entre Variables Clave",
         mar = c(0,0,1,0))

# Interpretación:
# - “Likes”, “Comments” y “Shares” están muy correlacionados con “Interacciones” (lógicamente).
# - “Reach” e “Impressions” también muestran correlación positiva moderada-alta con “Interacciones”.
# - “PageLikes” (likes de la página) puede tener correlación más débil, indicando que páginas grandes no siempre garantizan interacciones altas.

# ---------------------------------------------
# 6. Gráficos más específicos
# ---------------------------------------------
# 6.1. Scatterplot: Reach vs Interacciones, coloreado por tipo de publicación
ggplot(df2, aes(x = Reach, y = Interactions, color = Type)) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Relación entre Alcance (Reach) e Interacciones",
    x = "Alcance (Reach)",
    y = "Interacciones",
    color = "Tipo"
  ) +
  theme_minimal()

# Interpretación:
# - A mayor alcance, por lo general más interacciones, pero hay dispersión: algunas publicaciones con poco alcance reciben muchas interacciones (contenido muy atractivo), y viceversa.
# - Comparar si “Photo” o “Link” se comportan distinto en esta relación.

# 6.2. Gráfico de barras: Proporción de publicaciones pagadas vs no pagadas
df_pago <- df2 %>%
  group_by(Paid) %>%
  summarise(Conteo = n())

ggplot(df_pago, aes(x = Paid, y = Conteo, fill = Paid)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels = c("No", "Si"))+
  labs(
    title = "Número de Publicaciones: Pagadas vs Orgánicas",
    x = "¿Fue publicación pagada?",
    y = "Número de Publicaciones"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Interpretación:
# - Permite ver cuántas publicaciones se impulsaron con pago (‘Paid’ = 1) y cuántas fueron orgánicas (‘Paid’ = 0).
# - Si la proporción de pagadas es muy baja, podría haber oportunidad de promocionar más contenido.

# 6.3. Boxplot: Interacciones en publicaciones pagadas vs orgánicas
ggplot(df2, aes(x = Paid, y = Interactions, fill = Paid)) +
  geom_boxplot(outlier.color = "red", alpha = 0.6) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("No", "Si"))+
  labs(
    title = "Comparación de Interacciones: Pagadas vs No pagadas",
    x = "Publicidad pagada",
    y = "Interacciones"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ---------------------------------------------
# 7. Clustering (k-means) para segmentación de publicaciones
# ---------------------------------------------

# Seleccionar variables numéricas para clustering
vars_num_cluster <- df2 %>%
  select(Reach, Impressions, EUsers, PConsumers, Interactions) %>%
  filter(complete.cases(.)) # eliminar NAs

# Escalamiento de variables
vars_scaled <- scale(vars_num_cluster)

# k-means con 3 clusters
set.seed(123)
k3 <- kmeans(vars_scaled, centers = 3, nstart = 25)

# Añadir clusters al dataframe original, filtrando filas con datos completos
df_cluster <- df2 %>%
  filter(complete.cases(Reach, Impressions, EUsers, PConsumers, Interactions)) %>%
  mutate(Cluster = factor(k3$cluster))

# Tabla de contingencia: Cluster vs Tipo de publicación
print(table(df_cluster$Type, df_cluster$Cluster))

# Tabla de contingencia: Cluster vs Categoría
print(table(df_cluster$Category, df_cluster$Cluster))

# Gráfico: Distribución de clusters por Tipo
ggplot(df_cluster, aes(x = Type, fill = Cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de clusters por Tipo de publicación", y = "Proporción") +
  theme_minimal()

# Gráfico: Distribución de clusters por Categoría
ggplot(df_cluster, aes(x = Category, fill = Cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de clusters por Categoría de publicación", y = "Proporción") +
  theme_minimal()

