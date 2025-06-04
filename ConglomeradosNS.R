# ---------------------------------------------
# Librerías usadas
library(readxl)     # Para leer Excel
library(dplyr)      # Manipulación de data frames
library(ggplot2)    # Gráficos
library(corrplot)   # Matriz de correlaciones
library(scales)     # Para formatear ejes (e.g. comas en miles)
library(tidyverse)
library(factoextra) # Para visualización de PCA
library(MASS) 
library(pracma)
library(GGally)          # Para scatter plot usando ggplot2
library(here)
library(cluster)

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
    PHour    = as.integer(PHour),
    Reach            = as.integer(Reach),
    Impressions      = as.integer(Impressions),
    EUsers     = as.integer(EUsers),
    PConsumers    = as.integer(PConsumers),
    PConsumptions    = as.integer(PConsumptions),
    PIPLP    = as.integer(PIPLP),
    PRPLP    = as.integer(PRPLP),
    PPLE = as.integer(PPLE),
    Interactions    = as.integer(Interactions)
  ) 


newReach<-df2$Reach-df2$PRPLP
newImpresiones<-df2$Impressions-df2$PIPLP
df2$NReach<-newReach
df2$NImpressions<-newImpresiones
# Seleccionar variables numéricas para clustering
vars_num_cluster <- df2[,c(1,10,11,15,19,20,21)] 

# Escalamiento de variables
vars_scaled <- scale(vars_num_cluster)

# Determinar el número óptimo de clusters con el método del codo
fviz_nbclust(df_scaled, kmeans, method = "wss")

# k-means con 3 clusters
set.seed(123)
k3 <- kmeans(vars_scaled, centers = 3, nstart = 25)
k3$centers

# Añadir clusters al dataframe original, filtrando filas con datos completos
df_cluster <- df2 %>%
  mutate(Cluster = factor(k3$cluster))

# Tabla de contingencia: Cluster vs Tipo de publicación
print(table(df_cluster$Type, df_cluster$Cluster))

# Tabla de contingencia: Cluster vs Categoría
print(table(df_cluster$Category, df_cluster$Cluster))

# Tabla de contingencia: Cluster vs Paid
print(table(df_cluster$Paid, df_cluster$Cluster))

# Tabla de contingencia: Cluster vs Pagada
print(table(df_cluster$Paid, df_cluster$Cluster))

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

# Gráfico: Distribución de clusters por publicidad pagada
ggplot(df_cluster, aes(x = Paid, fill = Cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de clusters por pago de publicación", y = "Proporción") +
  theme_minimal()
