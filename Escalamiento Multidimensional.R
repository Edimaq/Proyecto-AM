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

# Escalamiento Multidimensional
#Elegimos las variables numericas de mayor importancia
df_numeric <- df2 %>% select(PLikes,Reach,Impressions,EUsers,PConsumers,PPLE,comment,like,share)

#Estandarizamos
df_numeric <- data.matrix(df_numeric)
df_scaled <- scale(df_numeric)

#Calcular matriz de distancias Euclidianas
dist_matrix <- dist(df_scaled, method = "euclidean")

#Usamos MDS clásica con cmdscale para 2 dimensiones 
mds_res <- cmdscale(dist_matrix, k = 2, eig = TRUE)

#Extraer coordenadas en 2D
mds_coords <- as.data.frame(mds_res$points)
colnames(mds_coords) <- c("Dim1", "Dim2")

#Usamos Type, Category y Paid para colorear
mds_coords$Type <- df2$Type
mds_coords$Category <- df2$Category
mds_coords$Paid <- df2$Paid

# Porcentaje de varianza en dos dimensiones
eig_vals <- mds_res$eig
var_explained <- eig_vals / sum(abs(eig_vals))
pct_dim1 <- round(var_explained[1] * 100, 2)
pct_dim2 <- round(var_explained[2] * 100, 2)

#Resultados para Type
ggplot(mds_coords, aes(x = Dim1, y = Dim2, color = Type)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "MDS Clásico en Publicaciones de Facebook",
    subtitle = paste0("Dim1: ", pct_dim1, "% varianza | Dim2: ", pct_dim2, "% varianza"),
    x = paste0("Dimensión 1 (", pct_dim1, "%)"),
    y = paste0("Dimensión 2 (", pct_dim2, "%)")
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9))

#Resultados para Category
ggplot(mds_coords, aes(x = Dim1, y = Dim2, color = Category)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "MDS Clásico en Publicaciones de Facebook",
    subtitle = paste0("Dim1: ", pct_dim1, "% varianza | Dim2: ", pct_dim2, "% varianza"),
    x = paste0("Dimensión 1 (", pct_dim1, "%)"),
    y = paste0("Dimensión 2 (", pct_dim2, "%)")
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9))

#resultados para Paid
ggplot(mds_coords, aes(x = Dim1, y = Dim2, color = Paid)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "MDS Clásico en Publicaciones de Facebook",
    subtitle = paste0("Dim1: ", pct_dim1, "% varianza | Dim2: ", pct_dim2, "% varianza"),
    x = paste0("Dimensión 1 (", pct_dim1, "%)"),
    y = paste0("Dimensión 2 (", pct_dim2, "%)")
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9))

k3 <- kmeans(vars_scaled, centers = 3, nstart = 25)
k3$centers

# Añadir clusters al dataframe original, filtrando filas con datos completos
df_cluster <- df2 %>%
  mutate(Cluster = factor(k3$cluster))
mds_coords$Cluster <- df_cluster$Cluster
#resultados para el cluster
ggplot(mds_coords, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "MDS Clásico en Publicaciones de Facebook",
    subtitle = paste0("Dim1: ", pct_dim1, "% varianza | Dim2: ", pct_dim2, "% varianza"),
    x = paste0("Dimensión 1 (", pct_dim1, "%)"),
    y = paste0("Dimensión 2 (", pct_dim2, "%)")
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9))

