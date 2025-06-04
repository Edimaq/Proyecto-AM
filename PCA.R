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
# Componentes principales 
# Seleccionamos variables numericas de importancia
df_pca<- df2 %>%
  select(PLikes,Reach,Impressions,EUsers,PConsumers,PPLE,comment,like,share)
df_scaled <- scale(df_pca)

# Usamos prcomp para calcular componentes
pca_res <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# Varianza de cada componente
var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)
cum_var_explained <- cumsum(var_explained)

variance_df <- data.frame(
  PC = paste0("PC", 1:length(var_explained)),
  Variance_Explained = var_explained,
  Cumulative = cum_var_explained
)

print(variance_df)

# Visualizamos varianza y usamos regla del codo
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 60)) +
  labs(
    title = "Scree Plot: Varianza Explicada por Componente",
    x = "Componentes Principales",
    y = "Porcentaje de Varianza Explicada"
  )

# Cargas de las primeras 3 componentes
loadings <- pca_res$rotation[, 1:3]
print(round(loadings, 3))


# Biplot sobre las primeras dos componentes
fviz_pca_biplot(
  pca_res,
  geom.ind = "point",
  pointshape = 21,
  pointsize = 1.5,
  fill.ind = "cyan",
  col.var = "magenta",
  repel = TRUE
) +
  labs(
    title = "Biplot PCA (PC1 vs PC2)",
    subtitle = "Muestra y direcciones de variables"
  ) +
  theme_minimal()
