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
