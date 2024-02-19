## ----------------------------------------------------------------------------------------------------------------------
df <- read.csv('../data/dataset_movie_info.csv', dec=',')
str(df)


## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)

# Convertimos la columna de géneros en un formato largo,
# donde cada fila es una película con un único género
df_long <- df %>%
  separate_rows(Género, sep = ",") %>%  # Separamos los géneros por comas
  mutate(Género = trimws(Género))       # Eliminamos espacios en blanco

# Ahora, creamos una lista de géneros únicos
unique_genres <- unique(df_long$Género)

# Creamos variables dummy para cada género único
# Cada película tendrá un 1 si pertenece a ese género, y un 0 si no pertenece
for(genre in unique_genres) {
  df[[genre]] <- ifelse(grepl(genre, df$Género), 1, 0)
}


## ----------------------------------------------------------------------------------------------------------------------
df_long <- df %>%
  separate_rows(Director, sep = ",") %>%
  mutate(Director = trimws(Director))

# Contamos el número de películas que ha dirigido (o co-dirigido) cada director
director_counts <- table(df_long$Director)

# Identificamos los directores que han dirigido al menos 4 películas
frequent_directors <- which(director_counts >= 4)

# Extraemos los nombres de estos directores
frequent_directors_list <- names(frequent_directors)

for(director in frequent_directors_list) {
  df[[director]] <- ifelse(grepl(director, df$Director), 1, 0)
}


## ----------------------------------------------------------------------------------------------------------------------
df_long <- df %>%
  separate_rows(Reparto, sep = ",") %>%
  mutate(Reparto = trimws(Reparto))

actor_counts <- table(df_long$Reparto)
frequent_actors <- which(actor_counts >= 8)
frequent_actors_list <- names(frequent_actors)

for(actor in frequent_actors_list) {
  df[[actor]] <- ifelse(grepl(actor, df$Reparto), 1, 0)
}


## ----------------------------------------------------------------------------------------------------------------------
df <- select(df, -Año, -Género, -Director, -Reparto, -Título, -Título.Original,
             -Número.de.Puntuaciones, -Sinopsis, -Enlace)


## ----------------------------------------------------------------------------------------------------------------------
str(df)


## ----------------------------------------------------------------------------------------------------------------------
df <- select(df, -V48)


## ----------------------------------------------------------------------------------------------------------------------
sum(is.na(df))


## ----------------------------------------------------------------------------------------------------------------------
sum(df=="")


## ----------------------------------------------------------------------------------------------------------------------
colSums(df==0)


## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------------
library(ggplot2)
library(grid)
library(gridExtra)

duration_plot <- ggplot(df, aes(x = "", y = Duración)) +
  geom_violin(fill = "seagreen", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  labs(x = "",
       y = "Duración (minutos)",
       title = "Duración") +
  theme_minimal()

rating_plot <- ggplot(df, aes(x = "", y = Puntuación.Media)) +
  geom_violin(fill = "steelblue", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  labs(x = "",
       y = "Puntuación (sobre 10)",
       title = "Puntuación media") +
  theme_minimal()

grid.arrange(duration_plot, rating_plot, ncol = 2)


## ----------------------------------------------------------------------------------------------------------------------
sort(boxplot.stats(df$Duración)$out)


## ----------------------------------------------------------------------------------------------------------------------
sort(boxplot.stats(df$Puntuación.Media)$out)


## ----------------------------------------------------------------------------------------------------------------------
animation_ratings <- df[df$Animación==1, "Puntuación.Media"]
non_animation_ratings <- df[df$Animación==0, "Puntuación.Media"]


## ----------------------------------------------------------------------------------------------------------------------
shapiro.test(df$Puntuación.Media)
shapiro.test(df$Duración)
shapiro.test(animation_ratings)
shapiro.test(non_animation_ratings)


## ----------------------------------------------------------------------------------------------------------------------
library(stats)
fligner.test(list(animation_ratings, non_animation_ratings))


## ----------------------------------------------------------------------------------------------------------------------
cor.test(df$Duración, df$Puntuación.Media, method="kendall")


## ----------------------------------------------------------------------------------------------------------------------
ggplot(data=df, aes(Duración, Puntuación.Media)) +
  geom_jitter(color="steelblue", alpha=0.3) +
  theme_minimal() +
  labs(title = "Puntuación media de películas según duración",
       x = "Duración",
       y = "Puntuación Media")


## ----------------------------------------------------------------------------------------------------------------------
table(factor(df$Animación, levels=c(0,1), labels=c("No animación", "Animación")))


## ----------------------------------------------------------------------------------------------------------------------
t.test(animation_ratings, non_animation_ratings, alternative = "greater", var.equal = FALSE)


## ----------------------------------------------------------------------------------------------------------------------
ggplot(df,
       aes(x = factor(Animación, levels=c(0,1), labels=c("No animación", "Animación")),
           y = Puntuación.Media)) +
    geom_violin(fill = "steelblue", color = "black", trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    labs(x = "",
         y = "Puntuación media",
         title = "Puntuación media de las películas según animación") +
    theme_minimal()


## ----------------------------------------------------------------------------------------------------------------------
length(unique(df$País))


## ----------------------------------------------------------------------------------------------------------------------
# Contamos el número de películas por país
country_counts <- table(df$País)

# Identificamos los países que aparecen menos de 10 veces
infrequent_countries <- which(country_counts < 10)

# Extraemos los nombres de estos países
infrequent_countries_list <- names(infrequent_countries)

# Reemplazamos los nombres de estos países en el dataframe por "Otro"
df$País[df$País %in% infrequent_countries_list] <- "Otro"

# Convertimos la columna País en un factor
df$País <- factor(df$País)

# Volvemos a comprobar el número de países únicos
length(unique(df$País))


## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------------
library(caret)

train_control <- trainControl(
  method = "cv",             # para la validación cruzada
#  verboseIter = TRUE,       # para un log detallado (comentar antes de knit to PDF)
)

# Establecemos un seed para asegurar reproducibilidad
set.seed(123)
rf_model <- train(
  Puntuación.Media ~ .,      # la formula para el modelo (puntuación media como variable dependiente)
  data = df,
  method = "rf",             # el tipo de modelo (Random Forest)
  trControl = train_control, # el objeto control que hemos creado en el paso anterior
  metric = "Rsquared",       # la métrica que queremos optimizar en el modelo
)

# Mostramos el resumen del modelo por pantalla
rf_model


## ----------------------------------------------------------------------------------------------------------------------
# Escalamos las importancias de las variables, estableciendo el máximo en 100
importance <- varImp(rf_model, scale = TRUE)

# Convertimos la importancia de las variables en un dataframe para poder manipularlas mejor
importance_df <- as.data.frame(importance$importance)

# Ordenamos el dataframe por la importancia de las variables en orden descendente
importance_sorted <- importance_df[order(-importance_df$Overall),]

# Extraemos los nombres de las 10 variables más importantes
top_features_names <- rownames(head(importance_sorted, 10))

# Creamos y mostramos un gráfico de las 10 variables más importantes en el modelo
plot(importance, top = 10)


## ----------------------------------------------------------------------------------------------------------------------
# Creamos variables dummy para los países Japón y Irlanda
df$Japón <- as.numeric(df$País == "Japón")
df$Irlanda <- as.numeric(df$País == "Irlanda")

summary(lm(Puntuación.Media ~ Duración + Animación + Japón + Irlanda +
             `Matthew McConaughey` + `Damien Chazelle`, data=df))


## ----------------------------------------------------------------------------------------------------------------------
coefficients(lm(Puntuación.Media ~ `Ciencia ficción`, data=df))


## ----------------------------------------------------------------------------------------------------------------------
coefficients(lm(Puntuación.Media ~ Drama, data=df))


## ----------------------------------------------------------------------------------------------------------------------
coefficients(lm(Puntuación.Media ~ Fantástico, data=df))


## ----------------------------------------------------------------------------------------------------------------------
coefficients(lm(Puntuación.Media ~ Terror, data=df))


## ----------------------------------------------------------------------------------------------------------------------
library(knitr)
purl("Memoria.rmd")


## ----------------------------------------------------------------------------------------------------------------------
write.csv(df, '../data/data_analyzed.csv', row.names = FALSE)

