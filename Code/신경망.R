install.packages("dummies")
install.packages("keras")


library(devtools)
install_github('rstudio/reticulate',force=T)
library(reticulate)
library(tensorflow)
install_tensorflow(version= "1.1.0")
install_github("rstudio/keras",force=T)
library(keras)
keras::install_keras()


devtools::install_github("rstudio/keras")


install_tensorflow(version = "1.12")

library(dummies)
library(keras)
install_keras()
install_tensorflow(version = "1.12")


data <- read.csv("movies_n2.csv", header = TRUE)
head(data)


y <- data$sales
logy <- log(y)
x.conti <- data[c("num_screen", "view")]
x.conti
x.categ <- data[c("distribution", "grade")]
x.categ

genre <- data$genre2
genrex <- dummy(genre)
genrex
genrex <- genrex[, -6]

country <- data$country
countryx <- dummy(country)
countryx <- countryx[, -3]

x.categ <- cbind(x.categ, genre = genrex, country = countryx)

x.c2 <- scale(x.conti)
avg <- attr(x.c2, "scaled:center")
std <- attr(x.c2, "scaled:scale")

x <- cbind(x.c2, x.categ)
x



model <- keras_model_sequential()

model %>%
  layer_dense(units = 64, activation = "relu", input_shape = 13) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)
summary(model)

model %>% get_config()


model %>% compile(
  loss = "mse", optimizer = optimizer_rmsprop(), metrics = c("mean_absolute_error"))

history <- model %>% fit(
  x, logy, epochs = 500, batch_size = 128, validation_split = 0.2)

plot(history)


test.predictions <- model %>% predict(test.x)
test.predictions

