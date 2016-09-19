
# use layer functions to specify a neural network -------------------------

library(MicrosoftRML)
library(magrittr)
library(ggplot2)

frm <- formula(price ~ ., env = diamonds)
frm <- formula(price ~ carat + cut + color + clarity + depth + table + x + y + z, env = diamonds)

frm <- formula(price ~ carat + cut + color + clarity, env = diamonds)

frm

idx <- sample(nrow(diamonds) * 0.8)
data_train <- diamonds[idx, ]
data_test  <- diamonds[-idx, ]


numIterations <- 25

model1 <- mxNeuralNet(frm,
                      data = data_train,
                      type = "regression",
                      numHiddenNodes = 10, 
                      numIterations = numIterations
)

summary(model1)


input_size <- layer_compute_input_size(frm, diamonds) + 2

nn <- layer_input(input_size, name = "diamonds") %>% 
  layer_full(10, name = "hid1") %>%
  # layer_full(100, name = "hid2") %>%
  layer_output(1, name = "price", activation = "linear")
nn


model2 <- mxNeuralNet(frm,
                      data = data_train,
                      # optimizer = maOptimizerSgd(weightDecay = 0.1),
                      netDefinition = nn,
                      type = "regression",
                      numIterations = numIterations
)

models <- list(model1, model2)

pred <- lapply(seq_along(models), 
               function(i){
                 mxPredict(models[[i]], data_test, 
                           extraVarsToWrite = "price", suffix = i)
               }
)


z <- do.call(data.frame, pred)
z$price.1 <- NULL
str(z)
library(tidyr)
library(dplyr)
zz <- gather(data = z, Model, Prediction, starts_with("score"))
head(zz)

library(ggplot2)
ggplot(zz, aes(x = price, y = Prediction, colour = Model)) + geom_point()
