
# use layer functions to specify a neural network -------------------------

library(MicrosoftML)
library(magrittr)
library(ggplot2)

frm <- formula(price ~ ., env = diamonds)
frm <- formula(price ~ carat + cut + color + clarity + depth + table + x + y + z, env = diamonds)

frm <- formula(price ~ carat + cut + color + clarity + depth, env = diamonds)

frm

idx <- sample(nrow(diamonds) * 0.8)
data_train <- diamonds[idx, ]
data_test  <- diamonds[-idx, ]


numIterations <- 100
optim_spec <- sgd(learningRate = 0.02, lRateRedRatio = 0.95, lRateRedFreq = 5)
optim_spec <- adaDeltaSgd()

model1 <- neuralNet(frm,
                      data = data_train,
                      type = "regression",
                      optimizer = optim_spec,
                      numHiddenNodes = 128, 
                      numIterations = numIterations
)

(model1)


#  ------------------------------------------------------------------------


input_size <- nnlayer_compute_input_size(frm, diamonds) + 2

nn <- nnlayer_input(input_size, name = "diamonds") %>% 
  nnlayer_full(64, name = "hid1", activation = "linear") %>%
  nnlayer_full(64, name = "hid2", activation = "linear") %>%
  nnlayer_output(1, name = "price", activation = "linear")
nn


model2 <- neuralNet(frm,
                      data = data_train,
                      type = "regression",
                      optimizer = optim_spec,
                      netDefinition = nn,
                      numIterations = numIterations
)



#  ------------------------------------------------------------------------


models <- list(model1, model2)

pred <- lapply(seq_along(models), 
               function(i){
                 predict(models[[i]], data_test, 
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
ggplot(zz, aes(x = price, y = Prediction, group = Model, colour = Model)) + 
  geom_point(alpha = 0.05) +
  geom_smooth(colour = "black", se = TRUE) +
  facet_grid(~Model)
