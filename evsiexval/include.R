#Last update: January 23, 2024

library(tidyverse)
library(voipred)
library(pROC)
library(sqldf)


set.seed(123)

my_format <- function(val, n_digits=4)
{
  format(round(val,n_digits),nsmall=n_digits)
}



data(gusto)
gusto$kill <- (as.numeric(gusto$Killip)>1)*1
gusto$Y <- gusto$day30
data_us <- gusto[gusto$regl %in% c(1, 7, 9, 10, 11, 12, 14, 15),]
data_other <- gusto[!gusto$regl %in% c(1, 7, 9, 10, 11, 12, 14, 15),]


dev_data <- data_other
val_data <- data_us

#model <- glm(Y ~ age + miloc + pmi + kill + pmin(sysbp,100) + lsp(pulse,50), data=dev_data, family=binomial(link="logit"))
model <- glm(Y ~ age + miloc + pmi + kill + pmin(sysbp,100) + pulse, data=dev_data, family=binomial(link="logit"))



pi <- predict(model, type="response", newdata=val_data)
val_data$pi <- pi


