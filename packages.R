install.packages("pak")
pak::pak(c("tidyverse", "tidymodels", "skimr", "ggcorrplot", "janitor", 
           "glmnet", "ranger", "gridExtra", "broom", "ggridges", 
           "rpart.plot", "finetune", "lme4"))

library(tidyverse)
library(tidymodels)
library(skimr)
library(ggcorrplot)
library(glmnet)
library(ranger)
library(gridExtra)
library(broom)
library(ggridges)
library(rpart.plot)
library(finetune)
