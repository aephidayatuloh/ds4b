library(tidyverse)
library(scales)
# library(readxl)
use("readxl", "read_xlsx")
# library(skimr)
use("skimr", "skim_without_charts")
# library(janitor)
use("janitor", "clean_names")
# library(gridExtra)
use("gridExtra", "grid.arrange")

demographics <- read_xlsx("data/Telco_customer_churn_demographics.xlsx")
demographics

demographics <- clean_names(demographics)
demographics

skim_without_charts(demographics)

demographics <- demographics %>% 
  select(-count, -under_30, -senior_citizen, -dependents)
demographics

services <- read_xlsx("data/Telco_customer_churn_services.xlsx")
services

services <- clean_names(services)
skim_without_charts(services)

services <- services %>% 
  select(-count, -quarter)

status <- read_xlsx("data/Telco_customer_churn_status.xlsx")
status

status <- clean_names(status)
skim_without_charts(status)

status <- status %>% 
  select(customer_id, churn_label, cltv)

telco_customer <- demographics %>% 
  left_join(services, by = join_by(customer_id)) %>% 
  left_join(status, by = join_by(customer_id))
telco_customer

skim_without_charts(telco_customer)

# EDA Target variable ----

telco_customer %>% 
  count(churn_label) %>% 
  mutate(pct = n/sum(n))

telco_customer %>% 
  count(churn_label) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = churn_label, y = pct)) + 
  geom_col(alpha = 0.75) + 
  geom_text(aes(label = paste0(comma(n), " (", percent(pct, accuracy = 0.01), "%)")), 
            size = 4, vjust = -0.25) + 
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) + 
  labs(title = "Proportion of Target Class (Churn)", 
       x = "Churn", 
       y = "Proportion") + 
  theme_minimal()

# EDA Predictors ----

p1 <- telco_customer %>% 
  ggplot(aes(x = age, fill = churn_label)) + 
  geom_density(alpha = 0.75) + 
  scale_y_continuous(labels = scientific_format()) + 
  theme_minimal()
p1

p2 <- telco_customer %>% 
  ggplot(aes(x = age, fill = churn_label)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scientific_format()) + 
  ylab(" ") + 
  theme_minimal()
p2

grid.arrange(p1, p2, heights = c(2, 1))

telco_customer %>% 
  ggplot(aes(x = gender, fill = churn_label)) + 
  geom_bar(alpha = 0.75, position = "fill") + 
  scale_y_continuous(labels = percent_format()) + 
  theme_minimal()

file.show("R/ideal-dist.R")

# Correlation ----
library(ggcorrplot)

cor_matrix <- telco_customer %>% 
  select_if(is.numeric) %>% 
  cor()

pval_matrix <- telco_customer %>% 
  select_if(is.numeric) %>% 
  cor_pmat()

ggcorrplot(cor_matrix, lab = TRUE, p.mat = pval_matrix)

# Export dataframe for next step ------------------------------------------

telco_customer %>% 
  write_csv("data/telco_customer_churn.csv")
