library(ggplot2)
use("gridExtra", "grid.arrange")
library(scales)

# Sebaran Numeric Ideal ----
set.seed(123)

n <- 100000
contoh <- data.frame(
  x = c(rnorm(n, mean = 0, sd = 0.4),  # Sebaran normal pertama, mean=0
        rnorm(n, mean = 3, sd = 0.4)), # Sebaran normal kedua, mean=3
  group = rep(c("Mean = 0", "Mean = 3"), each = n)
)

p01 <- ggplot(contoh, aes(x = x, fill = group)) +
  geom_density(alpha = 0.5) +  # plot density dengan transparansi
  labs(title = "Perbandingan Dua Sebaran Normal",
       x = "X",
       y = "Kepadatan (Density)",
       fill = "Kelompok") +
  theme_minimal() + 
  theme(legend.position = "none")


p01

p02 <- contoh %>% 
  ggplot(aes(x = x, y = "", fill = group)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_continuous(limits = c(-1.852836, 4.682436)) +
  labs(X = "X", y = " ") +
  theme_minimal() + 
  theme(legend.position = "none")
p02

grid.arrange(p01, p02, heights = c(2, 1))


data.frame(
  Gender = c("Female", "Female", "Male", "Male"), 
  Churn = c("Yes", "No", "Yes", "No"), 
  Proportion = c(0.92, 0.08, 0.15, 0.85)
) %>% 
  ggplot(aes(x = Gender, y = Proportion, fill = Churn)) + 
  geom_col(alpha = 0.75, position = "fill") + 
  scale_y_continuous(labels = percent_format()) + 
  theme_minimal()

data.frame(
  Education = factor(c("SD", "SD", "SMP", "SMP", "SMA", "SMA", "S1+", "S1+"), levels = c("SD", "SMP", "SMA", "S1+")), 
  Churn = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No"), 
  Proportion = c(0.92, 0.08, 0.85, 0.15, 0.65, 0.35, 0.15, 0.85)
) %>% 
  ggplot(aes(x = Education, y = Proportion, fill = Churn)) + 
  geom_col(alpha = 0.75, position = "fill") + 
  scale_y_continuous(labels = percent_format()) + 
  theme_minimal()
