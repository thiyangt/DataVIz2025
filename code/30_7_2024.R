# ToothGrowth

##1
library(tidyverse)

ToothGrowth |>
  group_by(supp, dose) |>
  summarise(mean_length = mean(len)) |>
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supp)) +
  geom_bar(stat = "identity")

##2
ToothGrowth |>
  group_by(supp, dose) |>
  summarise(mean_length = mean(len)) |>
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supp)) +
  geom_bar(stat = "identity",
           position = "dodge")

#3
ToothGrowth |>
  group_by(supp, dose) |>
  summarise(mean_length = mean(len)) |>
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supp)) +
  geom_bar(stat = "identity",
           position = "dodge", 
           colour = "#FFFFFF",
           size = 2)

#4
ToothGrowth |>
  mutate(supplement = 
           case_when(supp == "OJ" ~ "Orange Juice",
                     supp == "VC" ~ "Vitamin C",
                     TRUE ~ as.character(supp))) |>
  group_by(supplement, dose) |>
  summarise(mean_length = mean(len)) |>
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           position = "dodge", 
           colour = "#FFFFFF",
           size = 2)

#5
ToothGrowth |>
  mutate(supplement = 
           case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) |>
  summarise(mean_length = mean(len)) |>
  ggplot(aes(x = dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           position = "dodge", 
           colour = "#FFFFFF",
           size = 2) +
  theme_minimal()

#6
# Turn dose into categorical
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) |>
  summarise(mean_length = mean(len)) |>
  mutate(categorical_dose = factor(dose)) |> ##
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           position = "dodge",
           colour = "#FFFFFF", 
           size = 2) +
  theme_minimal()

#7
ToothGrowth |>
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) |>
  summarise(mean_length = mean(len)) |>
  mutate(categorical_dose = factor(dose)) |>
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()

#8
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose)) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  labs(x = "Dose",
       y = "Mean length (mm)") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()

#9
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose)) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(aes(alpha=dose),
           stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  labs(x = "Dose",
       y = "Mean length (mm)") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  facet_wrap(supplement ~ ., ncol = 1) +
  theme_minimal()

#9
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose)) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(aes(alpha=dose),
           stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  labs(x = "Dose",
       y = "Mean length (mm)") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  facet_wrap(supplement ~ ., ncol = 1) +
  scale_alpha(range = c(0.33, 1)) +
  theme_minimal()

#10
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose)) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(aes(alpha=dose),
           stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  labs(x = "Dose",
       y = "Mean length (mm)") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  facet_wrap(supplement ~ ., ncol = 1) +
  scale_alpha(range = c(0.33, 1)) +
  scale_x_discrete(breaks = c("0.5", "1", "2"), 
                  labels = function(x) 
                    paste0(x, " mg/day"))+
  theme_minimal()

#11
ToothGrowth %>%
  mutate(supplement = case_when(supp == "OJ" ~ "Orange Juice", supp == "VC" ~ "Vitamin C", TRUE ~ as.character(supp))) %>%
  group_by(supplement, dose) %>%
  summarise(mean_length = mean(len)) %>%
  mutate(categorical_dose = factor(dose)) %>%
  ggplot(aes(x = categorical_dose,
             y = mean_length,
             fill = supplement)) +
  geom_bar(aes(alpha=dose),
           stat = "identity",
           colour = "#FFFFFF", 
           size = 2) + 
  labs(x = "Dose",
       y = "Mean length (mm)") +
  scale_fill_manual(values = c("#fab909", 
                               "#E93603")) +
  facet_wrap(supplement ~ ., ncol = 1) +
  scale_alpha(range = c(0.33, 1)) +
  scale_x_discrete(breaks = c("0.5", "1", "2"), 
                   labels = function(x) 
                     paste0(x, " mg/day"))+
  coord_flip()+
  theme_minimal()

