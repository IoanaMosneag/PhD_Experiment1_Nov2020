library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(datarium)

getwd()
setwd('C:/Users/mosne/Documents/Experiment 1')
getwd()

#Version 2: 
#Grouping variables: Treatment 
#Covariate: CBF 4h
#Outcome variable: Lesion Volume (LV), CBF 72h

# Linearity assumption

ggscatter(
  Experiment_data, x = "CBF4h", y = "LV",
  facet.by  = c("treatment"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

# Homogeneity of regression slopes

Experiment_data %>%
  anova_test(
    LV ~ CBF4h + Treatment + CBF72h + 
      Treatment*CBF72h + CBF4h*Treatment +
      CBF4h*CBF72h + CBF4h*CBF72h*Treatment
  )

# Fit the model, the covariate goes first
model2 <- lm(LV ~ CBF4h + Treatment*CBF72h, data = Experiment_data)
# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model.metrics, 3)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)

#Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

# Computation
res.aov <- Experiment_data %>% 
  anova_test(LV ~ CBF4h + CBF72h ~ CBF4h + Treatment)
get_anova_table(res.aov)

# Simple main effect analyses for treatment
# Effect of treatment 
Experiment_data%>%
  group_by(CBF72h) %>%
  anova_test(LV ~ CBF4h + Treatment)

pwc <- Experiment_data %>% 
  group_by(CBF72h) %>%
  emmeans_test(
    LV ~ Treatment, covariate = CBF4h,
    p.adjust.method = "bonferroni"
  )

Experiment_data%>%
  group_by(Treatment) %>%
  anova_test(LV ~ CBF4h + CBF72h)
