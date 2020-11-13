library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(datarium)

getwd()
setwd('C:/Users/mosne/Documents/Experiment 1')
getwd()

#Version 1: 
#Grouping variables: Treatment and CBF4h
#Covariate: Lesion Volume (LV)
#Outcome variable: CBF 72h

# Linearity assumption (scatter plot between covariate and outcome variables)
# smoothes loess lines should help decide if the relationship is linear or not

ggscatter(
  Experiment_data, x = "LV", y = "CBF72h",
  facet.by  = c("Treatment"),
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

# Homogeneity of regression slopes
#checks whether there is no significant interaction between covariate
#and grouping variables

Experiment_data %>%
  anova_test(
    CBF72h ~ LV + Treatment + CBF4h + 
      Treatment*CBF4h + LV*Treatment +
      LV*CBF4h + LV*CBF4h*Treatment)

# There was homogeneity of regression slopes as the interaction terms, between the covariate (LV) and grouping variables (Treatment and CBF4h) was not statistically significant, p > 0.05.

# Normality of Residuals
#Fit the model, the covariate goes first

model <- lm(CBF72h ~ LV + Treatment*CBF4h, data = Experiment_data)

# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model.metrics, 3)

# Assess normality of residuals using shapiro wilk test

shapiro_test(model.metrics$.resid)

# The Shapiro Wilk test was not significant (p > 0.05), so we can assume normality of residuals

# Homogeneity of variances
levene_test(.resid ~ Treatment*CBF4h, data = model.metrics)
#The Levene's test was not significant (p > 0.05), so we can assume homogeneity of the residual variances for all groups.

# Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 2) %>%
  as.data.frame()

# Computation
res.aov <- Experiment_data %>% 
  anova_test(CBF72h ~ LV + Treatment*CBF4h)
get_anova_table(res.aov)

#Model answer: After adjustment for age, there was a statistically significant interaction between treatment and exercise on the stress score, F(2, 53) = 4.45, p = 0.016. This indicates that the effect of exercise on score depends on the level of exercise, and vice-versa.

#Post-Hoc tests
# Effect of CBF4h in each Treatment, data grouped by Treatment, one-way ANCOVA for CBF4h controlling for LV
Experiment_data %>%
  group_by(CBF4h) %>%
  anova_test(CBF72h ~ LV + Treatment)
get_anova_table(res.aov)

# Note that, we need to apply Bonferroni adjustment for multiple testing corrections. One common approach is lowering the level at which you declare significance by dividing the alpha value (0.05) by the number of tests performed. In our example, that is 0.05/3 = 0.016667
# Statistical significance was accepted at the Bonferroni-adjusted alpha level of (change -0.01667, that is 0.05/3). The effect of reperfusion at CBF4h was statistically significant in the Treatment 1 group (p = 0.004) and in Treatment group 3 (p = 0.007), but not in the the other two groups.

library(emmeans)

#Compute pairwise comparisons between treatment groups at each level of exercise. The Bonferroni multiple testing correction is applied.

# Pairwise comparisons
pwc <- Experiment_data %>% 
  group_by(CBF4h) %>%
  emmeans_test(
    CBF72h ~ Treatment, covariate = LV,
    p.adjust.method = "bonferroni"
  )
get_anova_table(pwc)

#Perform multiple pairwise comparisons between CBF4h at each level of treatment. 
#Doesn't work
pwc2 <- Experiment_data %>% 
  group_by(Treatment) %>%
  emmeans_test(
    CBF72h ~ CBF4h, covariate = LV,
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p)
get_anova_table(pwc2)# Remove details

# Report
# Line plot

lp <- ggline(
  get_emmeans(pwc), x = "CBF4h", y = "emmean", 
  color = "Treatment", palette = "jco"
) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = Treatment), 
    width = 0.1
  )

# Comparisons between treatment group at each exercise level
pwc <- pwc %>% add_xy_position(x = "CBF4h", fun = "mean_se", step.increase = 0.2)

pwc2.filtered <- pwc2 %>% filter(Treatment == "4")
lp + 
  stat_pvalue_manual(
    pwc2.filtered, hide.ns = TRUE, tip.length = 0,
    step.group.by = "Treatment", color = "Treatment"
  ) +
  labs(
    subtitle = get_test_label(res.aov,  detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )


