# 1-way ANCOVA
# Version 3: 
 
# Grouping variable:Treatment 
# Covariate: CBF 4h
# Outcome variable: Lesion Volume (LV)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(datarium)
library(emmeans)

getwd()
setwd('C:/Users/mosne/Documents/Experiment 1')
getwd()

# Linearity assumption
ggscatter(
  Experiment_data, x = "CBF4h", y = "LV",
  color = "Treatment", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Treatment)
  )
# There was a linear relationship between CBF4h and LV for all Treatment groups, as assessed by visual inspection of a scatter plot.

# Homogeneity of regression slopes
Experiment_data %>% anova_test(LV ~ Treatment*CBF4h)
# There was homogeneity of regression slopes as the interaction term was not statistically significant, F(1, 34) = 0.-47, p = 0.83.

# Normality of residuals
# Fit the model, the covariate goes first
model3 <- lm(LV ~ CBF4h + Treatment, data = Experiment_data)

# Inspect the model diagnostic metrics
model3.metrics <- augment(model3) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model3.metrics, 3)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model3.metrics$.resid)
# The Shapiro Wilk test was not significant (p > 0.05), so we can assume normality of residuals

# Homogeneity of variances
model3.metrics %>% levene_test(.resid ~ Treatment) # does not work

# Outliers
model3.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
# 1 outlier found

# Computation
res3.aov <- Experiment_data %>% anova_test(LV ~ CBF4h + Treatment)
get_anova_table(res3.aov)
# After adjustment for CBF4h, there was no statistically siginificant difference in LV between Treatment groups, F(1,35)= 0.265, p = 0.61. 

# Post-hoc test
# Pairwise comparisons
pwc3 <- Experiment_data %>% 
  emmeans_test(
    LV ~ Treatment, covariate = CBF4h,
    p.adjust.method = "bonferroni"
  )
pwc3

# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc3)

# Data are adjusted mean +/- standard error. There is no statistically significant difference between groups. T1 (emmean +/- se), T2 () p = .

# Report
# Visualization: line plots with p-values
pwc3 <- pwc3 %>% add_xy_position(x = "Treatment", fun = "mean_se")
ggline(get_emmeans(pwc3), x = "Treatment", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc3, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res3.aov, detailed = TRUE),
    caption = get_pwc_label(pwc3)
  )
# Text for Report
# An ANCOVA was run to determine the treatment effect on lesion volume after controlling for cerebral reperfusion at 4h post-surgery (CBF4h).
# After adjustment for CBF4h, there was no statistically significant difference in lesion volume between the groups, F(1,35)= 0.265, p = 0.61.
# Post hoc analysis was performed with a Bonferroni adjustment. The mean lesion volume was not statistically significant between treatment groups T1 (22.9 +/- 3.49), T2 (16.2 +/- 3.7), T3 (19.3 +/- 3.71), T4 (19.3 +/- 3.5), p > 0.05.