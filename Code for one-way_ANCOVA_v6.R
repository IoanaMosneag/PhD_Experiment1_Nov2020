# 1-way ANCOVA
# Version 6: 

# Grouping variable:Treatment 
# Covariate: Lesion Volume (LV)
# Outcome variable: CBF 72h

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
  Experiment_data, x = "LV", y = "CBF72h",
  color = "Treatment", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Treatment)
  )
# There was a linear relationship between CBF72h and LV for all Treatment groups, as assessed by visual inspection of a scatter plot.

# Homogeneity of regression slopes
Experiment_data %>% anova_test(CBF72h ~ Treatment*LV)
# There was homogeneity of regression slopes as the interaction term was not statistically significant, F(1, 34) = 0.811, p = 0.374.

# Normality of residuals
# Fit the model, the covariate goes first
model6 <- lm(CBF72h ~ LV + Treatment, data = Experiment_data)

# Inspect the model diagnostic metrics
model6.metrics <- augment(model6) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model6.metrics, 3)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model6.metrics$.resid)
# The Shapiro Wilk test was not significant (p > 0.05), so we can assume normality of residuals

# Homogeneity of variances
model6.metrics %>% levene_test(.resid ~ Treatment) # does not work

# Outliers
model6.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
# no outliers found

# Computation
res6.aov <- Experiment_data %>% anova_test(CBF72h ~ LV + Treatment)
get_anova_table(res6.aov)
# After adjustment for CBF4h, there was no statistically siginificant difference in CB72h between Treatment groups, F(1,35)= 0.091, p = 0.765. 

# Post-hoc test
# Pairwise comparisons
pwc6 <- Experiment_data %>% 
  emmeans_test(
    CBF72h ~ Treatment, covariate = LV,
    p.adjust.method = "bonferroni"
  )
pwc6

# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc6)

# Data are adjusted mean +/- standard error. There is no statistically significant difference between groups. T1 (emmean +/- se), T2 () p = .

# Report
# Visualization: line plots with p-values
pwc6 <- pwc6 %>% add_xy_position(x = "Treatment", fun = "mean_se")
ggline(get_emmeans(pwc6), x = "Treatment", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc6, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res6.aov, detailed = TRUE),
    caption = get_pwc_label(pwc6)
  )
# Text for Report
# An ANCOVA was run to determine the treatment effect on reperfusion at 72h after controlling for lesvion volume.
# After adjustment for LV, there was no statistically significant difference in CBF between the groups, F(1,35)= 0.091, p = 0.765.
# Post hoc analysis was performed with a Bonferroni adjustment. The mean lesion volume was not statistically significant between treatment groups T1 (-26.5 +/- 1.96), T2 (-21.1 +/- 2.07), T3 (-24.5 +/- 2.04), T4 (-26.2 +/- 1.93), p > 0.05.