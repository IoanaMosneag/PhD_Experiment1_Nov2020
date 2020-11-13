# 1-way ANCOVA
# Version 4: 

# Grouping variable:Treatment 
# Covariate: CBF 4h
# Outcome variable: CBF72h

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
  Experiment_data, x = "CBF4h", y = "CBF72h",
  color = "Treatment", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Treatment)
  )
# There was a linear relationship between CBF4h and CBF72h for all Treatment groups, as assessed by visual inspection of a scatter plot.

# Homogeneity of regression slopes
Experiment_data %>% anova_test(CBF72h ~ Treatment*CBF4h)
# There was homogeneity of regression slopes as the interaction term was not statistically significant, F(1, 37) = 1.144, p = 0.29.

# Normality of residuals
# Fit the model, the covariate goes first
model4 <- lm(CBF72h ~ CBF4h + Treatment, data = Experiment_data)

# Inspect the model diagnostic metrics
model4.metrics <- augment(model3) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model4.metrics, 3)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model4.metrics$.resid)
# The Shapiro Wilk test was not significant (p > 0.05), so we can assume normality of residuals

# Homogeneity of variances
model4.metrics %>% levene_test(.resid ~ Treatment) # does not work

# Outliers
model4.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
# 1 outlier found

# Computation
res4.aov <- Experiment_data %>% anova_test(CBF72h ~ CBF4h + Treatment)
get_anova_table(res4.aov)
# After adjustment for CBF4h, there was no statistically siginificant difference in CBF72h between Treatment groups, F(1,38)= 0.067, p = 0.797. 

# Post-hoc test
# Pairwise comparisons
pwc4 <- Experiment_data %>% 
  emmeans_test(
    CBF72h ~ Treatment, covariate = CBF4h,
    p.adjust.method = "bonferroni"
  )
pwc4

# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc4)

# Data are adjusted mean +/- standard error. There is no statistically significant difference between groups. T1 (emmean +/- se), T2 () p = .

# Report
# Visualization: line plots with p-values
pwc4 <- pwc4 %>% add_xy_position(x = "Treatment", fun = "mean_se")
ggline(get_emmeans(pwc4), x = "Treatment", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc4, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res4.aov, detailed = TRUE),
    caption = get_pwc_label(pwc4)
  )
# Text for Report
# An ANCOVA was run to determine the treatment effect on reperfusion at 72h after controlling for cerebral reperfusion at 4h post-surgery (CBF4h).
# After adjustment for CBF4h, there was no statistically significant difference in CBF72h between the groups, F(1,38)= 0.067, p = 0.797.
# Post hoc analysis was performed with a Bonferroni adjustment. The mean lesion volume was not statistically significant between treatment groups T1 (-27.9 +/- 2.4), T2 (-19.6 +/- 2.54), T3 (-22.4 +/- 2.31), T4 (-26.0 +/- 2.31), p > 0.05.