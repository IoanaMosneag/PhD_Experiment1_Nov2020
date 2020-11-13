# 1-way ANCOVA
# Version 3: 

# Grouping variable:Treatment 
# Covariate: CBF 72h
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
  Experiment_data, x = "CBF72h", y = "LV",
  color = "Treatment", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Treatment)
  )
# There was a linear relationship between CBF72h and LV for all Treatment groups, as assessed by visual inspection of a scatter plot.

# Homogeneity of regression slopes
Experiment_data %>% anova_test(LV ~ Treatment*CBF72h)
# There was homogeneity of regression slopes as the interaction term was not statistically significant, F(1, 34) = 2.384, p = 0.132.

# Normality of residuals
# Fit the model, the covariate goes first
model5 <- lm(LV ~ CBF72h + Treatment, data = Experiment_data)

# Inspect the model diagnostic metrics
model5.metrics <- augment(model5) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model5.metrics, 3)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model5.metrics$.resid)
# The Shapiro Wilk test was not significant (p > 0.05), so we can assume normality of residuals

# Homogeneity of variances
model5.metrics %>% levene_test(.resid ~ Treatment) # does not work

# Outliers
model5.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
# 1 outlier found

# Computation
res5.aov <- Experiment_data %>% anova_test(LV ~ CBF72h + Treatment)
get_anova_table(res5.aov)
# After adjustment for CBF4h, there was no statistically siginificant difference in LV between Treatment groups, F(1,35)= 0.40, p = 0.531. 

# Post-hoc test
# Pairwise comparisons
pwc5 <- Experiment_data %>% 
  emmeans_test(
    LV ~ Treatment, covariate = CBF72h,
    p.adjust.method = "bonferroni"
  )
pwc5

# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc5)

# Data are adjusted mean +/- standard error. There is no statistically significant difference between groups. T1 (emmean +/- se), T2 () p = .

# Report
# Visualization: line plots with p-values
pwc5 <- pwc5 %>% add_xy_position(x = "Treatment", fun = "mean_se")
ggline(get_emmeans(pwc5), x = "Treatment", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc5, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res5.aov, detailed = TRUE),
    caption = get_pwc_label(pwc5)
  )
# Text for Report
# An ANCOVA was run to determine the treatment effect on lesion volume after controlling for cerebral reperfusion at 72h post-surgery (CBF72h).
# After adjustment for CBF72h, there was no statistically significant difference in lesion volume between the groups, F(1,35)= 0.40, p = 0.531.
# Post hoc analysis was performed with a Bonferroni adjustment. The mean lesion volume was not statistically significant between treatment groups T1 (19.6 +/- 2.78), T2 (20.9 +/- 3.02), T3 (20.3 +/- 2.85), T4 (17.4 +/- 2.7), p > 0.05.