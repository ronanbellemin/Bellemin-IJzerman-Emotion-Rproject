# loading packages
library(lmerTest)
library(lme4)
library(tidyverse)

# seed for reproducibility
set.seed(290438)

# reading exploratory dataset
exploratory_df <- readxl::read_xlsx("exploratory_dataset.xlsx")


## exploratory data analysis (plot) ## -----------------------------------------

exploratory_df %>%
    ggplot(aes(x = standard_time, y = temperature, group = relational_model)) + 
    geom_smooth(aes(color=relational_model), method = lm) +
    facet_grid(~emotion) +
    #scale_color_manual(values=c('brown4','blue4', "green4", "orange4", "black4")) +
    theme_bw()


## modeling our predictions for SADNESS condition ## ---------------------------

# making the contrasts according to our predictions
exploratory_df_sadness <- exploratory_df %>% mutate(
    # C1 is testing ARd & MP VS ARs & EM
    C1 = case_when(
        emotion == "sadness" & relational_model == "ARd" ~ -0.5,
        emotion == "sadness" & relational_model=="MP" ~ -0.5,
        emotion == "sadness" & relational_model == "ARs" ~ 0.5,
        emotion == "sadness" & relational_model=="EM" ~ 0.5,
        emotion == "sadness" & relational_model == "CS" ~ 0),
    # C2 is testing EM & ARs VS CS
    C2_sadness = case_when(
        emotion == "sadness" & relational_model == "ARs" ~ -0.5,
        emotion == "sadness" & relational_model=="EM" ~ -0.5,
        emotion == "sadness" & relational_model=="CS" ~ 1,
        emotion == "sadness" & relational_model == "MP" ~ 0,
        emotion == "sadness" & relational_model == "ARd" ~ 0),
    # C3 is testing ARd & MP VS CS
    C3_sadness = case_when(
        emotion == "sadness" & relational_model == "ARd" ~ -0.5,
        emotion == "sadness" & relational_model=="MP" ~ -0.5,
        emotion == "sadness" & relational_model=="CS" ~ 1,
        emotion == "sadness" & relational_model == "EM" ~ 0,
        emotion == "sadness" & relational_model == "ARs" ~ 0),
    # C4 is testing ARd VS MP
    C4_sadness = case_when(
        emotion == "sadness" & relational_model == "ARd" ~ -0.5,
        emotion == "sadness" & relational_model == "MP" ~ 0.5,
        emotion == "sadness" & relational_model == "ARs" ~ 0,
        emotion == "sadness" & relational_model == "EM" ~ 0,
        emotion == "sadness" & relational_model == "CS" ~ 0),
    # C5 is testing ARs VS EM
    C5_sadness = case_when(
        emotion == "sadness" & relational_model == "ARs" ~ -0.5,
        emotion == "sadness" & relational_model == "EM" ~ 0.5,
        emotion == "sadness" & relational_model == "ARd" ~ 0,
        emotion == "sadness" & relational_model == "MP" ~ 0,
        emotion == "sadness" & relational_model == "CS" ~ 0)
    )

# creating the model
model1 <- lmer(temperature ~ C1_sadness + C2_sadness + C3_sadness + C4_sadness + C5_sadness + 
                    (C1_sadness + C2_sadness + C3_sadness + C4_sadness + C5_sadness | subj_id), 
                exploratory_df_sadness)

summary(model1)

# we check here the assumptions to run the model
# 1) Residuals normality
hist(residuals(model1)) # residuals histogram
qqnorm(residuals(model1))
qqline(residuals(model1)) # QQ plot
shapiro.test(residuals(model1))

# 2) Homogeneity of variance
plot(fitted(model1), residuals(model1))
plot(fitted(model1), abs(residuals(model1)^.5))
abline(lm((abs(residuals(model1)))^.5 ~ fitted(model1))) 

# 3) outliers check
outliers(VD ~ VI, DF)

# retrieving p-values of each effect
pvalues_model1 <- model1$p.value

# correcting p-values
pvalues_model1 <- p.adjust.methods(pvalues_model1, "bonferroni")
print(pvalues_model1)


## modeling our predictions for ANGER condition ## -----------------------------

# making the contrasts according to our predictions
exploratory_df_anger <- exploratory_df %>% mutate(
    # C1 is testing CS & MP VS ARd & ARs
    C1_anger = case_when(
        emotion == "anger" & relational_model == "ARs" ~ -0.5,
        emotion == "anger" & relational_model=="ARd" ~ -0.5,
        emotion == "anger" & relational_model == "CS" ~ 0.5,
        emotion == "anger" & relational_model=="MP" ~ 0.5,
        emotion == "anger" & relational_model == "EM" ~ 0),
    # C2 is testing CS & MP vs EM
    C2_anger = case_when(
        emotion == "anger" & relational_model == "CS" ~ 0.5,
        emotion == "anger" & relational_model=="MP" ~ 0.5,
        emotion == "anger" & relational_model=="EM" ~ -1,
        emotion == "anger" & relational_model == "ARs" ~ 0,
        emotion == "anger" & relational_model == "ARd" ~ 0),
    # C3 is testing ARd & ARs VS EM
    C3_anger = case_when(
        emotion == "anger" & relational_model == "ARd" ~ -0.5,
        emotion == "anger" & relational_model=="ARs" ~ -0.5,
        emotion == "anger" & relational_model=="EM" ~ 1,
        emotion == "anger" & relational_model == "CS" ~ 0,
        emotion == "anger" & relational_model == "MP" ~ 0),
    # C4 is testing CS VS MP
    C4_anger = case_when(
        emotion == "anger" & relational_model == "MP" ~ -0.5,
        emotion == "anger" & relational_model == "CS" ~ 0.5,
        emotion == "anger" & relational_model == "ARs" ~ 0,
        emotion == "anger" & relational_model == "EM" ~ 0,
        emotion == "anger" & relational_model == "ARd" ~ 0),
    # C5 is testing ARs VS ARd
    C5_anger = case_when(
        emotion == "anger" & relational_model == "ARs" ~ -0.5,
        emotion == "anger" & relational_model == "ARd" ~ 0.5,
        emotion == "anger" & relational_model == "EM" ~ 0,
        emotion == "anger" & relational_model == "MP" ~ 0,
        emotion == "anger" & relational_model == "CS" ~ 0)
)

# creating the model
model2 <- lmer(temperature ~ C1_anger + C2_anger + C3_anger + C4_anger + C5_anger + 
                   (C1_anger + C2_anger + C3_anger + C4_anger + C5_anger | subj_id), 
               exploratory_df_anger)

summary(model2)

# we check here the assumptions to run the model
# 1) Residuals normality
hist(residuals(model2)) # residuals histogram
qqnorm(residuals(model2))
qqline(residuals(model2)) # QQ plot
shapiro.test(residuals(model2))

# 2) Homogeneity of variance
plot(fitted(model2), residuals(model2))
plot(fitted(model2), abs(residuals(model2)^.5))
abline(lm((abs(residuals(model2)))^.5 ~ fitted(model2))) 

# 3) outliers check
outliers(VD ~ VI, DF)

# retrieving p-values of each effect
pvalues_model2 <- model2$p.value

# correcting p-values
pvalues_model2 <- p.adjust.methods(pvalues_model2, "bonferroni")
print(pvalues_model2)


## modeling our predictions for HAPPINESS condition ## -------------------------

# making the contrasts according to our predictions
exploratory_df_happiness <- exploratory_df %>% mutate(
    # C1 is testing CS & EM VS ARd & ARs & MP
    C1_happiness = case_when(
        emotion == "happiness" & relational_model == "ARs" ~ -2,
        emotion == "happiness" & relational_model=="ARd" ~ -2,
        emotion == "happiness" & relational_model == "MP" ~ -2,
        emotion == "happiness" & relational_model=="CS" ~ 3,
        emotion == "happiness" & relational_model == "EM" ~ 3),
    # C2 is testing EM VS CS
    C2_happiness = case_when(
        emotion == "happiness" & relational_model == "CS" ~ 0.5,
        emotion == "happiness" & relational_model=="EM" ~ -0.5,
        emotion == "happiness" & relational_model=="MP" ~ 0,
        emotion == "happiness" & relational_model == "ARs" ~ 0,
        emotion == "happiness" & relational_model == "ARd" ~ 0),
    # C3 is testing ARd VS ARs
    C3_happiness = case_when(
        emotion == "happiness" & relational_model == "ARd" ~ -0.5,
        emotion == "happiness" & relational_model=="ARs" ~ 0.5,
        emotion == "happiness" & relational_model=="EM" ~ 0,
        emotion == "happiness" & relational_model == "CS" ~ 0,
        emotion == "happiness" & relational_model == "MP" ~ 0),
    # C4 is testing ARs VS MP
    C4_happiness = case_when(
        emotion == "happiness" & relational_model == "ARs" ~ -0.5,
        emotion == "happiness" & relational_model == "MP" ~ 0.5,
        emotion == "happiness" & relational_model == "CS" ~ 0,
        emotion == "happiness" & relational_model == "EM" ~ 0,
        emotion == "happiness" & relational_model == "ARd" ~ 0),
    # C5 is testing ARd VS MP
    C5_happiness = case_when(
        emotion == "happiness" & relational_model == "ARd" ~ -0.5,
        emotion == "happiness" & relational_model == "MP" ~ 0.5,
        emotion == "happiness" & relational_model == "EM" ~ 0,
        emotion == "happiness" & relational_model == "ARd" ~ 0,
        emotion == "happiness" & relational_model == "CS" ~ 0)
)

# creating the model
model3 <- lmer(temperature ~ C1_happiness + C2_happiness + C3_happiness + C4_happiness + C5_happiness + 
                   (C1_happiness + C2_happiness + C3_happiness + C4_happiness + C5_happiness | subj_id), 
               exploratory_df_anger)

summary(model3)

# we check here the assumptions to run the model
# 1) Residuals normality
hist(residuals(model3)) # residuals histogram
qqnorm(residuals(model3))
qqline(residuals(model3)) # QQ plot
shapiro.test(residuals(model3))
    
# 2) Homogeneity of variance
plot(fitted(model3), residuals(model3))
plot(fitted(model3), abs(residuals(model3)^.5))
abline(lm((abs(residuals(model3)))^.5 ~ fitted(model3))) 

# 3) outliers check
outliers(VD ~ VI, DF)

# retrieving p-values of each effect
pvalues_model3 <- model3$p.value

# correcting p-values
pvalues_model3 <- p.adjust.methods(pvalues_model3, "bonferroni")
print(pvalues_model3)