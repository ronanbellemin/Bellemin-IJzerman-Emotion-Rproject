### Exploratory analysis ### -------------------------------------------------

# loading packages
library(lmerTest)
library(lme4)
library(tidyverse)

# seed for reproducibility
set.seed(290438)


## simulating data while waiting for real data ## 

# loading previous data to get both by-subject grand mean and by-subject standard deviation
df <- readxl::read_xlsx("temp_data_purplexus_validation.xlsx")

df <- df %>% 
    arrange(participant.id, purplexus_timestamp) %>% 
    group_by(participant.id) %>% 
    filter(row_number() == 1)

vec <- df$purplexus_temp

n_subj     = 20   # number of subjects
n_emo  =  3 # number of emo stimuli
n_relation = 5 # number of relational stimuli
beta_0     =  mean(vec)   # grand mean
beta_1     =  .1   # simple effect of relational models
beta_2     =  .3   # 
beta_3     = .3    # interaction effect
omega_0    =  2   # by-item random intercept sd
tau_0      =  sd(vec)   # by-subject random intercept sd
tau_1      =  2   # by-subject random slope sd
#tau_2      = .5   # by-subject random interaction sd
rho        = .01   # correlation between intercept and slope
sigma      = 4 # residual (standard deviation)
vec <- seq(1:120)

# data frame
items <- data.frame(
        item_id = seq_len(n_emo*n_relation),
        emotion = rep(c("happy","anger","sadness"), n_relation),
        relational_model = rep(c("CS", "ARs","ARd","EM","MP"), n_emo),
        standard_time = rep(vec, time = 120),
        X_i = rep(c(1, 1, 0), n_relation),
        Y_i = rep(c(0, -0.5, 0.5, 0, 0), n_emo),
        O_0i = rnorm(n = 1800, mean = 0, sd = omega_0))
    
# variance-covariance matrix
cov_mx  <- matrix(
        c(tau_0^2,             rho * tau_0 * tau_1, #* tau_2,
          rho * tau_0 * tau_1, #* tau_2, 
          tau_1^2            ),
        nrow = 2, byrow = TRUE)
    
subjects <- data.frame(subj_id = seq_len(n_subj),
                           MASS::mvrnorm(n = n_subj,
                                         mu = c(T_0s = 0, T_1s = 0),
                                         Sigma = cov_mx))
    
dat_sim <- crossing(subjects, items)  %>%
        mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
               temperature = beta_0 + T_0s + O_0i + (beta_1 + T_1s) * X_i + (beta_2 + T_1s) * Y_i +
                   (beta_3 + T_1s)*(X_i*Y_i) + e_si) %>%
        select(subj_id, item_id, emotion, X_i, relational_model, Y_i, temperature, standard_time)


## exploratory analysis ##

dat_sim %>% filter(emotion == "sadness") %>% 
    filter(relational_model == "ARs" | relational_model == "ARd") %>% 
    ggplot(aes(x = standard_time, y = temperature, group = relational_model)) + 
    geom_smooth(aes(color=relational_model), method = lm) +
    scale_color_manual(values=c('brown4','blue4')) +
    theme_bw()

## model ##

mod_sim <- lmer(temperature ~ X_i*Y_i + (1 | subj_id), dat_sim)

summary(mod_sim)

