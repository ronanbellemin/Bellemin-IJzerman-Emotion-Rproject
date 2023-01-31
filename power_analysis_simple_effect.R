# load required packages
library("lme4")        # model specification / estimation
library("lmerTest")    # provides p-values in the output
library("tidyverse")   # data wrangling and visualization
library("magrittr")      # for %>% function

# loading previous data to get both by-subject grand mean and by-subject standard deviation
# you can find it on the GitHub repo of this project: https://github.com/ronanbellemin/bellemin-ijzerman_emotion-relational-models
df <- readxl::read_xlsx("temp_data_purplexus_validation.xlsx")

df <- df %>% 
    arrange(participant.id, purplexus_timestamp) %>% 
    group_by(participant.id) %>% 
    filter(row_number() == 1)

vec <- df$purplexus_temp

# ensure this script returns the same results on each run
set.seed(8675309)

# set up the custom data simulation function
my_sim_data <- function(
        n_subj     = 20,   # number of subjects
        n_emo  =  3, # number of emo stimuli
        n_relation = 5, # number of relational stimuli
        beta_0     =  mean(vec),   # grand mean
        beta_1     =  .1,   # simple effect of relational models
        beta_2     =  .3,   # 
        beta_3     = .3,    # interaction effect
        omega_0    =  2,   # by-item random intercept sd
        tau_0      =  sd(vec),   # by-subject random intercept sd
        tau_1      =  2,   # by-subject random slope sd
        #tau_2      = .5,   # by-subject random interaction sd
        rho        = .01,   # correlation between intercept and slope
        sigma      = 4) { # residual (standard deviation)
    
    items <- data.frame(
        item_id = seq_len(n_emo*n_relation),
        emotion = rep(c("happiness","anger","sadness"), n_relation),
        relation_model = rep(c("CS", "ARd","ARs","EM","MP"), n_emo),
        X_i = rep(c(1, 1, 0), n_relation),
        Y_i = rep(c(0, -0.5, 0.5, 0, 0), n_emo),
        O_0i = rnorm(n = 1800, mean = 0, sd = omega_0)
    )
    
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
    
    crossing(subjects, items)  %>%
        mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
               temperature = beta_0 + T_0s + O_0i + (beta_1 + T_1s) * X_i + (beta_2 + T_1s) * Y_i +
                   (beta_3 + T_1s)*(X_i*Y_i) + e_si) %>%
        select(subj_id, item_id, emotion, X_i, relation_model, Y_i, temperature)
}

# simulate, analyze, and return a table of parameter estimates
single_run <- function(...) {
    # ... is a shortcut that forwards any arguments to 
    # my_sim_data(), the function created above
    dat_sim <- my_sim_data(...)
    mod_sim <- lmer(temperature ~ X_i*Y_i + (1 | subj_id), dat_sim)
    
    broom.mixed::tidy(mod_sim)
}

# run simulations and save to a file
n_runs <- 1000 # use at least 1000 to get stable estimates
sims <- purrr::map_df(1:n_runs, ~ single_run())
write_csv(sims, "sims4.csv")

# read saved simulation data
sims <- read_csv("sims4.csv", col_types = cols(
    # makes sure plots display in this order
    group = col_factor(ordered = TRUE),
    term = col_factor(ordered = TRUE)
))

sims %>%
    filter(effect == "fixed") %>%
    select(term, estimate, p.value)

# calculate mean estimates and power for specified alpha
alpha <- 0.05

pwr_df <- sims %>% 
    filter(effect == "fixed") %>%
    group_by(term) %>%
    summarize(
        mean_estimate = mean(estimate),
        mean_se = mean(std.error),
        power = mean(p.value < alpha),
        .groups = "drop"
    )
pwr_df

# saving the result
save(pwr_df, file = "pwr analysis ARd VS ARs b1=.1 & N = 20.Rdata")
