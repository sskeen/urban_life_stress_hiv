library(tidyverse)
library(lme4)  
library(car)   
library(gtsummary)

noah_filt <- read.csv("C:/Users/Ethan/Box/Gentrification PD CD4 Skeen Smith/gent_pat_cd4_61626.csv")

# Rescale buffer variables 
noah_filt <- noah_filt %>%
  mutate(across(starts_with("pd_call_"), scale, .names = "scaled_{.col}"),
         across(starts_with("vc_call_"), scale, .names = "scaled_{.col}"))

# Quick histogram/summary stats to reference 

hist(noah_filt$cd4_count, main = "Histogram of CD4 T-cell Count", xlab = "CD4 T-cell Count", col = "lightblue") # Slightly skewed 
table(noah_filt$cd4cat) # cd4cat: 1= <200 cells/mm3 | 2= 200-499 cells/mm3 | 3= 500+ cells/mm3
table(noah_filt$race)
table(noah_filt$gender)
table(noah_filt$polyusesum)
table(noah_filt$dailypolyuse)
table(noah_filt$auditcCAT) #1 =	Female: <3; Male: <4 | 2 =	Female: 3+; Male: 4+
table(noah_filt$homeless)
table(noah_filt$housing)
summary(noah_filt$lehtotal_sum)
summary(noah_filt$stigma_sum)

# Recodes

#Binary recodes 
noah_filt$cd4_bin <- ifelse(noah_filt$cd4cat==1,1,0) #low CD4 is <200 cells/mm^3
noah_filt$dep_bin <- ifelse(noah_filt$hads_depresscat==0,0,1) # This is normal vs. borderline/clinical 
noah_filt$anx_bin <- ifelse(noah_filt$hads_anxietycat==0,0,1) #  This is normal vs. borderline/clinical 
noah_filt$sixty_plus <- ifelse(noah_filt$age_current>60,1,0)
noah_filt$audit7plus <- ifelse(noah_filt$auditCAT==1,0,1) # for AUDIT > 7 

# Mean centering HIV stigma
stigma_mean <- mean(noah_filt$stigma_sum, na.rm = TRUE)
noah_filt$stigma_sum_centered <- noah_filt$stigma_sum - stigma_mean

# Scale ICE * 100
noah_filt$ice_race_scale <- noah_filt$ice_race*100

#Scale ML Scores
noah_filt$ML1scale <- noah_filt$ML1*100
noah_filt$ML2scale <- noah_filt$ML2*100


# Data seperation on Race; recode to black/non-black
noah_filt$black <- ifelse(noah_filt$race==4,1,0)

################################# Table 1 

table_one_data <- noah_filt %>%
  select(cd4_bin, race, gender, homeless, housing, lehtotal_sum, stigma_sum,
         auditcCAT, polyusesum, dailypolyuse, hads_depresscat, hads_anxietycat,
         gen_cat18, 
         pd_call_010m, vc_call_010m, pd_call_025m, vc_call_025m, pd_call_050m, vc_call_050m,
         scaled_pd_call_010m, scaled_vc_call_010m, scaled_pd_call_025m, scaled_vc_call_025m,
         scaled_pd_call_050m, scaled_vc_call_050m,
         ML1, ML2)

table_one <- table_one_data %>%
  tbl_summary(
    by = cd4_bin,  # Stratifying by cd4_bin
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",  #  mean (SD) for contin. var 
      all_categorical() ~ "{n} ({p}%)"    #  counts (percentages) for cat var
    ),
    digits = all_continuous() ~ 2,  # Round continuous variables to 2 decimal places
    missing = "no"  # Exclude missing values from the table?
  ) %>%
  add_p() %>%  # Add p-values for group comparisons
  bold_labels()  

table_one

##############################  Depression #####################################

############################### Table 2 

# Outcomes

#table(noah_filt$cd4cat)
#summary(noah_filt$cd4_count)
#table(noah_filt$hads_anxietycat) # 0 is normal, 1 is borderline anx, 2 is anx 
#table(noah_filt$hads_depresscat)


# If we want to rescale buffer variables 
#noah_filt <- noah_filt %>%
#  mutate(across(starts_with("pd_call_"), scale, .names = "scaled_{.col}"),
#         across(starts_with("vc_call_"), scale, .names = "scaled_{.col}"))


# Crude model function (categorical predictor):

run_crude_model <- function(predictor_var, outcome_var, data, exponentiate = TRUE) {

  formula <- as.formula(paste(outcome_var, "~ as.factor(", predictor_var, ")", sep = ""))
  
  crude_model <- glm(formula, data = data, family = binomial)
  
  tbl_regression(crude_model, exponentiate = exponentiate)
}

# Outcome var: anx_bin, dep_bin, cd4_bin

race_crude <- run_crude_model(predictor_var = "race", outcome_var = "dep_bin", data = noah_filt)
race_crude

gender_crude <- run_crude_model(predictor_var = "gender", outcome_var = "dep_bin", data = noah_filt)
gender_crude

gentrif_crude <- run_crude_model(predictor_var = "gen_cat18", outcome_var = "dep_bin", data = noah_filt)
gentrif_crude

incarcerated_crude <- run_crude_model(predictor_var = "incarcerated", outcome_var = "dep_bin", data = noah_filt)
incarcerated_crude

sixty_plus_crude <- run_crude_model(predictor_var = "sixty_plus", outcome_var = "dep_bin", data = noah_filt)
sixty_plus_crude

black_crude <- run_crude_model(predictor_var = "black", outcome_var = "cd4_bin", data = noah_filt)
black_crude

dep_dx_crude <- run_crude_model("medhx___36", "dep_bin", data = noah_filt)
dep_dx_crude

audit_crude <- run_crude_model("audit7plus", "dep_bin", data = noah_filt)
audit_crude


# Crude model function (continuous predictor):

run_crude_cont <- function(predictor_var, outcome_var, data, exponentiate = TRUE) {
  
  formula <- as.formula(paste(outcome_var, "~", predictor_var))
  
  crude_model <- glm(formula, data = data, family = binomial)
  
  tbl_regression(crude_model, exponentiate = exponentiate)
}

pd_call_10m_crude <- run_crude_cont("pd_call_010m", "dep_bin", data = noah_filt)
pd_call_10m_crude

pd_call_25m_crude <- run_crude_cont("pd_call_025m", "dep_bin", data = noah_filt)
pd_call_25m_crude

pd_call_50m_crude <- run_crude_cont("pd_call_050m", "dep_bin", data = noah_filt)
pd_call_50m_crude

vc_call_10m_crude <- run_crude_cont("vc_call_010m", "dep_bin", data = noah_filt)
vc_call_10m_crude

vc_call_25m_crude <- run_crude_cont("vc_call_025m", "dep_bin", data = noah_filt)
vc_call_25m_crude

vc_call_50m_crude <- run_crude_cont("vc_call_050m", "dep_bin", data = noah_filt)
vc_call_50m_crude

ice_scale_crude <- run_crude_cont("ice_race_scale", "dep_bin", data = noah_filt)
ice_scale_crude

lehtotal_sum_crude <- run_crude_cont("lehtotal_sum", "dep_bin", data = noah_filt)
lehtotal_sum_crude

ulss13_sum_crude <- run_crude_cont("ulss13", "dep_bin", data = noah_filt)
ulss13_sum_crude

ulss21_sum_crude <- run_crude_cont("ulss21", "dep_bin", data = noah_filt)
ulss21_sum_crude

noah_filt$GEOID
# Adjusted Models
# Individual Factor Adjustment 
adjusted_ind_dep <- glmer(cd4_bin ~ ML2scale + as.factor(audit7plus) +lehtotal_sum + as.factor(incarcerated) + as.factor(black) + as.factor(gender) + 
                              lehtotal_sum + as.factor(sixty_plus) + as.factor(medhx___36) + 
                              stigma_sum_centered + (1 | GEOID), 
                            data = noah_filt, family = binomial)

tbl_regression(adjusted_ind_dep, exponentiate = T)
noah_filt$ML2scale
# Environmental Adjustment 
adjusted_env_dep <- glmer(dep_bin ~ ulss13*as.factor(gen_cat18) + vc_call_010m + pd_call_010m + ice_race_scale + (1 | GEOID), 
                              data = noah_filt, family = binomial)

tbl_regression(adjusted_env_dep, exponentiate = T)

adjusted_env_dep <- glmer(dep_bin ~ ulss13*as.factor(gen_cat18) + vc_call_025m + pd_call_025m + ice_race_scale + (1 | GEOID), 
                          data = noah_filt, family = binomial)

tbl_regression(adjusted_env_dep, exponentiate = T)

adjusted_env_dep <- glmer(cd4_bin ~ ML2scale*as.factor(gen_cat18) + vc_call_050m + pd_call_050m + ice_race_scale + (1 | GEOID), 
                          data = noah_filt, family = binomial)

tbl_regression(adjusted_env_dep, exponentiate = T)

#Both 
adjusted_ind_env_dep <- glmer(cd4_bin ~ ML2scale*as.factor(gen_cat18) + vc_call_050m + pd_call_050m + ice_race_scale + lehtotal_sum + as.factor(incarcerated) + as.factor(black) + as.factor(gender) + 
                            lehtotal_sum + as.factor(sixty_plus) + as.factor(medhx___36) + 
                            stigma_sum_centered + (1 | GEOID), 
                          data = noah_filt, family = binomial)

tbl_regression(adjusted_ind_env_dep, exponentiate = T)


################################################# CD4 


######################################## Table 2


# Crude model function (categorical predictor):

run_crude_model <- function(predictor_var, outcome_var, data, exponentiate = TRUE) {
  
  formula <- as.formula(paste(outcome_var, "~ as.factor(", predictor_var, ")", sep = ""))
  
  crude_model <- glm(formula, data = data, family = binomial)
  
  tbl_regression(crude_model, exponentiate = exponentiate)
}

# Outcome var: anx_bin, cd4_bin, cd4_bin

race_crude <- run_crude_model(predictor_var = "race", outcome_var = "cd4_bin", data = noah_filt)
race_crude

gender_crude <- run_crude_model(predictor_var = "gender", outcome_var = "cd4_bin", data = noah_filt)
gender_crude

gentrif_crude <- run_crude_model(predictor_var = "gen_cat18", outcome_var = "cd4_bin", data = noah_filt)
gentrif_crude

incarcerated_crude <- run_crude_model(predictor_var = "incarcerated", outcome_var = "cd4_bin", data = noah_filt)
incarcerated_crude

sixty_plus_crude <- run_crude_model(predictor_var = "sixty_plus", outcome_var = "cd4_bin", data = noah_filt)
sixty_plus_crude

dailypolyuse_crude <- run_crude_model(predictor_var = "dailypolyuse", outcome_var = "cd4_bin", data = noah_filt)
dailypolyuse_crude

art_crude <- run_crude_model(predictor_var = "art", outcome_var = "cd4_bin", data = noah_filt)
art_crude

audit7plus_crude <- run_crude_model(predictor_var = "audit7plus", outcome_var = "cd4_bin", data = noah_filt)
audit7plus_crude


# Crude model function (continuous predictor):

run_crude_cont <- function(predictor_var, outcome_var, data, exponentiate = TRUE) {
  
  formula <- as.formula(paste(outcome_var, "~", predictor_var))
  
  crude_model <- glm(formula, data = data, family = binomial)
  
  tbl_regression(crude_model, exponentiate = exponentiate)
}

pd_call_10m_crude <- run_crude_cont("pd_call_010m", "cd4_bin", data = noah_filt)
pd_call_10m_crude

pd_call_25m_crude <- run_crude_cont("pd_call_025m", "cd4_bin", data = noah_filt)
pd_call_25m_crude

pd_call_50m_crude <- run_crude_cont("pd_call_050m", "cd4_bin", data = noah_filt)
pd_call_50m_crude

vc_call_10m_crude <- run_crude_cont("vc_call_010m", "cd4_bin", data = noah_filt)
vc_call_10m_crude

vc_call_25m_crude <- run_crude_cont("vc_call_025m", "cd4_bin", data = noah_filt)
vc_call_25m_crude

vc_call_50m_crude <- run_crude_cont("vc_call_050m", "cd4_bin", data = noah_filt)
vc_call_50m_crude

ice_scale_crude <- run_crude_cont("ice_race_scale", "cd4_bin", data = noah_filt)
ice_scale_crude

ulss13_sum_crude <- run_crude_cont("ulss13", "cd4_bin", data = noah_filt)
ulss13_sum_crude

ulss21_sum_crude <- run_crude_cont("ulss21", "cd4_bin", data = noah_filt)
ulss21_sum_crude

ML1_sum_crude <- run_crude_cont("ML1scale", "cd4_bin", data = noah_filt)
ML1_sum_crude

ML2_sum_crude <- run_crude_cont("ML2scale", "cd4_bin", data = noah_filt)
ML2_sum_crude


