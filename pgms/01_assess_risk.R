# Assessing the risk of identification
library(sdcMicro)
library(dplyr)
library(ggplot2)

# Build a DOB variable in pasting B5 (Month of Birth) and B6 (Year of Birth)
# IDHH : identification of households (=A1+A6_Month) or first digits of IDmem
lfs_micro_arm_2020 <- lfs_micro_arm_2020 %>%
  mutate(
    DOB = paste(B5,B6,sep="_"),
    IDHH = as.numeric(substr(IDmem,1,nchar(IDmem)-2))
  ) %>%
  mutate(
    AGE = cut((2020-as.numeric(B6)), breaks = seq(0,110,10),right=FALSE, ordered_result = TRUE)
  )

ggplot(lfs_micro_arm_2020) +
  geom_bar(aes(x=AGE))
#Maybe the last two or last three or last four categories could be grouped together

# Set key variables (individual level)
# first attempt with gender, date of birth and geographical area variables as keys
key_variables_arm <- c(
  'B3' # Gender
  ,'DOB' # DOB => age
  ,'A3','A5' # Geographical area (Province, Urban/Rural)
  # ,'B11' #Marital Status
  # ,'B7' # Diploma
)

key_variables_arm_alter <- c(
  'B3' # Gender
  ,'AGE' # 10 categories of AGE
  ,'A3','A5' # Geographical area (Province, Urban/Rural)
  # ,'B11' #Marital Status
  # ,'B7' # Diploma
)

potential_sensitive_variables <- c("C2","E15","long_unemployed")

compute_number_of_actual_keys <- function(data, keys_var){
  data %>%
    group_by(across(all_of(keys_var))) %>%
    count() %>%
    nrow()
}

compute_number_of_actual_keys(lfs_micro_arm_2020, key_variables_arm)
# With B3, DOB A3 and A5 key variables, there are 18508 actual combinations.
# This amount is due to the very detailed variable of age.

compute_number_of_actual_keys(lfs_micro_arm_2020, key_variables_arm_alter)
#Only 414 keys in this case

lfs_arm_sdc_object <- createSdcObj(
  dat=lfs_micro_arm_2020,
  keyVars=key_variables_arm,
  hhId = "IDHH", #Has to be an integer variable
  weightVar="WeightsCalib_year",
  seed = 20061789
)

# k-anonymity with sdcMicro
print(lfs_arm_sdc_object) #by default print for k = 2 or 3

lfs_arm_indiv_risks <- lfs_arm_sdc_object@risk$individual %>%
  as_tibble()

# Number of individuals by frequency counts in combinations of key variables
# There are 12149 individuals in the sample that are unique in a combination of keys.
lfs_arm_indiv_risks %>%
  count(fk) %>% 
  mutate(share = n/nrow(lfs_arm_indiv_risks)*100) %>%
  mutate(n_cum = cumsum(n), share_cum = cumsum(share))

kano <- function(sdcObj, k){
  sdcObj@risk$individual %>%
    as_tibble() %>% 
    mutate(test_kanon = fk < k) %>%
    count(test_kanon) %>%
    filter(test_kanon) %>%
    summarise(n = sum(n), .groups = 'drop') %>%
    mutate(nb_cells_total = nrow(lfs_arm_indiv_risks), share_risky_cells = n/nb_cells_total)
}
kano_vect <- Vectorize(kano, "k", USE.NAMES = TRUE)
kano_vect(lfs_arm_sdc_object, setNames(2:5,2:5))

# SAMPLE UNIQUES in the original data
lfs_micro_arm_2020 %>% 
  filter(lfs_arm_indiv_risks$fk < 2) %>%
  select(all_of(key_variables_arm), WeightsCalib_year, AGE) %>%
  arrange(WeightsCalib_year) %>%
  mutate(
    rk = 1/WeightsCalib_year
  ) %>%
  View()


# Example of l-diversity assessment
# Computing l-diversity

lfs_arm_sdc_object <- ldiversity(
  obj = lfs_arm_sdc_object,
  ldiv_index = c("C2"),
  l_recurs_c = 2, 
  missing = NA
)

# Output for l-diversity
lfs_arm_sdc_object@risk$ldiversity
# l-diversity score for each record
lfs_micro_arm_2020 %>% 
  filter(
    lfs_arm_indiv_risks$fk > 2 & #cells not concerned by risk measured by k-anonymity
      lfs_arm_sdc_object@risk$ldiversity[,"C2_Distinct_Ldiversity"] == 1
    ) %>%
  select(all_of(key_variables_arm),AGE,C2) %>%
  filter(C2 != 1) # remove armenian nationality which is not a potential sensitive information
# No problem of l-diversity for the Nationality variable in that case, 

lfs_arm_indiv_risks <- lfs_arm_sdc_object@risk$individual %>%
  as_tibble()

# The Risk measure proposed by sdcMicro
# A low frequency count in the sample is problematic because if we know that the 
# individual is in the sample, so we can deduce all its other released attributes.
# But, the k-anonymity measure doesn't take into account the fact that we are dealing 
# with a sample. Is there a risk to identify an individual in the population from 
# data released in the sample. 
# sdcMicro proposes an estimation of the risk to identify an individual 
# within the population given the sample.
print(lfs_arm_sdc_object, "risk")

summary(lfs_arm_indiv_risks$risk)
quantile(lfs_arm_indiv_risks$risk, probs=seq(0,1,0.01))

# The highest risk is 0.715, which is a very high individual risk
# We can interpret that risk as the probability to identify an individual in the population
# from an individual in the sample.
# the individuals with maximum risk of reidentification
lfs_micro_arm_2020 %>%
  filter(lfs_arm_indiv_risks$risk == max(lfs_arm_indiv_risks$risk)) %>%
  select(all_of(key_variables_arm), AGE, C2,E15) %>%
  View()
# The average risk is set to 0.033 but one quarter of the individuals of the sample
# have a risk over 0.05 (5% of probability to be reidentified taking into account the weights);
# and 10% of individuals have a risk over 8% 

lfs_arm_indiv_risks %>%
  ggplot() +
  geom_histogram(aes(x = risk), binwidth = 0.005) +
  geom_vline(xintercept = c(0.05,0.1), col = "orangered", linetype = "dashed") +
  scale_x_continuous("individual risk", breaks = seq(0,0.8,0.05), expand = c(0,0)) +
  ggtitle(
    label = "Individual risks distribution"
  )

# Risk vs frequency counts

lfs_arm_indiv_risks %>%
  ggplot() +
  geom_boxplot(aes(group=as.factor(fk), x = risk)) +
  scale_y_continuous("fk", labels = 1:11, breaks = seq(-0.333,0.4,0.0667), expand = c(0,0)) +
  ggtitle(
    label = "Individual risks distribution by sampled frequency of combinations of key variables"
  )
# In general, smaller is the frequency count within the combinations of key variables, higher the individual risk 

lfs_arm_indiv_risks %>%
  bind_cols(
    lfs_micro_arm_2020 %>% 
      select(all_of(key_variables_arm),WeightsCalib_year,AGE,all_of(potential_sensitive_variables))
  ) %>%
  filter(fk <= 5) %>% 
  ggplot() +
  geom_point(aes(col = as.factor(fk), x=WeightsCalib_year, y = risk), alpha = 0.65, size = 0.5) +
  scale_color_brewer("frequency count", type="qual", palette = 7) +
  ggtitle(
    label = "Individual risk as a function of Weights, by frequency counts",
    subtitle = "Only frequency counts <= 5 are drawn"
  )


# Summary:

# With four variables as identifiers (DOB, Area (2), gender):

# Measure risk based on the sample
# The file is 1-anonymized
# With 43% of individuals are sample uniques
# 74% of individuals in the microdata are risky if we set threshold k of k-anonymity to 3
# l-diversity on the Nationality variable (Assumption (maybe false) : Nationality is a sensitive variable)
# The file is 1-diverse 
# If we remove Armenian Nationality (not sensitive), there is no problem here

# Measure risk based on the population
# Global risk = mean of the individual risks = 0.033 (not significant before proceeding protection)
# If we choose a threshold of 10% for the risk of re-identification,  
# 6% of individuals are risky individuals

# Protection step 

# FIRST STEP ####

# First goal : reduce the sample uniques from the data

# Global recoding of DOB variable (very detailed and for sure the cause of many sample uniques)
# What is the effect of creating a categorical variable of age ?

lfs_micro_arm_2020 <- lfs_micro_arm_2020 %>%
  mutate(
    AGE = cut(
      (2020-as.numeric(B6)),  #some improvements could be done here
      breaks = seq(0,110,10),
      right=FALSE, ordered_result = TRUE)
  )

lfs_micro_arm_2020 %>%
  count(AGE)

ggplot(lfs_micro_arm_2020) +
  geom_bar(aes(x=AGE))
#Maybe the last two or last three or last four categories could be grouped together

key_variables_arm_alter <- c(
  'B3' # Gender
  ,'AGE' # 10 categories of AGE
  ,'A3','A5' # Geographical area (Province, Urban/Rural)
  # ,'B11' #Marital Status
  # ,'B7' # Diploma
)

lfs_arm_sdc_object_alter <- createSdcObj(
  dat=lfs_micro_arm_2020,
  keyVars=key_variables_arm_alter,
  hhId = "IDHH", #Has to be an integer variable
  weightVar="WeightsCalib_year",
  seed = 20061789
)

# k-anonymity with sdcMicro
print(lfs_arm_sdc_object_alter) #by default print for k = 2 or 3

lfs_arm_indiv_risks_alter <- lfs_arm_sdc_object_alter@risk$individual %>%
  as_tibble()

# Number of individuals by frequency counts in combinations of key variables
# There are 12149 individuals in the sample that are unique in a combination of keys.
lfs_arm_indiv_risks_alter %>%
  count(fk) %>% 
  mutate(share = n/nrow(lfs_arm_indiv_risks)*100) %>%
  mutate(n_cum = cumsum(n), share_cum = cumsum(share))

kano_vect(lfs_arm_sdc_object_alter, setNames(2:5,2:5))

# SAMPLE UNIQUES in the original data
lfs_micro_arm_2020 %>% 
  filter(lfs_arm_indiv_risks_alter$fk < 2) %>%
  select(all_of(key_variables_arm_alter), WeightsCalib_year) %>%
  arrange(WeightsCalib_year) %>%
  mutate(
    rk = 1/WeightsCalib_year
  ) %>%
  View()

# Summary with only 14 SAMPLE UNIQUES
# All these sample uniques are very old people => we can use top recoding
# to suppress at least cost all the sample uniques based on the three chosen keys
# We choose to put all the 80 or older in the same categories

lfs_micro_arm_2020 <- lfs_micro_arm_2020 %>%
  mutate(
    AGE_top = cut(
      (2020-as.numeric(B6)),  #some improvements could be done here
      breaks = c(seq(0,80,10),110),
      right=FALSE, ordered_result = TRUE)
  )

lfs_micro_arm_2020 %>%
  count(AGE_top)

ggplot(lfs_micro_arm_2020) +
  geom_bar(aes(x=AGE_top))
#Maybe the last two or last three or last four categories could be grouped together

key_variables_arm_topage <- c(
  'B3' # Gender
  ,'AGE_top' # 8 categories of AGE
  ,'A3','A5' # Geographical area (Province, Urban/Rural)
  # ,'B11' #Marital Status
  # ,'B7' # Diploma
)

lfs_arm_sdc_object_topage <- createSdcObj(
  dat=lfs_micro_arm_2020,
  keyVars=key_variables_arm_topage,
  hhId = "IDHH", #Has to be an integer variable
  weightVar="WeightsCalib_year",
  seed = 20061789
)

# k-anonymity with sdcMicro
print(lfs_arm_sdc_object_topage) #by default print for k = 2 or 3

lfs_arm_indiv_risks_topage <- lfs_arm_sdc_object_topage@risk$individual %>%
  as_tibble()

lfs_arm_indiv_risks_topage %>%
  count(fk) %>% 
  mutate(share = n/nrow(lfs_arm_indiv_risks_topage)*100) %>%
  mutate(n_cum = cumsum(n), share_cum = cumsum(share))

kano_vect(lfs_arm_sdc_object_topage, setNames(2:5,2:5))

# Individual risk
print(lfs_arm_sdc_object, "risk")

summary(lfs_arm_indiv_risks_topage$risk)
quantile(lfs_arm_indiv_risks_topage$risk, 
         probs=seq(0,1,0.1))


#Summary after recode and top code the Age variable :
# File is now 4-anonymized !
# So there is no more SAMPLE UNIQUES



