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

lfs_arm_sdc_object <- ldiversity(obj = lfs_arm_sdc_object, ldiv_index = c("E15"), l_recurs_c = 2, missing = NA)
# Output for l-diversity
lfs_arm_sdc_object@risk$ldiversity
# l-diversity score for each record
lfs_arm_sdc_object@risk$ldiversity[,'E15_Distinct_Ldiversity']


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
# The highest risk is 0.715, which is a very high individual risk
# We can interpret that risk as the probability to identify an individual in the population
# from an individual in the sample.
# The average risk is set to 0.033 but one quarter of the individuals of the sample
# have a risk over 0.05.

lfs_arm_indiv_risks %>%
  ggplot() +
  geom_histogram(aes(x = risk), binwidth = 0.005) +
  geom_vline(xintercept = 0.05, col = "orangered", linetype = "dashed") +
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
      select(all_of(key_variables_arm),WeightsCalib_year)
  ) %>%
  filter(fk <=5) %>% 
  ggplot() +
  geom_point(aes(col = as.factor(fk), x=WeightsCalib_year, y = risk), alpha = 0.65, size = 0.5) +
  scale_color_brewer("frequency count", type="qual", palette = 7) +
  ggtitle(
    label = "Individual risk as a function of Weights, by frequency counts",
    subtitle = "Only frequency counts <= 5 are drawn"
  )

# There is a link between the individual risk estimated by sdcMicro and the 
# the empirical estimation of the risk by 1/Fk where Fk is estimated by the 
# sum of weights of individuals in each 

lfs_arm_indiv_risks %>%
  bind_cols(
    lfs_micro_arm_2020 %>% 
      select(all_of(key_variables_arm),WeightsCalib_year)
  ) %>%
  mutate(ratio = fk/Fk, risk_emp = 1/Fk) %>% 
  ggplot() +
  geom_point(aes(x=risk_emp, y=risk)) +
  geom_abline(slope = 1, yintercept = 0) +
  facet_wrap(~fk, scales = "free")







