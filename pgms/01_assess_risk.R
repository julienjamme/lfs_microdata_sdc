# Assessing the risk of identification

library(sdcMicro)

# Build a DOB variable in pasting B5 (Month of Birth) and B6 (Year of Birth)
lfs_micro_arm_2020 <- lfs_micro_arm_2020 %>%
  mutate(DOB = paste(B5,B6,sep="_"))

# Set key variables (individual level)
key_variables_arm <- c(
  'B3' # Gender
  ,'DOB' # DOB => age
  ,'A3','A5' # Geographical area (Province, Urban/Rural)
  ,'B11' #Marital Status
  ,'B7' # Diploma
)

lfs_arm_sdc_object <- createSdcObj(
  dat=lfs_micro_arm_2020,
  keyVars=key_variables_arm,
  weightVar="WeightsCalib_year"
)

# k-anonymity
print(lfs_arm_sdc_object)


