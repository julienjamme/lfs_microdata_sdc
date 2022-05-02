# Assessing disclosure risk of lfs microdata
library(sdcMicro)
library(dplyr)

#french public file
lfs_micro_fr_2020 = read.csv2(
  "data/FD_csv_EEC20.csv",
  header = TRUE,
  sep = ";",
  quote = "\"",
  dec = ".",
  fill = TRUE, 
  comment.char = ""
)
#armenian file
lfs_micro_arm_2020 <- haven::read_spss("data/LFS_2020_2020 Dataset_Residents.sav")

# Dimensions of tables
dim(lfs_micro_arm_2020) #27986 records and 191 variables
dim(lfs_micro_fr_2020) #319398 records and 124 variables

# Variables 
colnames(lfs_micro_arm_2020)
colnames(lfs_micro_fr_2020)

# Presence of identifiers: No

# Which potential key variables ?
# Sex : arm => B3; fr => SEXE
# Age : arm => B5+B6 = Date of birth (month + year); fr => AGE5 and AGE3
lfs_micro_arm_2020 %>%
  group_by(B6, B5) %>%
  count() %T>%
  print() %>%
  filter(n==1) %>%
  nrow()

lfs_micro_fr_2020 %>%
  group_by(AGE3, AGE5) %>%
  count()

# Geographical Area : 
# arm => A3 = Marz (equivalent of province), A5 = Urban/Rural class; 
lfs_micro_arm_2020 %>%
  group_by(A3, A5) %>%
  count()

# fr => CATAU2010R = like a urban vs rural categories; METRODOM = metropolitan france or overseas territories
lfs_micro_fr_2020 %>%
  group_by(METRODOM, CATAU2010R) %>%
  count()

# Marital Status : arm => B11; fr => absent but the type of household is displayed (TYPMEN7)
lfs_micro_arm_2020 %>%
  group_by(B11) %>%
  count()

lfs_micro_fr_2020 %>%
  group_by(TYPMEN7) %>%
  count()

# Occupation : arm => maybe ; fr => ACTEU6 
lfs_micro_arm_2020 %>%
  group_by(empj, emsj, iempj, iemsj, LU1_unemployed, LU2_combined_nomin, long_unemployed) %>%
  count()

# Sensitive variables : arm => maybe E15 (income); fr => none

