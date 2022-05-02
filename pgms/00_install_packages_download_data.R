
# Parameters and initial downloads and installations

# Downloading required packages ####

packs <- c("archive","sdcMicro","data.table","dplyr","haven")
p <- sapply(packs, install.packages)



# Downloading data ####

options(timeout = 6000)

# french lfs microdata released to the public on the website of Insee (www.insee.fr)
download.file(
  destfile = "data/lfs_micro_fr_2020.zip",
  url = "https://www.insee.fr/fr/statistiques/fichier/5393560/fd_eec20_csv.zip"
)
unzip(zipfile = "data/lfs_micro_fr_2020.zip", exdir = "data/")
# To read it
lfs_micro_fr_2020 = read.csv2(
  "data/FD_csv_EEC20.csv",
  header = TRUE,
  sep = ";",
  quote = "\"",
  dec = ".",
  fill = TRUE, 
  comment.char = ""
)
str(lfs_micro_fr_2020)
# Documentation of french microdata available here :
# https://www.insee.fr/fr/statistiques/fichier/5393560/EEC2020_Dictionnaire_Fichier_Detail.pdf (only in french)

# armenian lfs microdata released to the public on the website of Armstat
download.file(
  destfile = "data/lfs_micro_arm_2020.7z",
  url = "https://www.armstat.am/file/doc/99528268.7z"
)
archive::archive_extract("data/lfs_micro_arm_2020.7z",dir = "data/")
# To read it 
lfs_micro_arm_2020 <- haven::read_spss("data/LFS_2020_2020 Dataset_Residents.sav")
str(lfs_micro_arm_2020, max.level = 2)
colnames(lfs_micro_arm_2020)

# The Questionnaire is here https://www.armstat.am/file/doc/99528263.pdf

