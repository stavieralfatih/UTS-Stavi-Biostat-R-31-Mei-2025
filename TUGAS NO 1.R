#TUGAS NO 1
#No 3 Membuka data dari link url
data_pef <- read.csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")
#Run tidyverse dulu
library(tidyverse) 
#Menemukan duplikasi data pidlink
data_pef %>%
  count(pidlink) %>%
  filter (n > 1)
#Menghapus duplikasi
pef_clean = data_pef %>% distinct(pidlink, .keep_all = TRUE)
write.csv(pef_clean, "pef_tanpa_duplikat.csv", row.names = FALSE)
