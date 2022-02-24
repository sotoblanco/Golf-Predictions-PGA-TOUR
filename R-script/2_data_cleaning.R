###### Data preparation ####
setwd("D:/Golf/2022/honda")
library(readxl)
df <- read_excel("Honda Classic_NA.xlsx", sheet = "Historic", na = c("", "-", "NaN")) # read the spreadsheet including using na for blank and - spaces
library(janitor)
df <- clean_names(df)
library(dplyr)
df_2022 <- read_excel("Honda Classic_NA.xlsx",sheet = "Current", na = c("", "-", "NaN"))
df_2022 <- clean_names(df_2022)
df_2022[names(df)]
df <- rbind(df_2022,df)

# explore missing values
sum(is.na(df))
#19191
naniar::gg_miss_var(df)
df <- df %>%
  group_by(player) %>%
  mutate(across(c(current:ss_all), ~replace(., is.na(.), median(., na.rm = TRUE)))) %>%
  ungroup() %>%
  mutate(across(c(current:ss_all), ~replace(., is.na(.), median(., na.rm = TRUE))))
sum(is.na(df))
naniar::gg_miss_var(df)
#18704
df <- df %>% select(player:ss_all)

sgtot <- grep("sgtot", names(df))
Xs_seq <- rev(seq_along(sgtot))
sgtot <- as.matrix(df[sgtot]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgtot <- sgtot

sgt2g <- grep("sgt2g", names(df))
Xs_seq <- rev(seq_along(sgt2g))
sgt2g <- as.matrix(df[sgt2g]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgt2g <- sgt2g


sgtee <- grep("sgtee", names(df))
Xs_seq <- rev(seq_along(sgtee))
sgtee <- as.matrix(df[sgtee]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgtee <- sgtee

sgbs <- grep("sg_bs", names(df))
Xs_seq <- rev(seq_along(sgbs))
sgbs <- as.matrix(df[sgbs]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgbs <- sgbs

sgsg <- grep("sg_sg", names(df))
Xs_seq <- rev(seq_along(sgsg))
sgsg <- as.matrix(df[sgsg]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgsg <- sgsg


sgatg <- grep("sgatg", names(df))
Xs_seq <- rev(seq_along(sgatg))
sgatg <- as.matrix(df[sgatg]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgatg <- sgatg

sgapp <- grep("sgapp", names(df))
Xs_seq <- rev(seq_along(sgapp))
sgapp <- as.matrix(df[sgapp]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgapp <- sgapp

sgp <- grep("sgp", names(df))
Xs_seq <- rev(seq_along(sgp))
sgp <- as.matrix(df[sgp]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgp <- sgp

par3 <- grep("par3", names(df))
Xs_seq <- rev(seq_along(par3))
sgp <- as.matrix(df[sgp]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$sgp <- sgp

par4 <- grep("par4", names(df))
Xs_seq <- rev(seq_along(par4))
par4 <- as.matrix(df[par4]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$par4 <- par4

par5 <- grep("par5", names(df))
Xs_seq <- rev(seq_along(par5))
par5 <- as.matrix(df[par5]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$par5 <- par5

dd <- grep("dd", names(df))
Xs_seq <- rev(seq_along(dd))
dd <- as.matrix(df[dd]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$dd <- dd

da <- grep("da", names(df))
Xs_seq <- rev(seq_along(da))
da <- as.matrix(df[da]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$da <- da

gir <- grep("gir", names(df))
Xs_seq <- rev(seq_along(gir))
gir <- as.matrix(df[gir]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$gir <- gir

ppgir <- grep("ppgir", names(df))
Xs_seq <- rev(seq_along(ppgir))
ppgir <- as.matrix(df[ppgir]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$ppgir <- ppgir

ss <- grep("ss", names(df))
Xs_seq <- rev(seq_along(ss))
ss <- as.matrix(df[ss]) %*% matrix(Xs_seq, ncol = 1) / sum(Xs_seq)
df$ss <- ss



# out the data
df <- df %>% dplyr::select(player:location,sgtot:ss)
write.csv(df, "data.csv", row.names = FALSE)

test_2022 <- df %>% filter(year == 2022)
write.csv(test_2022, "test_2022.csv", row.names = FALSE)

train_2022 <- df %>% filter(year != 2022)
write.csv(train_2022, "train_2022.csv", row.names = FALSE)