library(RODBC)
library(tidyverse)
library(lubridate)
library(quantmod)
library(reshape2)

current_tournament = "zozo" # tourtips path
current_Event = "Zozo Championship" # data from master file

# Masterfile from database
file_path = "C:/Users/Pastor/Desktop/Golf/Macro/Masterfile"
con <- odbcConnectAccess(file.path(file_path, 'Men_Master - TwoTours.mdb'))
re <- sqlFetch(con, 'Results')
re_subset = subset(re, Event == current_Event)
re_subset$year = year(re_subset$Date)
re_subset$player <- paste(re_subset$FirstName, re_subset$Surname)
current_tour <- re_subset %>% select(player, Date, year, Posn, Rd1, Rd2, Rd3, Rd4)


par <- sqlFetch(con, 'ResultsPar345Table')
par$player <- par$Player
par$year <- year(par$Date)

sg <- sqlFetch(con, 'StrokesGained')
sg$player <- paste(sg$FirstName, sg$Surname)
sg$year <- year(sg$Date)

# Bip stats downloaded from tourtips and use the excel macro to get a single file
bip_path = sprintf("C:/Users/Pastor/Desktop/Golf/%s", current_tournament)
bip <- readxl::read_excel(file.path(bip_path, "tour_tips.xlsx"), na = c("", "-", "NaN"))
colnames(bip)[1] <- "player"
colnames(bip)[17] <- "year"
bip <- bip %>% select(player, year, Current:Location)
bip <- bip %>% left_join(re_subset, by = c("player", "year"))

# Get the par gained data using the available dataframe
par_Gained <- ParGainedData(sg, bip, current_tour, par)

# get the stroke gained data
stroke_gained <- StrokeGainedData(bip, sg, current_tour)

# Join the two dataframes
final_file <- par_Gained %>% inner_join(stroke_gained, by = c("player", "year")) %>% select(player, year, Posn, round_total, Current:Location, sgtot4:sgp_all, par34:Ss_all)

final_file <- final_file %>% filter(round_total >= 10)
final_file <- final_file[order(final_file$year, final_file$player),]
df_2021 <- subset(final_file, year == 2021)
df <- subset(final_file, year !=2021)
df <- df[!is.na(df$Posn),]

# Export the data
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Current")
writeData(wb, sheet = "Current", x= df_2021)
addWorksheet(wb, sheetName = "Historic")
writeData(wb, sheet = "Historic", x= df)
saveWorkbook(wb, file.path(bip_path, "Tournament.xlsx"), overwrite = TRUE)
openXL("Tournament.xlsx")
