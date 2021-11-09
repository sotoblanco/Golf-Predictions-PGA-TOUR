library(RODBC)
library(tidyverse)
library(lubridate)
library(quantmod)
library(reshape2)
library(data.table)
#file_functions = "C:/Users/Pastor/Desktop/Golf/Golf-Predictions-PGA-TOUR/R-script"
#source(file.path(file_functions, "FunctionsPairGainedRounds.R"))
#source(file.path(file_functions, "FunctionsStrokeGainedRounds.R"))


current_tournament = "houston" # tourtips path
current_Event = "Houston Open" # data from master file
#same_event = "OHL Classic"
## Filter by rounds NA if you want the whole data, 1 just round 1, 2 round 2, and 3 round3
Round = NA



# Masterfile from database
file_path = "C:/Users/Pastor/Desktop/Golf/Macro/Masterfile"
con <- odbcConnectAccess(file.path(file_path, 'Men_Master - TwoTours.mdb'))
re <- sqlFetch(con, 'Results')

re_subset = subset(re, Event == "Houston Open")

#re_subset = subset(re, Location == "Mexico")
#re_subset = subset(re, Event == current_Event | Event == same_event)


re_subset$year = year(re_subset$Date)
unique(re_subset$year)
unique(re_subset$Event)
unique(re_subset$Course)
unique(re_subset$Tour)


re_subset$player <- paste(re_subset$FirstName, re_subset$Surname)
current_tour <- re_subset %>% select(player, Date, year, Posn, Rd1, Rd2, Rd3, Rd4)

par <- sqlFetch(con, 'ResultsPar345Table')
par$player <- par$Player
par$year <- year(par$Date)

# pick the variables to use on the par data
par_filter <- par %>% select(player, year, Event, Date,
                             AvgP3PerPlayerR1, AvgP3PerPlayerR2, AvgP3PerPlayerR3, AvgP3PerPlayerR4,
                             AvgP4PerPlayerR1, AvgP4PerPlayerR2, AvgP4PerPlayerR3, AvgP4PerPlayerR4,
                             AvgP5PerPlayerR1, AvgP5PerPlayerR2, AvgP5PerPlayerR3, AvgP5PerPlayerR4,
                             AvgDDPerPlayerR1, AvgDDPerPlayerR2, AvgDDPerPlayerR3, AvgDDPerPlayerR4,
                             AvgDAPerPlayerR1, AvgDAPerPlayerR2, AvgDAPerPlayerR3, AvgDAPerPlayerR4,
                             AvgGirPerPlayerR1, AvgGirPerPlayerR2, AvgGirPerPlayerR3, AvgGirPerPlayerR4,
                             AvgPpGirPerPlayerR1, AvgPpGirPerPlayerR2, AvgPpGirPerPlayerR3, AvgPpGirPerPlayerR4,
                             AvgSsPlayerR1, AvgSsPlayerR2, AvgSsPlayerR3, AvgSsPlayerR4)

sg <- sqlFetch(con, 'StrokesGained')
sg$player <- paste(sg$FirstName, sg$Surname)
sg$year <- year(sg$Date)

# Bip stats downloaded from tourtips and use the excel macro to get a single file
bip_path = sprintf("C:/Users/Pastor/Desktop/Golf/%s", current_tournament)
bip <- readxl::read_excel(file.path(bip_path, "tour_tips.xlsx"), na = c("", "-", "NaN"), guess_max = 10000)
colnames(bip)[1] <- "player"
colnames(bip)[17] <- "year"
bip <- bip %>% select(player, year, Current:Location)
# left join is important given that you want to have people that played but also 2021 which didn't play yet so there are not information on current tour dataframe
bip <- bip %>% left_join(current_tour, by = c("player", "year"))


players_df <- data.frame(bip$player) # get the players name that played this tournament before and will play this year
colnames(players_df) <- "player"
players_df <- unique(players_df)

# Use only the important variables for Stroke gained stats
sg_filter <- sg %>% select(player, year, Posn, Tour, Event, Date,
                           `sgtot(r1)`, `sgtot(r2)`, `sgtot(r3)`,`sgtot(r4)`,
                           `sgt2g(r1)`, `sgt2g(r2)`, `sgt2g(r3)`, `sgt2g(r4)`,
                           `sgapp(r1)`, `sgapp(r2)`, `sgapp(r3)`, `sgapp(r4)`,
                           `sgatg(r1)`, `sgatg(r2)`, `sgatg(r3)`, `sgatg(r4)`,
                           `sgtee(r1)`, `sgtee(r2)`, `sgtee(r3)`, `sgtee(r4)`,
                           `sgp(r1)`, `sgp(r2)`, `sgp(r3)`, `sgp(r4)`)

colnames(sg_filter) <- c("player", "year", "Posn", "Tour", "Event", "Date",
                         "sgtotr1", "sgtotr2", "sgtotr3", "sgtotr4",
                         "sgt2gr1", "sgt2gr2", "sgt2gr3", "sgt2gr4",
                         "sgappr1", "sgappr2", "sgappr3", "sgappr4",
                         "sgatgr1", "sgatgr2", "sgatgr3", "sgatgr4",
                         "sgteer1", "sgteer2", "sgteer3", "sgteer4",
                         "sgpr1", "sgpr2", "sgpr3", "sgpr4")


relevant_players <- function(data_filter, previous_players){
  # function that gets relevant players on the tournament
  data_filter <- previous_players %>% left_join(data_filter, by = c("player"))
  data_fil <- data_filter %>% group_by(player) %>% arrange(Date, .by_group = TRUE)
  data_fil <- data.table(data_fil)
  return(data_fil)
}

par_Data <- relevant_players(par_filter, players_df)
sg_Data <- relevant_players(sg_filter, players_df)

# ignore player that do withdraw from the tournament
sg_Data <- sg_Data[!(sg_Data$Posn == "w" | is.na(sg_Data$Posn)),]

# subtract four days to get the information before the tournament given that this date is the day when the tournament ends
dates_tour <- c(min(sg_filter$Date), unique(current_tour$Date) - days(4))

sg_patt <- c("sgtot", "sgt2g", "sgtee", "sgatg", "sgapp", "sgp")
par_patt <- c("AvgP3", "AvgP4", "AvgP5","AvgDD", "AvgDA", "AvgG", "AvgPp",  "AvgS")

sg_fil_long <- data.table::melt(sg_Data, measure = patterns(sg_patt))

colnames(sg_fil_long)[8:13] <- sg_patt

par_fil_long <- data.table::melt(par_Data, measure = patterns(par_patt))

colnames(par_fil_long)[6:13] <- c("par3", "par4", "par5", "DD", "DA", "Gir", "Ppgir", "Ss")

sg_fil_long <- sg_fil_long %>% group_by(player) %>% arrange(Date, .by_group = TRUE) %>% 
  drop_na(sgtot) %>% mutate(rounds_play = 1,
                            cum_sum_round = cumsum(rounds_play),
                            sg_bs = sgtee + sgapp,
                            sg_sg = sgatg + sgp)# %>% select(player, Date, year, Posn, Tour, Event, variable,
#           sgtot, sgt2g, sgtee, sg_bs, sg_sg ,sgatg, sgapp, sgp, cum_sum_round)


par_fil_long <- par_fil_long %>% group_by(player) %>% arrange(Date, .by_group = TRUE) %>% 
  drop_na(par3) %>% mutate(rounds_play = 1, cum_sum_round = cumsum(rounds_play))# %>% select(player, Date, year, Event, variable,
#            par3, par4, par5, DD, DA, Gir,Ppgir ,Ss ,cum_sum_round)

GainedData <- function(data_long, dates_tour = dates_tour){
  
  data_long <- data_long %>% mutate(tournament_count = cut(Date, breaks = dates_tour, labels = seq_along(dates_tour[-1])))
  
  # how many tournaments has been played
  length_list <- length(dates_tour)
  
  # fill out the NA values of the last tournament with the lenght of the tournaments
  data_long$tournament_count <- ifelse(is.na(data_long$tournament_count), length_list, data_long$tournament_count)
  
  return(data_long)
}

par_Gained <- GainedData(par_fil_long, dates_tour)
sg_Gained <- GainedData(sg_fil_long, dates_tour)


if (!is.na(Round)) {
  sg_Gained = sg_Gained[sg_Gained$variable == Round, ]
  par_Gained = par_Gained[par_Gained$variable == Round, ]
}

## Stroke gained data

sg_Gained <- sg_Gained %>% distinct(player, year, Event, Date, variable ,.keep_all = TRUE)
par_Gained <- par_Gained %>% distinct(player, year, Event, Date, variable ,.keep_all = TRUE)


year_tour <- Reduce(rbind, split(sg_Gained, ~tournament_count), accumulate = TRUE)

sg_list <- vector(mode = "list", length = length(year_tour))

for (i in 1:length(year_tour)) {
  sg_list[[i]] <- year_tour[[i]] %>% group_by(player) %>% summarise(last_posn = last(Posn),
                                                                    round_total = last(cum_sum_round),
                                                                    last_event = last(Event),
                                                                    year = last(year),
                                                                    
                                                                    sgtot4 = mean(tail(sgtot, 4) ,  na.rm=TRUE),
                                                                    sgtot8 = mean(tail(sgtot, 8) ,  na.rm=TRUE),
                                                                    sgtot12 = mean(tail(sgtot, 12) ,  na.rm=TRUE),
                                                                    sgtot24 = mean(tail(sgtot, 24) ,  na.rm=TRUE),
                                                                    sgtot36 = mean(tail(sgtot, 36) ,  na.rm=TRUE),
                                                                    sgtot50 = mean(tail(sgtot, 50) ,  na.rm=TRUE),
                                                                    sgtot75 = mean(tail(sgtot, 75) ,  na.rm=TRUE),
                                                                    sgtot100 = mean(tail(sgtot, 100) ,  na.rm=TRUE),
                                                                    sgtot_all = mean(sgtot, na.rm = TRUE),
                                                                    
                                                                    sgt2g4 = mean(tail(sgt2g, 4) ,  na.rm=TRUE),
                                                                    sgt2g8 = mean(tail(sgt2g, 8) ,  na.rm=TRUE),
                                                                    sgt2g12 = mean(tail(sgt2g, 12) ,  na.rm=TRUE),
                                                                    sgt2g24 = mean(tail(sgt2g, 24) ,  na.rm=TRUE),
                                                                    sgt2g36 = mean(tail(sgt2g, 36) ,  na.rm=TRUE),
                                                                    sgt2g50 = mean(tail(sgt2g, 50) ,  na.rm=TRUE),
                                                                    sgt2g75 = mean(tail(sgt2g, 75) ,  na.rm=TRUE),
                                                                    sgt2g100 = mean(tail(sgt2g, 100) ,  na.rm=TRUE),
                                                                    sgt2g_all = mean(sgt2g, na.rm = TRUE),
                                                                    
                                                                    sgtee4 = mean(tail(sgtee, 4) ,  na.rm=TRUE),
                                                                    sgtee8 = mean(tail(sgtee, 8) ,  na.rm=TRUE),
                                                                    sgtee12 = mean(tail(sgtee, 12) ,  na.rm=TRUE),
                                                                    sgtee24 = mean(tail(sgtee, 24) ,  na.rm=TRUE),
                                                                    sgtee36 = mean(tail(sgtee, 36) ,  na.rm=TRUE),
                                                                    sgtee50 = mean(tail(sgtee, 50) ,  na.rm=TRUE),
                                                                    sgtee75 = mean(tail(sgtee, 75) ,  na.rm=TRUE),
                                                                    sgtee100 = mean(tail(sgtee, 100) ,  na.rm=TRUE),
                                                                    sgtee_all = mean(sgtee, na.rm = TRUE),
                                                                    
                                                                    sg_bs4 = mean(tail(sg_bs, 4) ,  na.rm=TRUE),
                                                                    sg_bs8 = mean(tail(sg_bs, 8) ,  na.rm=TRUE),
                                                                    sg_bs12 = mean(tail(sg_bs, 12) ,  na.rm=TRUE),
                                                                    sg_bs24 = mean(tail(sg_bs, 24) ,  na.rm=TRUE),
                                                                    sg_bs36 = mean(tail(sg_bs, 36) ,  na.rm=TRUE),
                                                                    sg_bs50 = mean(tail(sg_bs, 50) ,  na.rm=TRUE),
                                                                    sg_bs75 = mean(tail(sg_bs, 75) ,  na.rm=TRUE),
                                                                    sg_bs100 = mean(tail(sg_bs, 100) ,  na.rm=TRUE),
                                                                    sg_bs_all = mean(sg_bs, na.rm = TRUE),
                                                                    
                                                                    sg_sg4 = mean(tail(sg_sg, 4) ,  na.rm=TRUE),
                                                                    sg_sg8 = mean(tail(sg_sg, 8) ,  na.rm=TRUE),
                                                                    sg_sg12 = mean(tail(sg_sg, 12) ,  na.rm=TRUE),
                                                                    sg_sg24 = mean(tail(sg_sg, 24) ,  na.rm=TRUE),
                                                                    sg_sg36 = mean(tail(sg_sg, 36) ,  na.rm=TRUE),
                                                                    sg_sg50 = mean(tail(sg_sg, 50) ,  na.rm=TRUE),
                                                                    sg_sg75 = mean(tail(sg_sg, 75) ,  na.rm=TRUE),
                                                                    sg_sg100 = mean(tail(sg_sg, 100) ,  na.rm=TRUE),
                                                                    sg_sg_all = mean(sg_sg, na.rm = TRUE),
                                                                    
                                                                    sgatg4 = mean(tail(sgatg, 4) ,  na.rm=TRUE),
                                                                    sgatg8 = mean(tail(sgatg, 8) ,  na.rm=TRUE),
                                                                    sgatg12 = mean(tail(sgatg, 12) ,  na.rm=TRUE),
                                                                    sgatg24 = mean(tail(sgatg, 24) ,  na.rm=TRUE),
                                                                    sgatg36 = mean(tail(sgatg, 36) ,  na.rm=TRUE),
                                                                    sgatg50 = mean(tail(sgatg, 50) ,  na.rm=TRUE),
                                                                    sgatg75 = mean(tail(sgatg, 75) ,  na.rm=TRUE),
                                                                    sgatg100 = mean(tail(sgatg, 100) ,  na.rm=TRUE),
                                                                    sgatg_all = mean(sgatg, na.rm = TRUE),
                                                                    
                                                                    sgapp4 = mean(tail(sgapp, 4) ,  na.rm=TRUE),
                                                                    sgapp8 = mean(tail(sgapp, 8) ,  na.rm=TRUE),
                                                                    sgapp12 = mean(tail(sgapp, 12) ,  na.rm=TRUE),
                                                                    sgapp24 = mean(tail(sgapp, 24) ,  na.rm=TRUE),
                                                                    sgapp36 = mean(tail(sgapp, 36) ,  na.rm=TRUE),
                                                                    sgapp50 = mean(tail(sgapp, 50) ,  na.rm=TRUE),
                                                                    sgapp75 = mean(tail(sgapp, 75) ,  na.rm=TRUE),
                                                                    sgapp100 = mean(tail(sgapp, 100) ,  na.rm=TRUE),
                                                                    sgapp_all = mean(sgapp, na.rm = TRUE),
                                                                    
                                                                    sgp4 = mean(tail(sgp, 4) ,  na.rm=TRUE),
                                                                    sgp8 = mean(tail(sgp, 8) ,  na.rm=TRUE),
                                                                    sgp12 = mean(tail(sgp, 12) ,  na.rm=TRUE),
                                                                    sgp24 = mean(tail(sgp, 24) ,  na.rm=TRUE),
                                                                    sgp36 = mean(tail(sgp, 36) ,  na.rm=TRUE),
                                                                    sgp50 = mean(tail(sgp, 50) ,  na.rm=TRUE),
                                                                    sgp75 = mean(tail(sgp, 75) ,  na.rm=TRUE),
                                                                    sgp100 = mean(tail(sgp, 100) ,  na.rm=TRUE),
                                                                    sgp_all = mean(sgp, na.rm = TRUE))
  
}


## Par data
year_tour_par <- Reduce(rbind, split(par_Gained, ~tournament_count), accumulate = TRUE)

par_list <- vector(mode = "list", length = length(year_tour_par))

for (i in 1:length(year_tour_par)) {
  par_list[[i]] <- year_tour_par[[i]] %>% group_by(player) %>% summarise(round_total = last(cum_sum_round),
                                                                         last_event = last(Event),
                                                                         year = last(year),
                                                                         
                                                                         par34 = mean(tail(par3, 4), na.rm=TRUE),
                                                                         par38 = mean(tail(par3, 8), na.rm=TRUE),
                                                                         par312 = mean(tail(par3, 12), na.rm=TRUE),
                                                                         par324 = mean(tail(par3, 24), na.rm=TRUE),
                                                                         par336 = mean(tail(par3, 36), na.rm=TRUE),
                                                                         par350 = mean(tail(par3, 50), na.rm=TRUE),
                                                                         par375 = mean(tail(par3, 75), na.rm=TRUE),
                                                                         par3100 = mean(tail(par3, 100), na.rm=TRUE),
                                                                         par3_all = mean(par3, na.rm = TRUE),
                                                                         
                                                                         par44 = mean(tail(par4, 4), na.rm=TRUE),
                                                                         par48 = mean(tail(par4, 8), na.rm=TRUE),
                                                                         par412 = mean(tail(par4, 12), na.rm=TRUE),
                                                                         par424 = mean(tail(par4, 24), na.rm=TRUE),
                                                                         par436 = mean(tail(par4, 36), na.rm=TRUE),
                                                                         par450 = mean(tail(par4, 50), na.rm=TRUE),
                                                                         par475 = mean(tail(par4, 75), na.rm=TRUE),
                                                                         par4100 = mean(tail(par4, 100), na.rm=TRUE),
                                                                         par4_all = mean(par4, na.rm = TRUE),
                                                                         
                                                                         par54 = mean(tail(par5, 4), na.rm=TRUE),
                                                                         par58 = mean(tail(par5, 8), na.rm=TRUE),
                                                                         par512 = mean(tail(par5, 12), na.rm=TRUE),
                                                                         par524 = mean(tail(par5, 24), na.rm=TRUE),
                                                                         par536 = mean(tail(par5, 36), na.rm=TRUE),
                                                                         par550 = mean(tail(par5, 50), na.rm=TRUE),
                                                                         par575 = mean(tail(par5, 75), na.rm=TRUE),
                                                                         par5100 = mean(tail(par5, 100), na.rm=TRUE),
                                                                         par5_all = mean(par5, na.rm = TRUE),
                                                                         
                                                                         DD4 = mean(tail(DD, 4), na.rm=TRUE),
                                                                         DD8 = mean(tail(DD, 8), na.rm=TRUE),
                                                                         DD12 = mean(tail(DD, 12), na.rm=TRUE),
                                                                         DD24 = mean(tail(DD, 24), na.rm=TRUE),
                                                                         DD36 = mean(tail(DD, 36), na.rm=TRUE),
                                                                         DD50 = mean(tail(DD, 50), na.rm=TRUE),
                                                                         DD75 = mean(tail(DD, 75), na.rm=TRUE),
                                                                         DD100 = mean(tail(DD, 100), na.rm=TRUE),
                                                                         DD_all = mean(DD, na.rm = TRUE),
                                                                         
                                                                         DA4 = mean(tail(DA, 4), na.rm=TRUE),
                                                                         DA8 = mean(tail(DA, 8), na.rm=TRUE),
                                                                         DA12 = mean(tail(DA, 12), na.rm=TRUE),
                                                                         DA24 = mean(tail(DA, 24), na.rm=TRUE),
                                                                         DA36 = mean(tail(DA, 36), na.rm=TRUE),
                                                                         DA50 = mean(tail(DA, 50), na.rm=TRUE),
                                                                         DA75 = mean(tail(DA, 75), na.rm=TRUE),
                                                                         DA100 = mean(tail(DA, 100), na.rm=TRUE),
                                                                         DA_all = mean(DA, na.rm = TRUE),
                                                                         
                                                                         Gir4 = mean(tail(Gir, 4), na.rm=TRUE),
                                                                         Gir8 = mean(tail(Gir, 8), na.rm=TRUE),
                                                                         Gir12 = mean(tail(Gir, 12), na.rm=TRUE),
                                                                         Gir24 = mean(tail(Gir, 24), na.rm=TRUE),
                                                                         Gir36 = mean(tail(Gir, 36), na.rm=TRUE),
                                                                         Gir50 = mean(tail(Gir, 50), na.rm=TRUE),
                                                                         Gir75 = mean(tail(Gir, 75), na.rm=TRUE),
                                                                         Gir100 = mean(tail(Gir, 100), na.rm=TRUE),
                                                                         Gir_all = mean(Gir, na.rm=TRUE),
                                                                         
                                                                         Ppgir4 = mean(tail(Ppgir, 4), na.rm=TRUE),
                                                                         Ppgir8 = mean(tail(Ppgir, 8), na.rm=TRUE),
                                                                         Ppgir12 = mean(tail(Ppgir, 12), na.rm=TRUE),
                                                                         Ppgir24 = mean(tail(Ppgir, 24), na.rm=TRUE),
                                                                         Ppgir36 = mean(tail(Ppgir, 36), na.rm=TRUE),
                                                                         Ppgir50 = mean(tail(Ppgir, 50), na.rm=TRUE),
                                                                         Ppgir75 = mean(tail(Ppgir, 75), na.rm=TRUE),
                                                                         Ppgir100 = mean(tail(Ppgir, 100), na.rm=TRUE),
                                                                         Ppgir_all = mean(Ppgir, na.rm = TRUE),
                                                                         
                                                                         Ss4 = mean(tail(Ss, 4), na.rm=TRUE),
                                                                         Ss8 = mean(tail(Ss, 8), na.rm=TRUE),
                                                                         Ss12 = mean(tail(Ss, 12), na.rm=TRUE),
                                                                         Ss24 = mean(tail(Ss, 24), na.rm=TRUE),
                                                                         Ss36 = mean(tail(Ss, 36), na.rm=TRUE),
                                                                         Ss50 = mean(tail(Ss, 50), na.rm=TRUE),
                                                                         Ss75 = mean(tail(Ss, 75), na.rm=TRUE),
                                                                         Ss100 = mean(tail(Ss, 100), na.rm=TRUE),
                                                                         Ss_all = mean(Ss, na.rm = TRUE))
}

######

par_list_tm <- par_list %>% bind_rows %>% select(player, year, round_total, last_event, par34:Ss_all)

dup_value <- duplicated(file_par[,1:5])
du_va <- file_par[dup_value,]

#par_list_tm_2 <- par_list_tm %>% distinct(player, year, round_total, last_event, .keep_all = TRUE)
par_list_tm_2 <- par_list_tm %>% distinct(player, year, .keep_all = TRUE)

file_par <- bip %>% left_join(par_list_tm_2, by = c("player", "year"))

file_par$Top20 <-  ifelse(is.na(file_par$Top20)==TRUE, file_par$Top25, file_par$Top20)
file_par$Top25 <- NULL

file_par <- file_par %>% select(player, year, Posn, round_total, Rd1:Rd4, Current:Location, par34:Ss_all)

#######
# get the stroke gained data

sg_list_tm <- sg_list %>% bind_rows %>% select(player, year, round_total, last_event, last_posn, sgtot4:sgp_all)

sg_list_tm_2 <- sg_list_tm %>% distinct(player, year, .keep_all = TRUE)

file_sg <- file_par %>% left_join(sg_list_tm_2, by = c("player", "year"))

file_sg <- file_sg %>% select(player:Location, sgtot4:sgp_all, par34:Ss_all)

####
final_file <- file_sg
final_file <- file_sg %>% filter(round_total.x >= 10)
final_file <- final_file[order(final_file$year, final_file$player),]
df_2021 <- subset(final_file, year == 2021)
df <- subset(final_file, year !=2021)
df <- df[!is.na(df$Posn),]
df <- df[!(df$Posn == "w"),]

df$Rd2[is.na(df$Rd2)] <- 80
df$Rd3[is.na(df$Rd3)] <- 80
df$Rd4[is.na(df$Rd4)] <- 80
df$score <- df$Rd1 + df$Rd2 + df$Rd3 + df$Rd4

df <- df %>% group_by(year) %>% mutate(Posn = rank(score, ties.method = "min"))

df <- df %>% select(player:Rd4, score, Current:Location, sgtot4:Ss_all )

df <- subset(df, year >= 2013)


# Export the data
library(openxlsx)
tour = sprintf("%s_%d.xlsx", current_Event, Round)
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Current")
writeData(wb, sheet = "Current", x= df_2021)
addWorksheet(wb, sheetName = "Historic")
writeData(wb, sheet = "Historic", x= df)
saveWorkbook(wb, file.path(bip_path, tour), overwrite = TRUE)
openXL(file.path(bip_path, tour))
