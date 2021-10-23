ParGainedData <- function(sg, bip, current_tour, par){
  
  players_df <- data.frame(bip$player) # get the players name that played this tournament before
  colnames(players_df) <- "player"
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
  
  # get data for only player that played this tournament before
  sg_filter_merge <- players_df %>% inner_join(sg_filter, by = c("player"))
  
  # ignore player that do withdraw from the tournament
  sg_filter_merge <- sg_filter_merge[!(sg_filter_merge$Posn == "w"),]
  
  # subtract four days to get the information before the tournament given that this date is the day when the tournament ends
  dates_tour <- c(min(sg_filter$Date), unique(current_tour$Date) - days(4))
  
  par_filter <- par %>% select(player, year, Event, Date,
                               AvgP3PerPlayerR1, AvgP3PerPlayerR2, AvgP3PerPlayerR3, AvgP3PerPlayerR4,
                               AvgP4PerPlayerR1, AvgP4PerPlayerR2, AvgP4PerPlayerR3, AvgP4PerPlayerR4,
                               AvgP5PerPlayerR1, AvgP5PerPlayerR2, AvgP5PerPlayerR3, AvgP5PerPlayerR4,
                               AvgDDPerPlayerR1, AvgDDPerPlayerR2, AvgDDPerPlayerR3, AvgDDPerPlayerR4,
                               AvgDAPerPlayerR1, AvgDAPerPlayerR2, AvgDAPerPlayerR3, AvgDAPerPlayerR4,
                               AvgGirPerPlayerR1, AvgGirPerPlayerR2, AvgGirPerPlayerR3, AvgGirPerPlayerR4,
                               AvgPpGirPerPlayerR1, AvgPpGirPerPlayerR2, AvgPpGirPerPlayerR3, AvgPpGirPerPlayerR4,
                               AvgSsPlayerR1, AvgSsPlayerR2, AvgSsPlayerR3, AvgSsPlayerR4)
  
  # get data for only player that played this tournament before
  par_filter_merge <- players_df %>% inner_join(par_filter, by = c("player"))
  
  # subtract four days to get the information before the tournament given that this date is the day when the tournament ends
  
  par_fil <- par_filter_merge %>% group_by(player, Date) %>% summarise(across(c(year, Event, AvgP3PerPlayerR1:AvgSsPlayerR4), ~last(.,)))
  
  par_fil <- par_fil %>% group_by(player) %>% arrange(Date, .by_group = TRUE)
  
  par_fil_long_1 <- melt(par_fil, measure.vars = c("AvgP3PerPlayerR1", "AvgP3PerPlayerR2", "AvgP3PerPlayerR3", "AvgP3PerPlayerR4"))
  par_fil_long_2 <- melt(par_fil, measure.vars = c("AvgP4PerPlayerR1", "AvgP4PerPlayerR2", "AvgP4PerPlayerR3", "AvgP4PerPlayerR4"))
  par_fil_long_3 <- melt(par_fil, measure.vars = c("AvgP5PerPlayerR1", "AvgP5PerPlayerR2", "AvgP5PerPlayerR3", "AvgP5PerPlayerR4"))
  par_fil_long_4 <- melt(par_fil, measure.vars = c("AvgDDPerPlayerR1", "AvgDDPerPlayerR2", "AvgDDPerPlayerR3", "AvgDDPerPlayerR4"))
  par_fil_long_5 <- melt(par_fil, measure.vars = c("AvgDAPerPlayerR1", "AvgDAPerPlayerR2", "AvgDAPerPlayerR3", "AvgDAPerPlayerR4"))
  par_fil_long_6 <- melt(par_fil, measure.vars = c("AvgGirPerPlayerR1", "AvgGirPerPlayerR2", "AvgGirPerPlayerR3", "AvgGirPerPlayerR4"))
  par_fil_long_7 <- melt(par_fil, measure.vars = c("AvgPpGirPerPlayerR1", "AvgPpGirPerPlayerR2", "AvgPpGirPerPlayerR3", "AvgPpGirPerPlayerR4"))
  par_fil_long_8 <- melt(par_fil, measure.vars = c("AvgSsPlayerR1", "AvgSsPlayerR2", "AvgSsPlayerR3", "AvgSsPlayerR4"))
  
  par_fil_long_1$par4 <- par_fil_long_2$value
  par_fil_long_1$par5 <- par_fil_long_3$value
  par_fil_long_1$DD <- par_fil_long_4$value
  par_fil_long_1$DA <- par_fil_long_5$value
  par_fil_long_1$Gir <- par_fil_long_6$value
  par_fil_long_1$Ppgir <- par_fil_long_7$value
  par_fil_long_1$Ss <- par_fil_long_8$value
  
  
  par_fil_long <- par_fil_long_1 %>% group_by(player) %>% arrange(Date, .by_group = TRUE) %>% 
    drop_na(value) %>% mutate(rounds_play = 1, cum_sum_round = cumsum(rounds_play)) %>% select(player, Date, year, Event, variable,
                                                                                               value, par4, par5, DD, DA, Gir,Ppgir ,Ss ,cum_sum_round)
  
  
  # create cuts in the data based on the tournaments that has been played before
  par_fil_long <- par_fil_long %>% mutate(tournament_count = cut(Date, breaks = dates_tour, labels = seq_along(dates_tour[-1])))
  
  # how many tournaments has been played
  length_list <- length(dates_tour)
  
  # fill out the NA values of the last tournament with the lenght of the tournaments
  par_fil_long$tournament_count <- ifelse(is.na(par_fil_long$tournament_count), length_list, par_fil_long$tournament_count)
  
  par_fil_long <- dplyr::rename(par_fil_long, "par3" = "value")
  
  year_tour <- Reduce(rbind, split(par_fil_long, ~tournament_count), accumulate = TRUE)
  
  par_list <- vector(mode = "list", length = length_list)
  
  for (i in 1:length(dates_tour)) {
    par_list[[i]] <- year_tour[[i]] %>% group_by(player) %>% summarise(round_total = last(cum_sum_round),
                                                                       last_event = last(Event),
                                                                       year = last(year),
                                                                       
                                                                       par34 = tryCatch(last(rollapply(par3, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par38 = tryCatch(last(rollapply(par3, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par312 = tryCatch(last(rollapply(par3, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par324 = tryCatch(last(rollapply(par3, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par336 = tryCatch(last(rollapply(par3, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par350 = tryCatch(last(rollapply(par3, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par375 = tryCatch(last(rollapply(par3, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par3100 = tryCatch(last(rollapply(par3, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par3_all = tryCatch(last(mean(par3, na.rm = TRUE)), error=function(e) NA),
                                                                       
                                                                       par44 = tryCatch(last(rollapply(par4, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par48 = tryCatch(last(rollapply(par4, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par412 = tryCatch(last(rollapply(par4, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par424 = tryCatch(last(rollapply(par4, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par436 = tryCatch(last(rollapply(par4, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par450 = tryCatch(last(rollapply(par4, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par475 = tryCatch(last(rollapply(par4, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par4100 = tryCatch(last(rollapply(par4, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par4_all = tryCatch(last(mean(par4, na.rm = TRUE)), error=function(e) NA),
                                                                       
                                                                       par54 = tryCatch(last(rollapply(par5, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par58 = tryCatch(last(rollapply(par5, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par512 = tryCatch(last(rollapply(par5, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par524 = tryCatch(last(rollapply(par5, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par536 = tryCatch(last(rollapply(par5, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par550 = tryCatch(last(rollapply(par5, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par575 = tryCatch(last(rollapply(par5, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par5100 = tryCatch(last(rollapply(par5, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       par5_all = tryCatch(last(mean(par5, na.rm = TRUE)), error=function(e) NA),
                                                                       
                                                                       DD4 = tryCatch(last(rollapply(DD, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DD8 = tryCatch(last(rollapply(DD, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DD12 = tryCatch(last(rollapply(DD, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DD24 = tryCatch(last(rollapply(DD, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DD36 = tryCatch(last(rollapply(DD, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DD50 = tryCatch(last(rollapply(DD, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DD75 = tryCatch(last(rollapply(DD, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DD100 = tryCatch(last(rollapply(DD, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DD_all = tryCatch(last(mean(DD, na.rm = TRUE)), error=function(e) NA),
                                                                       
                                                                       DA4 = tryCatch(last(rollapply(DA, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DA8 = tryCatch(last(rollapply(DA, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DA12 = tryCatch(last(rollapply(DA, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DA24 = tryCatch(last(rollapply(DA, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DA36 = tryCatch(last(rollapply(DA, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DA50 = tryCatch(last(rollapply(DA, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DA75 = tryCatch(last(rollapply(DA, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DA100 = tryCatch(last(rollapply(DA, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       DA_all = tryCatch(last(mean(DA, na.rm = TRUE)), error=function(e) NA),
                                                                       
                                                                       Gir4 = tryCatch(last(rollapply(Gir, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Gir8 = tryCatch(last(rollapply(Gir, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Gir12 = tryCatch(last(rollapply(Gir, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Gir24 = tryCatch(last(rollapply(Gir, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Gir36 = tryCatch(last(rollapply(Gir, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Gir50 = tryCatch(last(rollapply(Gir, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Gir75 = tryCatch(last(rollapply(Gir, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Gir100 = tryCatch(last(rollapply(Gir, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Gir_all = tryCatch(last(mean(Gir, na.rm=TRUE)), error=function(e) NA),
                                                                       
                                                                       Ppgir4 = tryCatch(last(rollapply(Ppgir, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ppgir8 = tryCatch(last(rollapply(Ppgir, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ppgir12 = tryCatch(last(rollapply(Ppgir, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ppgir24 = tryCatch(last(rollapply(Ppgir, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ppgir36 = tryCatch(last(rollapply(Ppgir, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ppgir50 = tryCatch(last(rollapply(Ppgir, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ppgir75 = tryCatch(last(rollapply(Ppgir, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ppgir100 = tryCatch(last(rollapply(Ppgir, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ppgir_all = tryCatch(last(mean(Ppgir, na.rm = TRUE)), error=function(e) NA),
                                                                       
                                                                       Ss4 = tryCatch(last(rollapply(Ss, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ss8 = tryCatch(last(rollapply(Ss, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ss12 = tryCatch(last(rollapply(Ss, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ss24 = tryCatch(last(rollapply(Ss, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ss36 = tryCatch(last(rollapply(Ss, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ss50 = tryCatch(last(rollapply(Ss, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ss75 = tryCatch(last(rollapply(Ss, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ss100 = tryCatch(last(rollapply(Ss, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                       Ss_all = tryCatch(last(mean(Ss, na.rm = TRUE)), error=function(e) NA))
  }
  
  par_list_tm <- par_list %>% bind_rows %>% select(player, year, round_total, last_event, par34:Ss_all)
  
  par_list_tm_2 <- par_list_tm %>% distinct(player, year, round_total, last_event, .keep_all = TRUE)
  
  file_par <- bip %>% inner_join(par_list_tm_2, by = c("player", "year"))
  
  file_par$Top20 <-  ifelse(is.na(file_par$Top20)==TRUE, file_par$Top25, file_par$Top20)
  file_par$Top25 <- NULL
  
  file_par <- file_par %>% select(player, year, round_total, Current:Location, par34:Ss_all)
  
}
