StrokeGainedData <- function(bip, sg, current_tour){
  
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
  
  sg_fil <- sg_filter_merge %>% group_by(player, Date) %>% summarise(across(c(year, Posn, Tour, Event, sgtotr1:sgpr4), ~last(.,)))
  
  sg_fil <- sg_fil %>% group_by(player) %>% arrange(Date, .by_group = TRUE)
  
  sg_fil_long_1 <- melt(sg_fil, measure.vars = c("sgtotr1", "sgtotr2", "sgtotr3", "sgtotr4"))
  sg_fil_long_2 <- melt(sg_fil, measure.vars = c("sgt2gr1", "sgt2gr2", "sgt2gr3", "sgt2gr4"))
  sg_fil_long_3 <- melt(sg_fil, measure.vars = c("sgteer1", "sgteer2", "sgteer3", "sgteer4"))
  sg_fil_long_4 <- melt(sg_fil, measure.vars = c("sgatgr1", "sgatgr2", "sgatgr3", "sgatgr4"))
  sg_fil_long_5 <- melt(sg_fil, measure.vars = c("sgappr1", "sgappr2", "sgappr3", "sgappr4"))
  sg_fil_long_6 <- melt(sg_fil, measure.vars = c("sgpr1", "sgpr2", "sgpr3", "sgpr4"))
  
  sg_fil_long_1$sgt2g <- sg_fil_long_2$value
  sg_fil_long_1$sgtee <- sg_fil_long_3$value
  sg_fil_long_1$sgatg <- sg_fil_long_4$value
  sg_fil_long_1$sgapp <- sg_fil_long_5$value
  sg_fil_long_1$sgp <- sg_fil_long_6$value
  
  sg_fil_long <- sg_fil_long_1 %>% group_by(player) %>% arrange(Date, .by_group = TRUE) %>% 
    drop_na(value) %>% mutate(rounds_play = 1,
                              cum_sum_round = cumsum(rounds_play),
                              sg_bs = sgtee + sgapp,
                              sg_sg = sgatg + sgp) %>% select(player, Date, year, Posn, Tour, Event, variable,
                                                              value, sgt2g, sgtee, sg_bs, sg_sg ,sgatg, sgapp, sgp, cum_sum_round)
  
  # create cuts in the data based on the tournaments that has been played before
  sg_fil_long <- sg_fil_long %>% mutate(tournament_count = cut(Date, breaks = dates_tour, labels = seq_along(dates_tour[-1])))
  
  # how many tournaments has been played
  length_list <- length(dates_tour)
  
  # fill out the NA values of the last tournament with the lenght of the tournaments
  sg_fil_long$tournament_count <- ifelse(is.na(sg_fil_long$tournament_count), length_list, sg_fil_long$tournament_count)
  
  sg_fil_long <- dplyr::rename(sg_fil_long, "sgtot" = "value")
  
  
}

StrokeGainedRounds <- function(sg_fil_long, bip){
  
  year_tour <- Reduce(rbind, split(sg_fil_long, ~tournament_count), accumulate = TRUE)
  
  sg_list <- vector(mode = "list", length = length(year_tour))
  
  for (i in 1:length(year_tour)) {
    sg_list[[i]] <- year_tour[[i]] %>% group_by(player) %>% summarise(last_posn = last(Posn),
                                                                      round_total = last(cum_sum_round),
                                                                      last_event = last(Event),
                                                                      year = last(year),
                                                                      
                                                                      sgtot4 = tryCatch(last(rollapply(sgtot, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtot8 = tryCatch(last(rollapply(sgtot, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtot12 = tryCatch(last(rollapply(sgtot, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtot24 = tryCatch(last(rollapply(sgtot, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtot36 = tryCatch(last(rollapply(sgtot, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtot50 = tryCatch(last(rollapply(sgtot, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtot75 = tryCatch(last(rollapply(sgtot, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtot100 = tryCatch(last(rollapply(sgtot, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtot_all = tryCatch(last(mean(sgtot, na.rm = TRUE)), error=function(e) NA),
                                                                      
                                                                      sgt2g4 = tryCatch(last(rollapply(sgt2g, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgt2g8 = tryCatch(last(rollapply(sgt2g, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgt2g12 = tryCatch(last(rollapply(sgt2g, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgt2g24 = tryCatch(last(rollapply(sgt2g, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgt2g36 = tryCatch(last(rollapply(sgt2g, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgt2g50 = tryCatch(last(rollapply(sgt2g, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgt2g75 = tryCatch(last(rollapply(sgt2g, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgt2g100 = tryCatch(last(rollapply(sgt2g, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgt2g_all = tryCatch(last(mean(sgt2g, na.rm = TRUE)), error=function(e) NA),
                                                                      
                                                                      sgtee4 = tryCatch(last(rollapply(sgtee, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtee8 = tryCatch(last(rollapply(sgtee, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtee12 = tryCatch(last(rollapply(sgtee, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtee24 = tryCatch(last(rollapply(sgtee, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtee36 = tryCatch(last(rollapply(sgtee, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtee50 = tryCatch(last(rollapply(sgtee, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtee75 = tryCatch(last(rollapply(sgtee, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtee100 = tryCatch(last(rollapply(sgtee, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgtee_all = tryCatch(last(mean(sgtee, na.rm = TRUE)), error=function(e) NA),
                                                                      
                                                                      sg_bs4 = tryCatch(last(rollapply(sg_bs, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_bs8 = tryCatch(last(rollapply(sg_bs, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_bs12 = tryCatch(last(rollapply(sg_bs, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_bs24 = tryCatch(last(rollapply(sg_bs, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_bs36 = tryCatch(last(rollapply(sg_bs, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_bs50 = tryCatch(last(rollapply(sg_bs, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_bs75 = tryCatch(last(rollapply(sg_bs, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_bs100 = tryCatch(last(rollapply(sg_bs, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_bs_all = tryCatch(last(mean(sg_bs, na.rm = TRUE)), error=function(e) NA),
                                                                      
                                                                      sg_sg4 = tryCatch(last(rollapply(sg_sg, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_sg8 = tryCatch(last(rollapply(sg_sg, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_sg12 = tryCatch(last(rollapply(sg_sg, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_sg24 = tryCatch(last(rollapply(sg_sg, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_sg36 = tryCatch(last(rollapply(sg_sg, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_sg50 = tryCatch(last(rollapply(sg_sg, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_sg75 = tryCatch(last(rollapply(sg_sg, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_sg100 = tryCatch(last(rollapply(sg_sg, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sg_sg_all = tryCatch(last(mean(sg_sg, na.rm = TRUE)), error=function(e) NA),
                                                                      
                                                                      sgatg4 = tryCatch(last(rollapply(sgatg, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgatg8 = tryCatch(last(rollapply(sgatg, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgatg12 = tryCatch(last(rollapply(sgatg, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgatg24 = tryCatch(last(rollapply(sgatg, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgatg36 = tryCatch(last(rollapply(sgatg, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgatg50 = tryCatch(last(rollapply(sgatg, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgatg75 = tryCatch(last(rollapply(sgatg, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgatg100 = tryCatch(last(rollapply(sgatg, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgatg_all = tryCatch(last(mean(sgatg, na.rm = TRUE)), error=function(e) NA),
                                                                      
                                                                      sgapp4 = tryCatch(last(rollapply(sgapp, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgapp8 = tryCatch(last(rollapply(sgapp, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgapp12 = tryCatch(last(rollapply(sgapp, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgapp24 = tryCatch(last(rollapply(sgapp, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgapp36 = tryCatch(last(rollapply(sgapp, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgapp50 = tryCatch(last(rollapply(sgapp, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgapp75 = tryCatch(last(rollapply(sgapp, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgapp100 = tryCatch(last(rollapply(sgapp, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgapp_all = tryCatch(last(mean(sgapp, na.rm = TRUE)), error=function(e) NA),
                                                                      
                                                                      sgp4 = tryCatch(last(rollapply(sgp, 4, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgp8 = tryCatch(last(rollapply(sgp, 8, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgp12 = tryCatch(last(rollapply(sgp, 12, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgp24 = tryCatch(last(rollapply(sgp, 24, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgp36 = tryCatch(last(rollapply(sgp, 36, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgp50 = tryCatch(last(rollapply(sgp, 50, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgp75 = tryCatch(last(rollapply(sgp, 75, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgp100 = tryCatch(last(rollapply(sgp, 100, mean, na.rm=TRUE)), error=function(e) NA),
                                                                      sgp_all = tryCatch(last(mean(sgp, na.rm = TRUE)), error=function(e) NA))
    
  }
  
  sg_list_tm <- sg_list %>% bind_rows %>% select(player, year, round_total, last_event, last_posn, sgtot4:sgp_all)
  
  sg_list_tm_2 <- sg_list_tm %>% distinct(player, year, round_total, last_event, .keep_all = TRUE)
  
  file_sg <- bip %>% inner_join(sg_list_tm_2, by = c("player", "year"))
  
  file_sg <- file_sg %>% select(player, year, Posn, sgtot4:sgp_all)
}



