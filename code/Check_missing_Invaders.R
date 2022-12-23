
# PARIS
PA_all <- data.frame(code = (1:PAmax)) 
PA_all$code <- paste0("PA_", sprintf("%04d",PA_all$code))
PA_missing <- PA_all %>% anti_join(invader.final, by = "code")
PA_missing$code <- stringr::str_trim(PA_missing$code)

# LONDRES
LDN_all <- data.frame(code = (1:LDNmax)) 
LDN_all$code <- paste0("LDN_", sprintf("%03d",LDN_all$code))
LDN_missing <- LDN_all %>% anti_join(invader.final, by = "code")
LDN_missing$code <- stringr::str_trim(LDN_missing$code)

#LDN <- invader.final %>% filter(str_detect(code, "LDN_") ==TRUE) %>% filter(status != "Got.it")


missing <- rbind(PA_missing, LDN_missing)
write.csv(missing , file = "../data/OUT_invader.to.add.csv", row.names=F, quote = F)

