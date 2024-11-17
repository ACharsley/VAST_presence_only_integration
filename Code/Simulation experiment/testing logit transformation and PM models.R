

# with logit transform
mod_df_logit <- data.frame("POE_OM" = logit(as.vector(POE_OM)), 
                     "POE_EM" = logit(as.vector(POE_EM)), 
                     "year" = rep(c(1978:2022), each=nrow(POE_OM)))
mod_logit1 <- lm(POE_OM ~ POE_EM, data = mod_df_logit[mod_df_logit$year == "2022",])
mod_logit1_pred <- predict(mod_logit1, data.frame("POE_EM" = logit(seq(0.05,0.95,by=0.05))))

plot(POE_OM ~ POE_EM, data = mod_df_logit[mod_df_logit$year == "2022",])
lines(x=logit(seq(0.05,0.95,by=0.05)), y=mod_logit1_pred)


mod_logit2 <- lm(POE_EM ~ POE_OM, data = mod_df_logit[mod_df_logit$year == "2022",])
mod_logit2_pred <- predict(mod_logit2, data.frame("POE_OM" = logit(seq(0.05,0.95,by=0.05))))

plot(POE_EM ~ POE_OM, data = mod_df_logit[mod_df_logit$year == "2022",])
lines(x=logit(seq(0.05,0.95,by=0.05)), y=mod_logit2_pred)



# without logit transform
mod_df <- data.frame("POE_OM" = as.vector(POE_OM), 
                     "POE_EM" = as.vector(POE_EM), 
                     "year" = rep(c(1978:2022), each=nrow(POE_OM)))
mod1 <- lm(POE_OM ~ POE_EM, data = mod_df[mod_df$year == "2022",])
mod1_pred <- predict(mod1, data.frame("POE_EM" = seq(0.05,0.95,by=0.05)))

plot(POE_OM ~ POE_EM, data = mod_df[mod_df$year == "2022",])
lines(x=seq(0.05,0.95,by=0.05), y=mod1_pred)


mod2 <- lm(POE_EM ~ POE_OM, data = mod_df[mod_df$year == "2022",])
mod2_pred <- predict(mod2, data.frame("POE_OM" = seq(0.05,0.95,by=0.05)))

plot(POE_EM ~ POE_OM, data = mod_df[mod_df$year == "2022",])
lines(x=seq(0.05,0.95,by=0.05), y=mod2_pred)
