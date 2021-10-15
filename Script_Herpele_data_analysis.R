################################################################################
#     EFFECT OF SUBSTRATE CHANGE ON FEEDING IN HERPELE - ZDZ163
################################################################################

library(dplyr)
library(ggplot2)
library(lme4)
library(AICcmodavg)
library(MuMIn)



###___ DATA IMPORT _____________________________________________________________

df <- read.csv("Herpele_substrate_diet_data.csv", sep=";")
lapply(df, class)
df <- df %>% mutate(Food_type = as.factor(Food_type),
                    Date = as.Date(Date, format = "%d/%m/%Y"),
                    Date_last_change = as.Date(Date_last_change, format = "%d/%m/%Y"),
                    Time_bt_meas = as.integer(Time_bt_meas),
                    Tsoil_max = as.numeric(Tsoil_max),
                    Tsoil_min = as.numeric(Tsoil_min),
                    Trial_nb = case_when(Date_last_change == "2019-11-26" ~ 1,
                                         Date_last_change == "2020-04-21" ~ 2,
                                         Date_last_change == "2021-02-16" ~ 3,
                                         TRUE ~ 4),
                    Tsoil_range = as.numeric (Tsoil_max-Tsoil_min)) %>%
  rowwise() %>% mutate(Tsoil_med = median(c(Tsoil_min, Tsoil_max)))



###___ DATA EXPLORATION ________________________________________________________

#__Data summaries__
summary(df)
df %>% filter (Food_type=="Shrimp")%>% summary()
df %>% filter (Food_type=="Crickets")%>% summary()
difftime("2021-09-27", "2020-02-07", units="days")
difftime("2020-04-21", "2019-11-26", units="days")
difftime("2021-02-16", "2020-04-21", units="days")
difftime("2021-09-21", "2021-02-16", units="days")

#__Scatterplot: linear regression by time since change and 95%CIs__
ggplot(df,aes(x=Days_since_change,y=Prop_eaten))+
  geom_point() + 
  geom_smooth(method = glm, level=0.95, color = "black") +
  theme_classic() +
  labs(x = "Time since last substrate change (days)", y = "Proportion of food items eaten")

#__Scatterplot: linear regression by median soil temperature and 95%CIs__
ggplot(df,aes(x=Tsoil_med,y=Prop_eaten))+
  geom_point() + 
  geom_smooth(method = glm, level=0.95, color = "black") +
  theme_classic() +
  labs(x = "Median soil temperature (°C)", y = "Proportion of food items eaten")



###___ FULL MODEL CREATION _____________________________________________________

#__Creation of response variable for proportion analysis__
df$y <- cbind(df$Tot_eaten, df$Tot_retrieved)
head(df$y)

#__Creation of full model__
m_full <- glmer(y ~ Days_since_change + Food_type + Tsoil_med + Tsoil_range + (1|Trial_nb), family=binomial, data=df) 

#__Overdispersion estimate from Bolker__
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(m_full)



###___ MODEL SELECTION _________________________________________________________

#__Evaluation of the fixed effects through null-hypothesis testing using Wald-Z tests__
summary(m_full)

#__Final model__
m_final <- glmer(y ~ Days_since_change + Tsoil_med + (1|Trial_nb), family=binomial, data=df) 



###___ FINAL MODELS FIT, ASSUMPTIONS & ADEQUACY CHECKS _________________________

#__Overdispersion estimation__
overdisp_fun(m_final)

#__R² sensus Nakagawa & Schielzeth (2013), metric for assessing model fit__
r.squaredGLMM(m_final) 

#__Plot residuals vs. explanatory variables to check model's assumptions adequacy__
df_noNA <- na.omit(df)
ggplot(data.frame(x1=df_noNA$Days_since_change,pearson=residuals(m_final,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw() +
  labs(title = "Pearson residuals vs. Days since substrate change", y = "Pearson's residuals", x = "Time since substrate change (days)")
ggplot(data.frame(x1=df_noNA$Tsoil_med,pearson=residuals(m_final,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw() +
  labs(title = "Pearson residuals vs. median temperature", y = "Pearson's residuals", x = "Median temperature (°C)")

#__QQ-plot of the model's residuals to check its assumptions adequacy__
qqnorm(residuals(m_final)) 



###___ MODEL INTERPRETATION ____________________________________________________

#__Estimates and 95% CIs on the scale of the linear predictor__
summary(m_final)
exp(confint(m_final))

#__Odds-ratios__
exp(0.004)
exp(-0.11)
