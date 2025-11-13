#Szakdolgozat. Lord-féle paradoxon - Kelemen Flóra Anna

rm(list=ls())

#Adatok behívása
library(haven)
data <- read_dta("C:/Users/.../szakdolg.dta")
View(data)
summary(data)

#Roma network változó (dif_roma_netw) szétválasztása két részre aszerint, 
#hogy nőtt vagy csökkent/változatlan maradt a network a két időpont között
library(dplyr)
data_filt_incr <- filter(data, dif_roma_netw > 0)
data_filt_decr <- filter(data, dif_roma_netw <= 0)

#Felhasznált változók és jelentésük

#roma_netw: roma network 2016-ban 
#dif_roma_netw: roma network változása 
#R_PN1_CLMATES: objektív társas norma 2016-ban 
#dif_R_PN1_CLMATES: objektív társas norma változása 
#mean_pred1: észlelt társas norma 2016-ban 
#dif_mean_pred: észlelt társas norma változása 
#W1_roma_prej: romaellenesség 2016-ban 
#GENDER: nem 
#R_PN1_TEACH: észlelt tanári norma 2016-ban 
#dif_R_PN1_TEACH: észlelt tanári norma változása 
#dif_roma_prej: romellenesség változása 
#W2_roma_prej: romaellenesség 2017-ben 

#Gain score modell

#network növekedés
gain_model1 <- lm(dif_roma_prej ~ roma_netw + dif_roma_netw +
                   R_PN1_CLMATES + dif_R_PN1_CLMATES + mean_pred1 + dif_mean_pred + 
                   GENDER + R_PN1_TEACH + dif_R_PN1_TEACH, data = data_filt_incr)
summary(gain_model1)

#Standardizált (β) kiszámolása
library("lm.beta")  
lm.beta(gain_model1)  

#network decrease/unchanged
gain_model2 <- lm(dif_roma_prej ~ roma_netw + dif_roma_netw +
                    R_PN1_CLMATES + dif_R_PN1_CLMATES + mean_pred1 + dif_mean_pred + 
                    GENDER + R_PN1_TEACH + dif_R_PN1_TEACH, data = data_filt_decr)
summary(gain_model2)

#Standardizált (β) kiszámolása
lm.beta(gain_model2) 

#Autoregresszív modell

#network increase
autoreg_model1 <- lm(W2_roma_prej ~ roma_netw + dif_roma_netw +
                      R_PN1_CLMATES + dif_R_PN1_CLMATES + mean_pred1 + dif_mean_pred + 
                      W1_roma_prej + GENDER + R_PN1_TEACH + dif_R_PN1_TEACH, data = data_filt_incr)
summary(autoreg_model1)

#Standardizált (β) kiszámolása
lm.beta(autoreg_model1) 

#network változatlan/csökkenés
autoreg_model2 <- lm(W2_roma_prej ~ roma_netw + dif_roma_netw +
                       R_PN1_CLMATES + dif_R_PN1_CLMATES + mean_pred1 + dif_mean_pred + 
                       W1_roma_prej + GENDER + R_PN1_TEACH + dif_R_PN1_TEACH, data = data_filt_decr)
summary(autoreg_model2)

#Standardizált (β) kiszámolása
lm.beta(autoreg_model2)

#Feltételek ellenőrzése
##Linearitás
##Normalitás Q-Q plot
##Homoszkedaszticitás
##Outlierek és befolyásos pontok

#Gain score modell

#network növekedés
par(mfrow = c(2, 2))
plot(gain_model1)

#Multikollinearitás ellenőrzése (VIF)
library(car)
vif_gain1 <- car::vif(gain_model1)
vif_gain1

#network változatlan/csökkenés
par(mfrow = c(2, 2))
plot(gain_model2)

#Multikollinearitás ellenőrzése (VIF)
vif_gain2 <- car::vif(gain_model2)
vif_gain2

#Autoregresszív modell

#network növekedés
plot(autoreg_model1) #homoszkedaszticitás sérülni látszik --> robosztus modell

#Multikollinearitás ellenőrzése (VIF)
vif_autoreg1 <- car::vif(autoreg_model1)
vif_autoreg1

#network változatlan/csökkenés
plot(autoreg_model2) #homoszkedaszticitás sérülni látszik --> robosztus modell

#Multikollinearitás ellenőrzése (VIF)
vif_autoreg2 <- car::vif(autoreg_model2)
vif_autoreg2

#Robosztus autoregresszív modellek

#network növekedés
library("sandwich")
library("lmtest")
coeftest(autoreg_model1, vcovHC = sandwich)
coeftest(autoreg_model2, vcovHC = sandwich)
