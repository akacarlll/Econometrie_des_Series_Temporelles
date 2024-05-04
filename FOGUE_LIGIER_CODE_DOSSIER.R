library(tidyr)
library(tidyverse)
library(TSstudio)
library(lubridate)
library(dygraphs)
library(forecast)
library(MTS)
library(lattice)
library(astsa)
library(readxl)
library(urca)
library(psych)
library(rpart)
library(rpart.plot)
library(vars)
library(dynlm)

#Supprime les valeurs contenues dans l'environnement
rm(list="gdp_growth_sec_test")

#Importe la dataset au nom de df
df <- read.csv("C:\\Users\\carlf\\OneDrive\\Bureau\\DOSSIER_MR_TRABELSI\\WorldDataDossier.csv")

#Donne le nom des colonnes
colnames(df)


# Supprime les lignes où X. est NA
df <- subset(df, !is.na(df$X.))

#Remplace les noms des colonnes de la 1ère à la 29ème
names(df)[1:29] <- c("Year", "Country", "GDP","GDP_growth","GDPpercap","DeathRate_per1000","FertilityRate",
                     "Life_expect","pct_pop_014","pop_014","pct_pop1564","pop_male_1564","pop65above", "pctpop65above",
                     "net_migration", "HIV_prev", "Inflation","External_balance","pct_urbanpop", "urbanpop_growth","pctdomesticsaving",
                     "pctsaving", "displacedpeople","unemployed_advanced", "unemployed_basic", "unemployed_inter", "pctexpense",
                     "Interest", "taxonexports")



#Retire les virgules des variables, change le type de celles-ci en numérique

df$GDP <- gsub(",", "", df$GDP)
df$GDP <- as.numeric(df$GDP)
df$External_balance <- gsub(",", "", df$External_balance)
df$External_balance <- as.numeric(df$External_balance)
df$Life_expect <- as.numeric(df$Life_expect)
df$Inflation <- as.numeric(df$Inflation)
df$net_migration <- gsub(",", "", df$net_migration)
df$net_migration <- as.numeric(df$net_migration)
df$FertilityRate <- as.numeric(df$FertilityRate)

#Compte les NA
nb_na <- sum(is.na(df$FertilityRate))

#Modification de la dataset 
df$logGDP <- log(df$GDP)

#Sous dataset par pays
df_fr <- df[df$Country == "France", ]
df_sen <- df[df$Country == "SENEGAL", ]
df_chin <- df[df$Country == "CHINA", ]

#Modification de la dataset du Sénégal
df_sen[50, "Life_expect"] <- 67



#Statistiques descriptives 
summary(df_sen)

#Graphe de deux variables 
#On peut afficher plusieurs graphiques en même temps
par(mfrow = c(2,2), col = "aquamarine")
plot(df_sen$Year, df_sen$GDP_growth, main = "GDP_growth du sénégal",
     ylab = "GDPgrowth",
     xlab = "year")
plot(df_fr$Year, df_fr$GDP_growth, main = "GDP_growth de la France",
     ylab = "GDPgrowth",
     xlab = "year")
plot(df_chin$Year, df_chin$GDP_growth, main = "GDP_growth de la Chine",
     ylab = "GDPgrowth",
     xlab = "year")

# Graphe de n variables simultanément 
sen_mts_3 <- sen_mts[, c("GDP_growth", "Life_expect", "Inflation")]
noms_variables <- c("PIB", "Espérance de vie", "Inflation")
y_min <- min(sen_mts_3, na.rm = TRUE)
y_max <- max(sen_mts_3, na.rm = TRUE)

plot.ts(
  sen_mts_3,
  main = "PIB GALSEN",
  xlab = "Année",
  ylab = "Valeur",
  col = c("blue", "green", "red"),
  lwd = 2,
  ylim = c(y_min, y_max)
)

#Visualisation interactive avec dygraph
dygraph(
  sen_mts_3,
  main = "PIB GALSEN",
  xlab = "Année",
  ylab = "Valeur")



names(df_sen)

#Création de séries temporelles univariées
depart <- c(year(min(df_sen$Year)))
frequence <- 1

gdp_sen_ts <- ts(
  data = df_sen$logGDP,
  start = depart,
  frequency = frequence
)

gdp_growth_sen_ts <- ts(
  data = df_sen$GDP_growth,
  start = depart,
  frequency = frequence
)

life_expect_sen_ts <- ts(
  data = df_sen$Life_expect,
  start = depart,
  frequency = frequence
)

inflation_sen_ts <- ts(
  data = df_sen$Inflation,
  start = depart,
  frequency = frequence
)
class(gdp_sen_ts)

#Création d'une série temporelle multivariée avec toutes les variables

sen_mts <-df_sen %>%
  arrange(Year) %>%
  dplyr::select(-Year)


sen_mts <- df_sen %>%
  dplyr::select(-Year) %>%
  as.matrix() %>%
  ts(start = 1973, frequency = 1)

class(sen_mts)
ts_info(sen_mts)

#Fonction d'autocorrélation : pour déterminer l’ordre d’intégration, on utilise un corrélogramme
acf_gdp <- acf(gdp_sen_ts, lag.max = 50)
acf_gdp_growth <- acf(gdp_growth_sen_ts, lag.max = 50)
acf_gdp <- acf(diff(gdp_sen_ts, lag.max = 50))
acf_gdp_growth <- acf(diff(gdp_growth_sen_ts, lag.max = 50))
acf_gdp
acf_gdp_growth
#Fonction d'autocorrélation partielle :  on souhaite connaître l'ordre d'intégration
pacf_gdp <- acf2(gdp_sen_ts, lag.max = 50)
pacf_growth <- acf2(gdp_growth_sen_ts)
pacf_gdp
pacf_growth



##tests de racine unitaire de DF urca
summary(ur.df(gdp_sen_ts, type="trend",selectlags="AIC"))
summary(ur.df(gdp_growth_sen_ts, type="trend",selectlags="AIC"))


##tests de racine unitaire de kpss
summary(ur.kpss(gdp_growth_sen_ts, type="mu"))
summary(ur.kpss(diff(gdp_growth_sen_ts), type="mu"))

summary(ur.kpss(gdp_sen_ts, type="mu"))
summary(ur.kpss(diff(gdp_sen_ts), type="mu"))

###test de cointegration
dp=gdp_sen_ts-life_expect_sen_ts
modele2=lm(gdp_growth_sen_ts ~ life_expect_sen_ts + inflation_sen_ts, data=df_sen)
summary(modele2)

res1=residuals(modele2)
summary(ur.df(res1, type="trend",selectlags="AIC"))

acf(res1)

modele3=lm(gdp_sen_ts ~ dp-1 + life_expect_sen_ts + inflation_sen_ts, data=df_sen)
summary(modele3)
res=residuals(modele3)
plot(res)
summary(ur.df(res, type="trend",selectlags="AIC"))

acf2(res)


#Après ces test et modification nécessaires on peut passer à la prédiction






#Partition des données
gdpgrowth_partitions <- ts_split(gdp_growth_sen_ts)
gdp_partitions <- ts_split(gdp_sen_ts)

#Partie GDP GROWTH
train <- gdpgrowth_partitions$train
test <- gdpgrowth_partitions$test
#Partie GDP
train2 <- gdp_partitions$train
test2 <- gdp_partitions$test

gdp_first_test <- auto.arima(train2)
gdp_growth_first_test <- auto.arima(diff(train))
summary(gdp_first_test)

md <- arima(train, order=c(10,1,3))
summary(md)
md2 <- arima(train2, order=c(10,1,1)) 
summary(md2)

#Analyse des résidus du modèle
#Permet de vérifier si les résidus des modèles comporte des tendances, correlations valeurs abberantes ou non-stationnarité
checkresiduals(md2)

#Affiche les résidus du modèle au cours du temps, permet de voir des sturctures temporelles
#Histogramme des résidus, plus les résisus ont une distribution normale, mieux c'est
#Graphique de densité des résidus, courbe rouge 
#ACF indique s'il reste des infos corrélés ds le résidus du modèle.

#Prédiction sur les données de test
fc <- forecast(gdp_first_test, h = length(test2))
fc
accuracy(fc,test)

test_forecast(
  actual = gdp_growth_sen_ts,
  forecast.obj = fc,
  test = test2
)



# Définir la taille de l'horizon de test
h <- 15

# Séparer les données en ensembles d'entraînement et de test
train_df <- head(df_sen, n = -h)

test_df <- tail(df_sen, n = h)


# Régression linéaire multiple appliquée aux séries temporelles
lr <- lm(logGDP ~ Life_expect + pctpop65above + Inflation, data = train_df)
summary(lr)

# Prédiction sur les données de test# Prédiction sur les donnéesInflation de test
test_df$lr_hat <- predict(lr, newdata = test_df)

# Prévisions avec le modèle de régression linéaire
forecast_lr <- predict(lr, newdata = test_df)

# Évaluation de la performance de la prévision
accuracy(forecast_lr, test_df$logGDP)

# Affichage des prévisions

plot(test_df$logGDP, type = "l", col = "blue", ylim = range(c(test_df$logGDP, forecast_lr)), xlab = "Time", ylab = "log(GDP)")
lines(forecast_lr, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 0.5)

#Test d'héteroscédasticité
bptest(lr)

#Test d'endogénéïté
dwtest(lr)

test_df2[15, "FertilityRate"] <- 4.4

# Définir la taille de l'horizon de test
h <- 15

# Ajouter une colonne pour les différences de logGDP
df_sen$diff_logGDP <- c(NA, diff(df_sen$logGDP))

# Ajouter une colonne pour les valeurs retardées de diff_logGDP
df_sen$lag_diff_logGDP <- lag(df_sen$diff_logGDP, 1)

# Séparer les données en ensembles d'entraînement et de test
train_df2 <- head(df_sen, n = -h)
test_df2 <- tail(df_sen, n = h)

# Ajuster le modèle ECM
ecm_model <- dynlm(logGDP ~ lag_diff_logGDP +pct_pop1564+FertilityRate+ net_migration + Life_expect + Inflation + External_balance, data = train_df2)

# Afficher un résumé du modèle
summary(ecm_model)

test_df2$lr_hat2 <- predict(ecm_model, newdata = test_df2)
forecast_lr2 <- predict(ecm_model, newdata = test_df2)

accuracy(forecast_lr2, test_df2$logGDP)


plot(test_df2$logGDP, type = "l", col = "blue", ylim = range(c(test_df2$logGDP, forecast_lr2)), xlab = "Time", ylab = "log(GDP)")
lines(forecast_lr2, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("red","blue"), lty = 1, cex = 0.8)

