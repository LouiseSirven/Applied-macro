#PROJET APPLIED MACROECONOMETRICS V28/04 10h30

#Notes : Ne pas oublier de citer les packages R dans le fichier final
#voir : https://ropengov.github.io/eurostat/articles/eurostat_tutorial.html
#(c'est le tuto pour utiliser le package eurostat)

#TO-DO :
#il faut standardiser les variables
#trouver le lag
#structurer  le var


#######################################################

#Set your directory here :
setwd("W:/Bureau/Applied macro/Code applied macro")

#Load data
#Packages
install.packages("eurostat", dependancies = True)
install.packages("fredr", dependancies = True)
install.packages("knitr", dependancies = True)
install.packages("dplyr", dependancies=True)
install.packages("zoo", dependancies=True)
install.packages("tidyverse", dependancies=True)
install.packages("panelr", dependancies=True)
install.packages("rlang", dependancies=True)
install.packages("tseries", dependancies=True)




library(eurostat)
library(fredr)
library(knitr)
library(dplyr)
library(zoo)
library(tidyverse)
library(panelr)
library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(readxl)


##package de la banque mondiale
#install.packages("WDI", dependancies = True)
#library(WDI)

#Selection des pays
pays<-c("FR","IT","EU15","LV","EU27_2020","EA20","EA19","EU28")

###visualisation des donnees
check_access_to_data
#pour verifier si tout va bien. Si ca chouine, c'est pas bon signe, sinon, all good

#premier appercu de la base de donnees
toc <- get_eurostat_toc()
kable(tail(toc))

#Differents moyens de chercher des variables avec des mots cles :
kable(head(search_eurostat(pattern="energy", type="table", fixed=FALSE)))

print (kable(head(search_eurostat(pattern="prc_hicp_mmor", type="table", fixed=FALSE))), n=20)

query <- search_eurostat(pattern="inflation", type= "table", fixed = FALSE)
query[,1:2]

test <- get_eurostat(id="sdg_07_50")
test


###GDP : previous year prices (volume), in euro#################################################################################
gdp <- get_eurostat(id="namq_10_gdp", time_format="num", filters = list(unit="PYP_MEUR",s_adj=("NSA"),na_item="B1GQ"))
gdp <- arrange(gdp,time)
gdp
gdp[,5]
gdp_s<-gdp %>% filter(geo=="EA19") #Meme chose avec les bons pays selectionnes
#bonnes dates
gdp_s<-gdp_s[-c(1:88), ]
gdp_s<-gdp_s[-c(93:104), ]
#la timeserie 
gdpts <- ts(gdp_s$values, start=c(1997,01), end=c(2019,04), frequency=4)
#affichage timeserie
plot(gdpts)
#affichage non time serie
ggplot(gdp_s,aes(x=time, y=values, color=geo, label=geo))+geom_point()+geom_line()

###INFLATION#################################################################################
inf <- get_eurostat(id="prc_hicp_mmor", time_format="num", filters = list(unit="RCH_M",coicop="CP00"))
inf
inf <- arrange(inf,time)
inf$time=paste(inf$time,"-01", sep="")
inf$time=as.Date(inf$time, format="%Y-%m-%d")
inf$qdate=as.yearqtr(inf$time)

i =subset(inf, select = c(geo, qdate, values))

inf_q <- i %>% group_by(geo, qdate) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()
inf_q

inf=inf_q
inf_s<-inf%>% filter(geo=="EA19") #Meme chose avec les bons pays selectionnes
#bonnes dates
inf_s<-inf_s[-c(1:4), ]
inf_s<-inf_s[-c(97:109), ]
#la timeserie 
infts <- ts(inf_s$values, start=c(1997,01), end=c(2019,04), frequency=4)
plot(infts) #affichage timeserie
#Affichage non time serie
ggplot(inf_s,aes(x=qdate, y=values, color=geo, label=geo))+geom_point()+geom_line()

########## CHOMAGE ############# (%of active population)
un <- get_eurostat(id="une_rt_q_h")
un <- un %>%filter(age == "Y15-74", sex == "T", s_adj=="NSA", unit=="PC_ACT", geo=="EA19")
#bonnes dates
un=arrange(un,time)
un=un[-c(93:96),]
#timeseries
tsun = ts(un$values, start=c(1997,01), end=c(2019,4), frequency=4)
plot(tsun)

################# SALAIRE ######################
w <- get_eurostat(id="namq_10_gdp", time_format="num", filters = list(unit="CP_MEUR",s_adj=("NSA"),na_item="D11", geo="EA19"))
w <- arrange(w,time)
print(w,n=150)
#Les bonnes variables
w_s<-w%>% filter(geo=="EA19") #Meme chose avec les bons pays selectionnes
#bonnes dates
w_s<-w_s[-c(1:88),]
w_s<-w_s[-c(93:104),]
#transformation en time series
wts=ts(w_s$values,start=c(1997,01), end=c(2019,04), frequency=4)
wts

################# EURODOLLAR ###################
eurodol <- get_eurostat(id="ERT_BIL_EUR_M", filters = list(currency="USD"))
print(eurodol, n=200)
eurodol <- arrange(eurodol,time)
eurodol$time=paste(eurodol$time,"-01", sep="")
eurodol$time=as.Date(eurodol$time,format="%Y-%m-%d")
eurodol$qdate=as.yearqtr(eurodol$time)
#Les bonnes variables
eurodol<-eurodol%>% filter(statinfo=="AVG")
i =subset(eurodol, select = c(qdate, values))

eurodol_q <- i %>% group_by(qdate) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()
eurodol_q

#bonnes dates
eurodol_q<-eurodol_q[-c(1:104), ]
eurodol_q<-eurodol_q[-c(93:105), ]
#la timeserie pour le prix du petrole
eurodolts <- ts(eurodol_q$values, start=c(1997,01), end=c(2019,4), frequency=4)
plot(eurodolts) #affichage timeserie



################# TAUX D'INTERET ###############
fredr_set_key("73a241b47839f9dd3a35764f8db93ece")
ti=fredr(series_id = "IR3TIB01EZQ156N", observation_start = as.Date("1990-01-01"), observation_end = as.Date("2020-01-01"))
ti <- arrange(ti,time) #ne marche pas
ti
#bonnes dates
ti<-ti[-c(1:12), ]
ti<-ti[-c(93), ]
tits <- ts(ti$value, start=c(1997,01), end=c(2019,4), frequency=4)
plot(tits) #affichage timeserie

############# Oil price (Brent europe) ###############

fredr_set_key("73a241b47839f9dd3a35764f8db93ece")
fredr(series_id = "DCOILBRENTEU", observation_start = as.Date("1990-01-01"), observation_end = as.Date("2020-01-01"))
oil_price = fredr(series_id = "DCOILBRENTEU", observation_start = as.Date("1990-01-01"), observation_end = as.Date("2020-01-01"))
oil_price
oil_price <- arrange(oil_price,date)
#bonnes dates
oil_price<-oil_price[-c(1:1827), ]
oil_price<-oil_price[-c(6001), ]
#retrait des valeurs manquantes
oilprice=drop_na(oil_price,value)
#Format trimestriel
oil=xts(oilprice$value, order.by = oilprice$date)
oil_q=apply.quarterly(oil, mean)#donnees trimestrielles
#la timeserie pour le prix du petrole
colnames(oil_q) <- c("valeur")
oilts <- ts(oil_q$valeur, start=c(1997,01), end=c(2019,4), frequency=4)
plot(oilts) #affichage timeserie
#affichage serie normale
dev.off()
par(mar=c(4,4,3,3))#ajustement des marges
plot(oil_q, ylab="Oil price", xlab='Date',xaxt="n")
axis(side=1,at=seq(1990,2019,4))

colnames(oil_q) <- c("time","value")

test = ts(oil_q)
class(test)

ggplot(test, aes(x=index, y=value))+geom_point()

oil_q = cbind(time = rownames(oil_q),oil_q)
rownames(oil_q) <- 1:nrow(oil_q)

test <- cbind(newColName = rownames(test), test)
rownames(test) <- 1:nrow(test)

#####################################################################
###################### Stationnarisation ############################
#####################################################################
###La serie du GDP doit etre differenciee 2 fois pour etre stationnaire -> differencier 2 fois toutes les series


oildec <- decompose(doil.ts)
doil.ts <- diff(oilts,differences = 2)
plot(oildec)
adf.test(doil.ts) # Dickey-Fuller pvalue=0.01 OK

gdpdec <- decompose(gdpts)
plot(gdpdec)
dgdp.ts <- diff(gdpts,differences = 2)
adf.test(dgdp.ts) # Dickey-Fuller pvalue=0.01 OK
plot(dgdp.ts)

infdec <- decompose(infts)
dinf.ts <- diff(infts,differences = 2)
plot(infdec)
adf.test(dinf.ts) # Dickey-Fuller pvalue=0.01 OK

un_dec <- decompose(tsun)
dun.ts <- diff(tsun,differences = 2)
plot(un_dec)
adf.test(dun.ts) # Dickey-Fuller pvalue=0.02 OK

wdec <- decompose(wts)
dw.ts <- diff(wts,differences = 2)
plot(wdec)
adf.test(dw.ts) # Dickey-Fuller pvalue=0.01 OK

eurodoldec <- decompose(eurodolts)
deurodol.ts <- diff(eurodolts,differences = 2)
plot(eurodoldec)
adf.test(deurodol.ts) # Dickey-Fuller pvalue=0.01 OK

tidec <- decompose(tits)
dti.ts <- diff(tits,differences = 2)
plot(tidec)
adf.test(dti.ts) # Dickey-Fuller pvalue=0.01 OK

#####################################################################
########################### Aggregation #############################
#####################################################################

un_model <- data.frame(Y=as.matrix(diff_un.ts), date=time(diff_un.ts))
ti_model <- data.frame(Y=as.matrix(dti.ts), date=time(dti.ts)) #ne marche pas
xr_model <- data.frame(Y=as.matrix(deurodol.ts), date=time(deurodol.ts))
w_model <- data.frame(Y=as.matrix(dw.ts), date=time(dw.ts))
inf_model <- data.frame(Y=as.matrix(dinf.ts), date=time(dinf.ts))
gdp_model <- data.frame(Y=as.matrix(dgdp.ts), date=time(dgdp.ts))
oil_model <- data.frame(Y=as.matrix(doil.ts), date=time(doil.ts))

model <- un_model %>% 
  inner_join(xr_model, by ="date") %>%
  inner_join(w_model, by ="date") %>%
  inner_join(inf_model, by ="date") %>%
  inner_join(gdp_model, by ="date") %>%
  inner_join(oil_model, by ="date")

#jointure des tables
sv <- cbind(time(dinf.ts), dun.ts,deurodol.ts, dw.ts, dinf.ts, dgdp.ts, doil.ts, dti.ts)
colnames(sv) <- c("date","unemployment", "ex_rate", "wage", "infla", "gdp", "oil", "taux_int")

#colnames(model) <- c("unemployment","date", "ex_rate", "wage", "infla", "gdp", "oil", "taux_int")

