#PROJET APPLIED MACROECONOMETRICS
#Notes : Ne pas oublier de citer les packages R dans le fichier final
#voir : https://ropengov.github.io/eurostat/articles/eurostat_tutorial.html
#(c'est le tuto pour utiliser le package eurostat)

#TO-DO :
#Chercher les données pour le prix du pétrole
#Mettre l'inflation en trimestriel
#Mettre les variables au propre pour pouvoir les utiliser après


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



library(eurostat)
library(fredr)
library(knitr)
library(dplyr)
library(zoo)
library(tidyverse)
library(panelr)



##package de la banque mondiale
#install.packages("WDI", dependancies = True)
#library(WDI)


###visualisation des données
check_access_to_data
#pour vérifier si tout va bien. Si ca chouine, c'est pas bon signe, sinon, all good

#premier apperçu de la base de données
toc <- get_eurostat_toc()
kable(tail(toc))

#Différents moyens de chercher des variables avec des mots clés :
kable(head(search_eurostat(pattern="energy", type="table", fixed=FALSE)))

print (kable(head(search_eurostat(pattern="prc_hicp_mmor", type="table", fixed=FALSE))), n=20)

query <- search_eurostat(pattern="inflation", type= "table", fixed = FALSE)
query[,1:2]

test <- get_eurostat(id="sdg_07_50")
test


###GDP : previous year prices (volume), in euro
gdp <- get_eurostat(id="namq_10_gdp", time_format="num", filters = list(unit="PYP_MEUR",s_adj=("NSA")))
gdp
gdp[,5]

###Inflation (MONTHLY -> changed in quarterly)
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


###Unemployment rate (%of active population)
un <- get_eurostat(id="une_rt_q_h", time_format="num", filters = list(unit="PC_ACT",s_adj=("NSA")))
un


###Oil price (Brent europe)-> journalier à mettre en quartely

fredr_set_key("73a241b47839f9dd3a35764f8db93ece")
fredr(series_id = "DCOILBRENTEU", observation_start = as.Date("1990-01-01"), observation_end = as.Date("2020-01-01"))
oil_price = fredr(series_id = "DCOILBRENTEU", observation_start = as.Date("1990-01-01"), observation_end = as.Date("2020-01-01"))
oil_price
oil_price <- arrange(oil_price,date)

oil_price$date=as.Date(oil_price$date, format="%Y-%m-%d")
oil_price$qdate=as.yearqtr(oil_price$date)

i =subset(oil_price, select = c(value, qdate))
qoil = i %>%
	group_by(qdate)%>%
	summarise_all(mean)
qoil

qoil <- i %>% 
	group_by(qdate) %>% 
	summarise(across(everything(), mean), .groups = 'drop')  %>%
	as.data.frame()
qoil


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





###Wage 
wa <- get_eurostat(id="tps00113", time_format="num")
wa



#################################################################################
#afficher premiere (ou autre) colonne
test=inf[,4]
test
unique(test)
print(unique(test),n=40)

#Exporter les données en excel :
library("writexl")
write_xlsx(the dataframe name,"path to store the Excel file\\file name.xlsx")

write_xlsx(inf,"D:/Bureau/inflation.xlsx")


inf_quarter <- inf %>%
group_by(qdate) %>%
print(inf_quarter,n=100)

test=subset(inf, select = c(geo, time, values))
test


