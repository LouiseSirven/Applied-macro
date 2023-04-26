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
library(xts)
library(ggplot2)
library(dplyr)

##package de la banque mondiale
#install.packages("WDI", dependancies = True)
#library(WDI)

#Selection des pays
pays<-c("FR","IT","EU15","LV","EU27_2020","EA20","EA19","EU28")

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


###GDP : previous year prices (volume), in euro #selectionner un seul item par pays
gdp <- get_eurostat(id="namq_10_gdp", time_format="num", filters = list(unit="PYP_MEUR",s_adj=("NSA"),na_item="B1GQ"))
gdp
gdp[,5]
gdp_s<-gdp%>% filter(geo==pays) #Meme chose avec les bons pays selectionnes
ggplot(gdp_s,aes(x=time, y=values, color=geo, label=geo))+geom_point()+geom_line()


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
inf_s<-inf%>% filter(geo==pays) #Meme chose avec les bons pays selectionnes
ggplot(inf_s,aes(x=qdate, y=values, color=geo, label=geo))+geom_point()+geom_line()


###Unemployment rate (%of active population), A voir pour selectionner le bon age
un <- get_eurostat(id="une_rt_q_h", time_format="num", filters = list(unit="PC_ACT",s_adj=("NSA")))
un
un_s<-un%>% filter(geo==pays) #Meme chose avec les bons pays selectionnes
ggplot(un_s,aes(x=time, y=values, color=geo, label=geo))+geom_point()

###Oil price (Brent europe)

fredr_set_key("73a241b47839f9dd3a35764f8db93ece")
fredr(series_id = "DCOILBRENTEU", observation_start = as.Date("1990-01-01"), observation_end = as.Date("2020-01-01"))
oil_price = fredr(series_id = "DCOILBRENTEU", observation_start = as.Date("1990-01-01"), observation_end = as.Date("2020-01-01"))
oil_price
oil_price <- arrange(oil_price,date)
oilprice=drop_na(oil_price,value)#retrait des valeurs manquantes
oil=xts(oilprice$value, order.by = oilprice$date)
oil_q=apply.quarterly(oil, mean)#donn�es trimestrielles

dev.off()
par(mar=c(4,4,3,3))#ajustement des marges
plot(oil_q, ylab="Oil price", xlab='Date',xaxt="n")
axis(side=1,at=seq(1990,2019,4))


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


