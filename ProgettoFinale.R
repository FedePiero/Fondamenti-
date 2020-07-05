library(scales)
library(ggplot2)
library(tidyverse)
library(ggfortify)
library(tidyr)
library(dplyr)
library(ggpubr)
library(plotly)

#importo i data
production=read_csv("/Users/federicopierobon/Desktop/fondamenti/global-meat-production.csv")
population=read_csv("/Users/federicopierobon/Desktop/fondamenti/population-of-all-world-regions-including-the-un-projection-until-2100.csv")
land=read_csv("/Users/federicopierobon/Desktop/fondamenti/land-use-over-the-long-term.csv")
meat=read_csv("/Users/federicopierobon/Desktop/fondamenti/daily-meat-consumption-per-person.csv")

#creo le strutture che mi servono in base ai 5 continenti
#data popolazione: 
#Continente europeo
popEuropa<-population %>%
  filter(Entity=="Europe") %>%
  rename(Pop="Estimates, 1950 - 2015: Total population by broad age group, both sexes combined (thousands) - Total ( )") %>%
  rename(PopStimata="Projection ( )") %>%
  select(Year,Pop,PopStimata)

popEuropa[is.na(popEuropa)]<-"" 
popEuropa$Pop[ which(popEuropa$Year == 2015) ]<-"" 
popEuropa<-unite(popEuropa,PopolazioneEuropa, c(Pop,PopStimata), sep="")
popEuropa$PopolazioneEuropa<-as.numeric(popEuropa$PopolazioneEuropa)  

#Continente americano
#siccome è diviso da nord e sud, unisco le tabelle con un inner_join e successivamente 
#sommo le due popolazioni creando così la popolazione totale del continente 
popAmericaS<-population %>%
  filter(Entity=="South America") %>%
  rename(Pop="Estimates, 1950 - 2015: Total population by broad age group, both sexes combined (thousands) - Total ( )") %>%
  rename(PopStimata="Projection ( )") %>%
  select(Year,Pop, PopStimata)

popAmericaS[is.na(popAmericaS)]<-""
popAmericaS$Pop[ which(popAmericaS$Year == 2015) ]<-""
popAmericaS<-unite(popAmericaS,Popolazione1, c(Pop,PopStimata), sep="")
popAmericaS$Popolazione1<-as.numeric(popAmericaS$Popolazione1)  

popAmericaN<-population %>%
  filter(Entity=="Northern America") %>%
  rename(Pop="Estimates, 1950 - 2015: Total population by broad age group, both sexes combined (thousands) - Total ( )") %>%
  rename(PopStimata="Projection ( )") %>%
  select(Year,Pop, PopStimata) 

popAmericaN[is.na(popAmericaN)]<-"" 
#ho aggiunto la modifica di questo dato perchè il numero è presente sia in pop che in popstimata
popAmericaN$Pop[ which(popAmericaN$Year == 2015) ]<-"" 
popAmericaN<-unite(popAmericaN,Popolazione2, c(Pop,PopStimata), sep="")
popAmericaN$Popolazione2<-as.numeric(popAmericaN$Popolazione2)  

popAmerica<-popAmericaN %>%
  inner_join(popAmericaS,by="Year")%>%
  mutate(PopolazioneAmerica=Popolazione1+Popolazione2)
  popAmerica$Popolazione1<-NULL
  popAmerica$Popolazione2<-NULL

#Continente asiatico
popAsia<-population %>%
  filter(Entity=="Asia") %>%
  rename(Pop="Estimates, 1950 - 2015: Total population by broad age group, both sexes combined (thousands) - Total ( )") %>%
  rename(PopStimata="Projection ( )") %>%
  select(Year,Pop,PopStimata)

popAsia[is.na(popAsia)]<-"" 
popAsia$Pop[ which(popAsia$Year == 2015) ]<-"" 
popAsia<-unite(popAsia,PopolazioneAsia, c(Pop,PopStimata), sep="")
popAsia$PopolazioneAsia<-as.numeric(popAsia$PopolazioneAsia)  

#Continente oceania
popOceania<-population %>%
  filter(Entity=="Oceania") %>%
  rename(Pop="Estimates, 1950 - 2015: Total population by broad age group, both sexes combined (thousands) - Total ( )") %>%
  rename(PopStimata="Projection ( )") %>%
  select(Year,Pop,PopStimata)

popOceania[is.na(popOceania)]<-"" 
popOceania$Pop[ which(popOceania$Year == 2015) ]<-"" 
popOceania<-unite(popOceania,PopolazioneOceania, c(Pop,PopStimata), sep="")
popOceania$PopolazioneOceania<-as.numeric(popOceania$PopolazioneOceania)  

#Continente africano
popAfrica<-population %>%
  filter(Entity=="Africa") %>%
  rename(Pop="Estimates, 1950 - 2015: Total population by broad age group, both sexes combined (thousands) - Total ( )") %>%
  rename(PopStimata="Projection ( )") %>%
  select(Year,Pop,PopStimata)

popAfrica[is.na(popAfrica)]<-"" 
popAfrica$Pop[ which(popAfrica$Year == 2015) ]<-"" 
popAfrica<-unite(popAfrica,PopolazioneAfrica, c(Pop,PopStimata), sep="")
popAfrica$PopolazioneAfrica<-as.numeric(popAfrica$PopolazioneAfrica)  

#unisco tutte le popolazioni dei continenti su un'unica tabella 
popContinenti<-popEuropa%>%
  inner_join(popAmerica,by="Year")%>%
  inner_join(popAsia,by="Year")%>%
  inner_join(popOceania,by="Year")%>%
  inner_join(popAfrica,by="Year")

#rappresentazione grafica per continente
#popolazione:
plotPopCont <- plot_ly(x = popContinenti$Year, y = popContinenti$PopolazioneEuropa, name = 'Europa', type = 'scatter', mode = 'lines',line = list(color="#008000")) %>% 
  add_trace(y = popContinenti$PopolazioneAsia, name = 'Asia', mode = 'lines',line = list(color="#cecf00")) %>%
  add_trace(y = popContinenti$PopolazioneAmerica, name = 'America', mode = 'lines',line = list(color="#FF0000")) %>%
  add_trace(y = popContinenti$PopolazioneAfrica, name = 'Africa', mode = 'lines',line = list(color="#000000")) %>%
  add_trace(y = popContinenti$PopolazioneOceania, name = 'Oceania', mode = 'lines',line = list(color="#0000FF")) %>%
  layout(title="Popolazione")

#data produzione:
#Continente europeo
proEuropa<-production %>%
  filter(Entity=="Europe") %>%
  rename(ProduzioneEuropa="Livestock Primary - Meat, Total - 1765 - Production - 5510 - tonnes (tonnes)") %>%
  select(Year,ProduzioneEuropa)
#Continente americano
proAmerica<-production %>%
  filter(Entity=="Americas") %>%
  rename(ProduzioneAmerica="Livestock Primary - Meat, Total - 1765 - Production - 5510 - tonnes (tonnes)") %>%
  select(Year,ProduzioneAmerica)
#Continente asiatico
proAsia<-production %>%
  filter(Entity=="Asia") %>%
  rename(ProduzioneAsia="Livestock Primary - Meat, Total - 1765 - Production - 5510 - tonnes (tonnes)") %>%
  select(Year,ProduzioneAsia)
#Continente oceania
proOceania<-production %>%
  filter(Entity=="Oceania") %>%
  rename(ProduzioneOceania="Livestock Primary - Meat, Total - 1765 - Production - 5510 - tonnes (tonnes)") %>%
  select(Year,ProduzioneOceania)
#Continente africano
proAfrica<-production %>%
  filter(Entity=="Africa") %>%
  rename(ProduzioneAfrica="Livestock Primary - Meat, Total - 1765 - Production - 5510 - tonnes (tonnes)") %>%
  select(Year,ProduzioneAfrica)

#unisco tutte le produzioni di carne dei continenti su un'unica tabella
proContinenti<-proEuropa%>%
  inner_join(proAmerica,by="Year")%>%
  inner_join(proAsia,by="Year")%>%
  inner_join(proOceania,by="Year")%>%
  inner_join(proAfrica,by="Year")

#rappresentazione grafica per continente
#produzione carne:
plotProCont <- plot_ly(x = proContinenti$Year, y = proContinenti$ProduzioneEuropa, name = 'Europa', type = 'scatter', mode = 'lines',line = list(color="#008000"),showlegend = FALSE) %>% 
  add_trace(y = proContinenti$ProduzioneAsia, name = 'Asia', mode = 'lines',line = list(color="#cecf00"),showlegend = FALSE) %>%
  add_trace(y = proContinenti$ProduzioneAmerica, name = 'America', mode = 'lines',line = list(color="#FF0000"),showlegend = FALSE) %>%
  add_trace(y = proContinenti$ProduzioneAfrica, name = 'Africa', mode = 'lines',line = list(color="#000000"),showlegend = FALSE) %>%
  add_trace(y = proContinenti$ProduzioneOceania, name = 'Oceania', mode = 'lines',line = list(color="#0000FF"),showlegend = FALSE) %>%
  layout(title="Produzione carne")

#consumo carne
#Europa
meatEuropa<-meat %>%
  filter(Entity=="Europe") %>%
  rename(grammiEuropa="Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (g per person per day)") %>%
  select(Year,grammiEuropa)
meatEuropa<-aggregate(meatEuropa$grammiEuropa, by=list(Year=meatEuropa$Year), FUN=sum)
meatEuropa<-meatEuropa%>%
  rename(grammiEuropa=x)
#Africa
meatAfrica<-meat %>%
  filter(Entity=="Africa") %>%
  rename(grammiAfrica="Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (g per person per day)") %>%
  select(Year,grammiAfrica)
meatAfrica<-aggregate(meatAfrica$grammiAfrica, by=list(Year=meatAfrica$Year), FUN=sum)
meatAfrica<-meatAfrica%>%
  rename(grammiAfrica=x)

#Oceania
meatOceania<-meat %>%
  filter(Entity=="Oceania") %>%
  rename(grammiOceania="Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (g per person per day)") %>%
  select(Year,grammiOceania)
meatOceania<-aggregate(meatOceania$grammiOceania, by=list(Year=meatOceania$Year), FUN=sum)
meatOceania<-meatOceania%>%
  rename(grammiOceania=x)

#Asia
meatAsia<-meat %>%
  filter(Entity=="Asia") %>%
  rename(grammiAsia="Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (g per person per day)") %>%
  select(Year,grammiAsia)
meatAsia<-aggregate(meatAsia$grammiAsia, by=list(Year=meatAsia$Year), FUN=sum)
meatAsia<-meatAsia%>%
  rename(grammiAsia=x)

#America
meatAmerica<-meat %>%
  filter(Entity=="Americas") %>%
  rename(grammiAmerica="Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (g per person per day)") %>%
  select(Year,grammiAmerica)
meatAmerica<-aggregate(meatAmerica$grammiAmerica, by=list(Year=meatAmerica$Year), FUN=sum)
meatAmerica<-meatAmerica%>%
  rename(grammiAmerica=x)


#unisco tutte le consumazioni di carne dei continenti su un'unica tabella
meatContinenti<-meatEuropa%>%
  inner_join(meatAmerica,by="Year")%>%
  inner_join(meatAsia,by="Year")%>%
  inner_join(meatOceania,by="Year")%>%
  inner_join(meatAfrica,by="Year")

#rappresentazione grafica per continente
#meat

plotMeatCont <- plot_ly(x = meatContinenti$Year, y = meatContinenti$grammiEuropa, name = 'Europa', type = 'scatter', mode = 'lines',line = list(color="#008000"),showlegend = FALSE) %>% 
  add_trace(y = meatContinenti$grammiAsia, name = 'Asia', mode = 'lines',line = list(color="#cecf00"),showlegend = FALSE) %>%
  add_trace(y = meatContinenti$grammiAmerica, name = 'America', mode = 'lines',line = list(color="#FF0000"),showlegend = FALSE) %>%
  add_trace(y = meatContinenti$grammiAfrica, name = 'Africa', mode = 'lines',line = list(color="#000000"),showlegend = FALSE) %>%
  add_trace(y = meatContinenti$grammiOceania, name = 'Oceania', mode = 'lines',line = list(color="#0000FF"),showlegend = FALSE) %>%
  layout(title="consumo carne in grammi/per giorno/per persona")

#area nella terra
#mondo
  landMondo<-land%>%
    filter(Entity=="World")%>%
    rename(Costruzione="Built-up Area (hectares)")%>%
    rename(Allevamento="Grazing (hectares)")%>%
    rename(Agricoltura="Cropland (hectares)")
    landMondo$Code<-NULL
    landMondo$Entity<-NULL

#terra:
    
    View(landMondo)
landMondo$Year[ which(landMondo$Year == '2000 BCE') ]<-NA
landMondo$Year[ which(landMondo$Year == '3000 BCE') ]<-NA
landMondo$Year[ which(landMondo$Year == '4000 BCE') ]<-NA
landMondo$Year[ which(landMondo$Year == '5000 BCE') ]<-NA
landMondo$Year[ which(landMondo$Year == '6000 BCE') ]<-NA 
landMondo$Year[ which(landMondo$Year == '7000 BCE') ]<-NA
landMondo$Year[ which(landMondo$Year == '8000 BCE') ]<-NA 
landMondo$Year[ which(landMondo$Year == '9000 BCE') ]<-NA 
landMondo$Year[ which(landMondo$Year == '200') ]<-NA
landMondo$Year[ which(landMondo$Year == '300') ]<-NA 
landMondo$Year[ which(landMondo$Year == '400') ]<-NA 
landMondo$Year[ which(landMondo$Year == '500') ]<-NA 
landMondo$Year[ which(landMondo$Year == '600') ]<-NA 
landMondo$Year[ which(landMondo$Year == '700') ]<-NA 
landMondo$Year[ which(landMondo$Year == '800') ]<-NA 
landMondo$Year[ which(landMondo$Year == '900') ]<-NA 
landMondo<-subset(landMondo,landMondo$Year>=1960)
landMondo$Year<-as.numeric(landMondo$Year) 
plotLand <- plot_ly(x = landMondo$Year, y = landMondo$Costruzione, name = 'aree Costruite', type = 'scatter', mode = 'lines',line = list(color="#000080")) %>% 
  add_trace(y = landMondo$Allevamento, name = 'Allevamento', mode = 'lines',line = list(color="#ff0000")) %>%
  add_trace(y = landMondo$Agricoltura, name = 'Agricoltura', mode = 'lines',line = list(color="#00ff00"))

#rappresentazione grafica totale

popTotale<-popContinenti %>%
  mutate(Popolazione=PopolazioneAmerica+PopolazioneEuropa+PopolazioneAsia+PopolazioneAfrica+PopolazioneOceania)
  popTotale$PopolazioneAmerica<-NULL
  popTotale$PopolazioneEuropa<-NULL
  popTotale$PopolazioneAsia<-NULL
  popTotale$PopolazioneOceania<-NULL
  popTotale$PopolazioneAfrica<-NULL

proTotale<-proContinenti %>%
  mutate(Produzione=ProduzioneAmerica+ProduzioneEuropa+ProduzioneAsia+ProduzioneAfrica+ProduzioneOceania)
  proTotale$ProduzioneAmerica<-NULL
  proTotale$ProduzioneEuropa<-NULL
  proTotale$ProduzioneAsia<-NULL
  proTotale$ProduzioneOceania<-NULL
  proTotale$ProduzioneAfrica<-NULL

plotPopTotale <- plot_ly(x = popTotale$Year, y = popTotale$Popolazione, name = 'Popolazione', type = 'scatter', mode = 'lines',line = list(color="#000000")) 

plotProTotale <- plot_ly(x = proTotale$Year, y = proTotale$Produzione, name = 'Produzione Carne', type = 'scatter', mode = 'lines',line = list(color="#FF0000"))

plotPopProTotale<-subplot(plotPopTotale,plotProTotale) %>%
  layout(xaxis = list(range = c(1961, 2018)),title="Popolazione / Produzione Carne")

plotPopProTerraTotale<-subplot(plotPopProTotale,plotLand)%>%
  layout(title="Popolazione / Produzione Carne / terra usata")


#fare una stima per il futuro, se oggi serve tot domani quanto servirà?

#previsioni 
#parto con la tabella popolazione, dove le previsioni arrivano fino al 2100
#provo una correlazione lineare tra Aumento popolazione e produzione carne


correlazione<-popTotale %>%
  left_join(proTotale,by="Year")

plotcorrelazione <- plot_ly(x=correlazione$Popolazione, y=correlazione$Produzione)

model <- lm( Produzione~ Popolazione, data =correlazione)

summary(model)

popProTotalePrevisione<-correlazione

for (i in 2019:2100) {
  popProTotalePrevisione$Produzione[ which(popProTotalePrevisione$Year == i) ]<-predict(model,data.frame(Popolazione=(popProTotalePrevisione%>% filter(Year==i) %>% select(Popolazione))))
}

#previsione popolazione e aumento delle terre usate

correlazione<-popTotale %>%
  left_join(landMondo,by="Year")

plotcorrelazioneCostruzione <- plot_ly(x=correlazione$Popolazione, y=correlazione$Costruzione)
plotcorrelazioneAllevamento <- plot_ly(x=correlazione$Popolazione, y=correlazione$Allevamento)
plotcorrelazioneAgricoltura <- plot_ly(x=correlazione$Popolazione, y=correlazione$Agricoltura)

modelCos <- lm( Costruzione~ Popolazione, data =correlazione)
modelAll <- lm( Allevamento~ Popolazione, data =correlazione)
modelAgr <- lm( Agricoltura~ Popolazione, data =correlazione)

summary(modelCos)

popLandTotalePrevisione<-correlazione

for (i in 2017:2100) {
  popLandTotalePrevisione$Costruzione[ which(popLandTotalePrevisione$Year == i) ]<-predict(modelCos,data.frame(Popolazione=(popLandTotalePrevisione%>% filter(Year==i) %>% select(Popolazione))))
}
for (i in 2017:2100) {
  popLandTotalePrevisione$Allevamento[ which(popLandTotalePrevisione$Year == i) ]<-predict(modelAll,data.frame(Popolazione=(popLandTotalePrevisione%>% filter(Year==i) %>% select(Popolazione))))
}
for (i in 2017:2100) {
  popLandTotalePrevisione$Agricoltura[ which(popLandTotalePrevisione$Year == i) ]<-predict(modelAgr,data.frame(Popolazione=(popLandTotalePrevisione%>% filter(Year==i) %>% select(Popolazione))))
}

#produzione carne previsione grafico

plotProTotalePrevisione <- plot_ly(x=popProTotalePrevisione$Year, y=popProTotalePrevisione$Produzione)

plotpopProTotalePrevisione <- subplot(plotProTotalePrevisione,plotPopTotale)%>%
  layout(xaxis = list(range = c(1961, 2100)),title="Produzione Carne / Popolazione")

plotpopProTotalePrevisione

#land previsione grafico

plotLandPrevisione <- plot_ly(x = popLandTotalePrevisione$Year, y = popLandTotalePrevisione$Costruzione, name = 'aree Costruite', type = 'scatter', mode = 'lines',line = list(color="#000080")) %>% 
  add_trace(y = popLandTotalePrevisione$Allevamento, name = 'Allevamento', mode = 'lines',line = list(color="#ff0000")) %>%
  add_trace(y = popLandTotalePrevisione$Agricoltura, name = 'Agricoltura', mode = 'lines',line = list(color="#00ff00"))
  
plotLandPrevisione

#grafico per terra usata adesso

AllNowPie<-popLandTotalePrevisione$Allevamento[which(popLandTotalePrevisione$Year == '2020')]
AgrNowPie<-popLandTotalePrevisione$Agricoltura[which(popLandTotalePrevisione$Year == '2020')]
CosNowPie<-popLandTotalePrevisione$Costruzione[which(popLandTotalePrevisione$Year == '2020')]
TerraRimastaNow<-(104000000*100)-(AllNowPie+AgrNowPie+CosNowPie)

labels1 = c("Allevamento","Agricoltura","Costruzione","Terra rimasta")
values1 = c(AllNowPie, AgrNowPie, CosNowPie,TerraRimastaNow)

#grafico per terra usata in futuro

AllPrePie<-popLandTotalePrevisione$Allevamento[which(popLandTotalePrevisione$Year == '2100')]
AgrPrePie<-popLandTotalePrevisione$Agricoltura[which(popLandTotalePrevisione$Year == '2100')]
CosPrePie<-popLandTotalePrevisione$Costruzione[which(popLandTotalePrevisione$Year == '2100')]
TerraRimastaPre<-(104000000*100)-(AllPrePie+AgrPrePie+CosPrePie)

labels2 = c("Allevamento","Agricoltura","Costruzione","Terra rimasta")
values2 = c(AllPrePie, AgrPrePie, CosPrePie,TerraRimastaPre)

#grafici torta terra messi insieme

pieLandNowPre<-plot_ly()
pieLandNowPre<-pieLandNowPre %>%
  add_pie( labels = labels1, values = values1, name = "2020", domain = list(row = 0, column = 0))
pieLandNowPre<-pieLandNowPre %>%
  add_pie( labels = labels2, values = values2, name = "2100", domain = list(row = 0, column = 1))
pieLandNowPre<-pieLandNowPre %>% 
  layout( showlegend = T, grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
