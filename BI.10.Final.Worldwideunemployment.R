# title: "Worldwide Unemployment"
# indicator code: SL.UEM.TOTL.ZS

library(sf)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(data.table, warn.conflicts = FALSE, quietly = TRUE) 
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(dtplyr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(maps, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse)
library(tibble)
library(maps)
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(viridis)
library(viridisLite)
require(maps)
require(viridis)
theme_set(
  theme_void()
)
indicators <- read.csv("Indicators.csv")
# What can the World Development Indicator dataset tell us about unemployment?
# Where in the world are unemployment rates the highest?
# Correcting Country Names
correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")

for (c in names((correction))) {
  indicators[indicators$CountryName==c,"CountryName"] = correction[c]
}

ue2013<-indicators %>%
  filter(Year==2013
         & IndicatorCode =="SL.UEM.TOTL.ZS")

map.world <- merge(map_data(map="world"),
                   select(ue2013,CountryName,Value),
                   by.x='region',
                   by.y='CountryName',
                   all.x=TRUE,
                   fill=0)

map.world <- map.world[order(map.world$order),]

ggplot(map.world) +
  geom_map(map=map.world, aes(map_id=region, x=long, y=lat, fill=Value)) +
  borders("world",colour="black")+
  scale_fill_gradient(low ="yellow",high="red",guide="colourbar",name="Unemployment Rate (%)")+ labs(title="Unemployment Rates (2013)",x="Longitude",y="Latitude")+theme(legend.position="bottom")
# What clusters can a k-means analysis point out?
# * On the left, cluster 1 is 167 countries with relatively stable unemployment rates over 20+ year period.
# * On the right, cluster 2 is 39 countries with decreasing unemployment rates over 20+ year period.

ue <- indicators %>%
  filter(IndicatorCode == "SL.UEM.TOTL.ZS") %>%
  select(CountryName,Year,Value)  %>%
  spread(Year,Value,sep="")

country_name <- ue$CountryName
ue <- ue %>%
  select(-CountryName) %>%
  t() %>%
  scale(center=TRUE,scale=FALSE) %>%
  t()
ue <- data.frame(country_name,ue)

# Determine number of clusters
wss<-numeric()
for (i in 2:15) {
  km <-kmeans(select(ue,-country_name),centers=i)
  wss[i]<-sum(km$withinss)/sum(km$totss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
set.seed(1001)
fit <- kmeans(select(ue,-country_name), centers=2,iter.max=100,nstart=1)
ue <- data.frame(ue, fit$cluster)
ue <- ue %>%
  gather(Year,Value,-c(country_name,fit.cluster)) %>%
  group_by(fit.cluster) %>%
  mutate(cluster.count = n_distinct(country_name)) %>%
  ungroup() %>%
  mutate(Year = as.numeric(gsub("Year","",Year)),
         fit.cluster = paste("Cluster ",fit.cluster," (",cluster.count," Countries)",sep=""))
ggplot(ue,aes(Year,Value,colour=as.factor(fit.cluster)))+
  geom_point(shape=1)+geom_smooth()+
  scale_colour_discrete(guide=FALSE)+
  labs(title="Changes in World Unemployment Between 1991 and 2013",x="Year",y="Unemployment Rate (scaled)")+
  facet_wrap(~fit.cluster)

# Where are these clusters located?
map.world2 <- merge(map_data(map="world"),
                    select(ue,country_name,fit.cluster),
                    by.x='region',
                    by.y='country_name',
                    all.x=TRUE,
                    fill=0)
map.world2 <- map.world2[order(map.world2$order),]
ggplot(map.world2) +
  geom_map(map=map.world2, aes(map_id=region, x=long, y=lat, fill=fit.cluster)) +
  borders("world",colour="black")+
  scale_fill_discrete(name="")+
  labs(x="Longitude",y="Latitude")+ theme(legend.position="bottom")
# Where are unemployment rates increasing and decreasing?
ue <- indicators %>%
  filter(IndicatorCode == "SL.UEM.TOTL.ZS") %>%
  select(CountryName,Year,Value) %>%
  group_by(CountryName) %>%
  do(ue.rate=coef(lm(Value~Year,data=.))[[2]])

# Convert from list to numeric. 
ue$ue.rate<-as.numeric(unlist(ue$ue.rate))
map.world3 <- merge(map_data(map="world"),
                    ue, by.x='region',  by.y='CountryName',  all.x=TRUE,fill=0)
map.world3 <- map.world3[order(map.world3$order),]
ggplot(map.world3) +
  geom_map(map=map.world3, aes(map_id=region, x=long, y=lat, fill=ue.rate)) +
  borders("world",colour="black")+
  scale_fill_continuous(low="yellow",high="red",name="Annual World Unemployment Rate Change")+labs(x="Longitude",y="Latitude") +theme(legend.position="bottom")





rm(list=ls())
# title: "Africa Unemployment"
# indicator code: SL.UEM.TOTL.ZS
indicators <- read.csv("Indicators.csv")
# Where in Africa are the unemployment rates highest?
correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")

for (c in names((correction))) {
  indicators[indicators$CountryName==c,"CountryName"] = correction[c]
}
africa <- c("Algeria","Angola","Benin",
            "Botswana","Burkina Faso","Burundi","Ivory Coast","Cabo Verde","Cameroon",
            "Central African Republic","Chad","Comoros",
            "Democratic Republic of the Congo","Republic of Congo","Djibouti","Egypt",
            "Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia","Ghana","Guinea",
            "Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar","Malawi",
            "Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia",
            "Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles",
            "Sierra Leone","Somalia","South Africa","South Sudan","Sudan","Swaziland",
            "Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe")

ue2013<-indicators %>%
  filter(Year==2013
         & IndicatorCode =="SL.UEM.TOTL.ZS"
         & CountryName %in% africa)

map.africa <- merge(map_data(map="world", region = africa), select(ue2013,CountryName,Value),by.x='region', by.y='CountryName', all.x=TRUE, fill=0)
map.africa <- map.africa[order(map.africa$order),]
ggplot(map.africa) +
  geom_map(map=map.africa, aes(map_id=region, x=long, y=lat, fill=Value)) + borders("world",colour="black")+ scale_fill_gradient(low ="yellow",high="red",guide="colourbar",name="Unemployment Rate (%)")+ labs(title="Unemployment Rates - Africa (2013)",x="Longitude",y="Latitude")+ theme(legend.position="bottom") + coord_map("ortho", orientation = c(10, 15, 0))

# What clusters can a k-means analysis point out?
ue <- indicators %>%
  filter(IndicatorCode == "SL.UEM.TOTL.ZS"
         & CountryName %in% africa) %>%
  select(CountryName,Year,Value)  %>%
  spread(Year,Value,sep="")
country_name <- ue$CountryName
ue <- ue %>%
  select(-CountryName) %>%
  t() %>%
  scale(center=TRUE,scale=FALSE) %>%
  t()
ue <- data.frame(country_name,ue)

# Determine number of clusters
wss<-numeric()
for (i in 2:15) {
  km <-kmeans(select(ue,-country_name),centers=i)
  wss[i]<-sum(km$withinss)/sum(km$totss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

set.seed(1001)
fit <- kmeans(select(ue,-country_name), centers=5,iter.max=100,nstart=1)
ue <- data.frame(ue, fit$cluster)

ue <- ue %>%
  gather(Year,Value,-c(country_name,fit.cluster)) %>%
  group_by(fit.cluster) %>%
  mutate(cluster.count = n_distinct(country_name)) %>%
  ungroup() %>%
  mutate(Year = as.numeric(gsub("Year","",Year)),
         fit.cluster = paste("Cluster ",fit.cluster," (",cluster.count," Countries)",sep=""))

ggplot(ue,aes(Year,Value,colour=as.factor(fit.cluster)))+
  geom_point(shape=1)+geom_smooth()+
  scale_colour_discrete(guide=FALSE)+
  labs(title="Changes in World Unemployment Between 1991 and 2013 - Africa",x="Year",y="Unemployment Rate (scaled)")+
  facet_wrap(~fit.cluster)

# Where are these clusters located?
map.africa2 <- merge(map_data(map="world", region = africa),
                     select(ue,country_name,fit.cluster),
                     by.x='region',
                     by.y='country_name',
                     all.x=TRUE,
                     fill=0)
map.africa2 <- map.africa2[order(map.africa2$order),]
ggplot(map.africa2) +
  geom_map(map=map.africa2, aes(map_id=region, x=long, y=lat, fill=fit.cluster)) +
  borders("world",colour="black")+
  scale_fill_discrete(name="")+
  labs(x="Longitude",y="Latitude")+
  theme(legend.position="bottom") +   coord_map("ortho", orientation = c(10, 15, 0))
# Where in africa are unemployment rates increasing and decreasing?
ue <- indicators %>%
  filter(IndicatorCode == "SL.UEM.TOTL.ZS"
         & CountryName %in% africa) %>%
  select(CountryName,Year,Value) %>%
  group_by(CountryName) %>%
  do(ue.rate=coef(lm(Value~Year,data=.))[[2]])

# Convert from list to numeric. 
ue$ue.rate<-as.numeric(unlist(ue$ue.rate))
map.africa3 <- merge(map_data(map="world", region = africa),
                     ue,
                     by.x='region',
                     by.y='CountryName',
                     all.x=TRUE,
                     fill=0)
map.africa3 <- map.africa3[order(map.africa3$order),]

ggplot(map.africa3) +geom_map(map=map.africa3, aes(map_id=region, x=long, y=lat, fill=ue.rate))+borders("world",colour="black")+scale_fill_continuous(low="yellow",high="red",name="Annual Unemployment Rate Change - Africa")+  labs(x="Longitude",y="Latitude") +theme(legend.position="bottom")  +  coord_map("ortho", orientation = c(10, 15, 0))






rm(list=ls())
# title: "Africa food deficit"
# indicator code: SN.ITK.DFCT
indicators <- read.csv("Indicators.csv")

# What can the World Development Indicator dataset tell us about food deficit?
# Where in the world are food deficit rates the highest?
# Correcting Country Names
correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")

for (c in names((correction))) {
  indicators[indicators$CountryName==c,"CountryName"] = correction[c]
}
africa <- c("Algeria","Angola","Benin",
            "Botswana","Burkina Faso","Burundi","Ivory Coast","Cabo Verde","Cameroon",
            "Central African Republic","Chad","Comoros",
            "Democratic Republic of the Congo","Republic of Congo","Djibouti","Egypt",
            "Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia","Ghana","Guinea",
            "Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar","Malawi",
            "Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia",
            "Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles",
            "Sierra Leone","Somalia","South Africa","South Sudan","Sudan","Swaziland",
            "Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe")

fd2013<-indicators %>%
  filter(Year==2013
         & IndicatorCode =="SN.ITK.DFCT"
         & CountryName %in% africa)
map.africa <- merge(map_data(map="world", region = africa),
                    select(fd2013,CountryName,Value),
                    by.x='region',
                    by.y='CountryName',
                    all.x=TRUE,
                    fill=0)

map.africa <- map.africa[order(map.africa$order),]
ggplot(map.africa) +geom_map(map=map.africa, aes(map_id=region, x=long, y=lat, fill=Value)) + borders("world",colour="black")+scale_fill_gradient(low ="yellow",high="red",guide="colourbar",name="Food Deficit Rate (%)")+labs(title="Food Deficit Rates - Africa (2013)",x="Longitude",y="Latitude")+theme(legend.position="bottom") + coord_map("ortho", orientation = c(10, 15, 0))

# What clusters can a k-means analysis point out?

fd <- indicators %>%
  filter(IndicatorCode == "SN.ITK.DFCT"
         & CountryName %in% africa) %>%
  select(CountryName,Year,Value)  %>%
  spread(Year,Value,sep="")

country_name <- fd$CountryName

fd <- fd %>%
  select(-CountryName) %>%
  t() %>%
  scale(center=TRUE,scale=FALSE) %>%
  t()

fd <- data.frame(country_name,fd)

# imputation
for(i in 1:ncol(fd)) {
  fd[ , i][is.na(fd[ , i])] <- mean(fd[ , i], na.rm = TRUE)
}

# Determine number of clusters
wss<-numeric()
for (i in 2:15) {
  km <-kmeans(select(fd,-country_name),centers=i)
  wss[i]<-sum(km$withinss)/sum(km$totss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

set.seed(1001)
fit <- kmeans(select(fd,-country_name), centers=3,iter.max=100,nstart=1)
fd <- data.frame(fd, fit$cluster)

fd <- fd %>%
  gather(Year,Value,-c(country_name,fit.cluster)) %>%
  group_by(fit.cluster) %>%
  mutate(cluster.count = n_distinct(country_name)) %>%
  ungroup() %>%
  mutate(Year = as.numeric(gsub("Year","",Year)),
         fit.cluster = paste("Cluster ",fit.cluster," (",cluster.count," Countries)",sep=""))

ggplot(fd,aes(Year,Value,colour=as.factor(fit.cluster)))+
  geom_point(shape=1)+geom_smooth()+
  scale_colour_discrete(guide=FALSE)+
  labs(title="Changes in food deficit in Africa Between 1992 and 2013 - Africa",x="Year",y="food deficit Rate (scaled)")+
  facet_wrap(~fit.cluster)

# Where are these clusters located?
map.africa2 <- merge(map_data(map="world", region = africa),select(fd,country_name,fit.cluster), by.x='region', by.y='country_name',  all.x=TRUE,fill=0)
map.africa2 <- map.africa2[order(map.africa2$order),]
ggplot(map.africa2) +
  geom_map(map=map.africa2, aes(map_id=region, x=long, y=lat, fill=fit.cluster)) +
  borders("world",colour="black")+
  scale_fill_discrete(name="")+
  labs(x="Longitude",y="Latitude")+
  theme(legend.position="bottom") + 
  coord_map("ortho", orientation = c(10, 15, 0))

# Where are food deficit rates increasing and decreasing?
# Below is a regression of each country's employment against time (year). 
# This is made simple with the dplyr library.

fd <- indicators %>%
  filter(IndicatorCode == "SN.ITK.DFCT"
         & CountryName %in% africa) %>%
  select(CountryName,Year,Value) %>%
  group_by(CountryName) %>%
  do(ue.rate=coef(lm(Value~Year,data=.))[[2]])

# Convert from list to numeric. 
fd$ue.rate<-as.numeric(unlist(fd$ue.rate))

map.africa3 <- merge(map_data(map="world", region = africa), fd,by.x='region',by.y='CountryName',all.x=TRUE, fill=0)
map.africa3 <- map.africa3[order(map.africa3$order),]
ggplot(map.africa3) +geom_map(map=map.africa3, aes(map_id=region, x=long, y=lat, fill=ue.rate)) +borders("world",colour="black")+scale_fill_continuous(low="yellow",high="red",name="Annual Food Deficit Rate Change - Africa")+  labs(x="Longitude",y="Latitude") + theme(legend.position="bottom")  + coord_map("ortho", orientation = c(10, 15, 0))







rm(list=ls())
# title: " GNI Current US Dollar - Africa"
# indicator code: NY.GNP.MKTP.CD 
indicators <- read.csv("Indicators.csv")

# Correction
correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")

for (c in names((correction))) {
  indicators[indicators$CountryName==c,"CountryName"] = correction[c]
}
africa <- c("Algeria","Angola","Benin",
            "Botswana","Burkina Faso","Burundi","Ivory Coast","Cabo Verde","Cameroon",
            "Central African Republic","Chad","Comoros",
            "Democratic Republic of the Congo","Republic of Congo","Djibouti","Egypt",
            "Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia","Ghana","Guinea",
            "Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar","Malawi",
            "Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia",
            "Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles",
            "Sierra Leone","Somalia","South Africa","South Sudan","Sudan","Swaziland",
            "Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe")

gni2013<- indicators %>%
  filter(Year==2013
         & IndicatorCode =="NY.GNP.MKTP.CD"
         & CountryName %in% africa)

map.africa <- merge(map_data(map="world", region = africa), select(gni2013,CountryName,Value),  by.x='region', by.y='CountryName', all.x=TRUE, fill=0)
map.africa <- map.africa[order(map.africa$order),]
map.africa$Value <- map.africa$Value/10000000000 
ggplot(map.africa) + geom_map(map=map.africa, aes(map_id=region, x=long, y=lat, fill=Value)) +borders("world",colour="black") + scale_fill_gradient(low ="yellow",high="red",guide="colourbar",name="GNI (%)") + labs(title="GNI (2013)",x="Longitude",y="Latitude")+theme(legend.position="bottom") + coord_map("ortho", orientation = c(10, 15, 0))

# What clusters can a k-means analysis point out?
gni <- indicators %>%
  filter(IndicatorCode == "NY.GNP.MKTP.CD"
         & CountryName %in% africa) %>%
  select(CountryName,Year,Value)  %>%
  spread(Year,Value,sep="")
country_name <-gni$CountryName
gni <- gni %>%
  select(-CountryName) %>%
  t() %>%
  scale(center=TRUE,scale=FALSE) %>%
  t()
gni <- data.frame(country_name,gni)
for(i in 1:ncol(gni)) {
  gni[ , i][is.na(gni[ , i])] <- mean(gni[ , i], na.rm = TRUE)
}

# Determine number of clusters
wss<-numeric()
for (i in 2:15) {
  km <-kmeans(select(gni,-country_name),centers=i)
  wss[i]<-sum(km$withinss)/sum(km$totss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
set.seed(1001)
fit <- kmeans(select(gni,-country_name), centers=3,iter.max=100,nstart=1)
gni <- data.frame(gni, fit$cluster)
gni <- gni %>%
  gather(Year,Value,-c(country_name,fit.cluster)) %>%
  group_by(fit.cluster) %>%
  mutate(cluster.count = n_distinct(country_name)) %>%
  ungroup() %>%
  mutate(Year = as.numeric(gsub("Year","",Year)),
         fit.cluster = paste("Cluster ",fit.cluster," (",cluster.count," Countries)",sep=""))
gni$Value <- gni$Value/100000000
ggplot(gni,aes(Year,Value,colour=as.factor(fit.cluster)))+
  geom_point(shape=1)+geom_smooth()+
  scale_colour_discrete(guide=FALSE)+
  labs(title="Changes in GNI Between 1960 and 2014 - Africa",x="Year",y="GNI (scaled)")+
  facet_wrap(~fit.cluster)

# Where are these clusters located?
map.africa2 <- merge(map_data(map="world",region = africa),
                     select(gni,country_name,fit.cluster),
                     by.x='region',
                     by.y='country_name',
                     all.x=TRUE,
                     fill=0)
map.africa2 <- map.africa2[order(map.africa2$order),]
ggplot(map.africa2) +
  geom_map(map=map.africa2, aes(map_id=region, x=long, y=lat, fill=fit.cluster)) +
  borders("world",colour="black")+
  scale_fill_discrete(name="")+
  labs(x="Longitude",y="Latitude")+
  theme(legend.position="bottom") + 
  coord_map("ortho", orientation = c(10, 15, 0))

# Where in africa gni rates increasing and decreasing?
gni <- indicators %>%
  filter(IndicatorCode == "NY.GNP.MKTP.CD"
         & CountryName %in% africa) %>%
  select(CountryName,Year,Value) %>%
  group_by(CountryName) %>%
  do(gni.rate=coef(lm(Value~Year,data=.))[[2]])

# Convert from list to numeric. 
gni$gni.rate<-as.numeric(unlist(gni$gni.rate))

map.africa3 <- merge(map_data(map="world", region = africa),  gni,  by.x='region', by.y='CountryName', all.x=TRUE, fill=0)
map.africa3 <- map.africa3[order(map.africa3$order),]
ggplot(map.africa3) +
  geom_map(map=map.africa3, aes(map_id=region, x=long, y=lat, fill=gni.rate)) +
  borders("world",colour="black")+ scale_fill_continuous(low="yellow",high="red",name="Annual GNI Rate Change - Africa")+
  labs(x="Longitude",y="Latitude")+ theme(legend.position="bottom")  + coord_map("ortho", orientation = c(10, 15, 0))
