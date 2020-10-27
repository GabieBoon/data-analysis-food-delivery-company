# Gebruikte packages
library(DBI)
library(RMySQL)
library(odbc)
library("dbplyr")
library(leaflet)
library(jsonlite)
library(httr)
library(stringr)
library(tidyverse)
library(geojsonio)
library(lubridate)
library(tidyr)
library(gdata)
library("corrplot")
library(rjson)
library(timeDate)

# Inlog database
user = 'user'
password = 'password'

dbConn <- dbConnect(
  MySQL(),
  user,
  password,
  dbname = 'dbname',
  host = 'host'
)
# Verkenning: welke tabellen hebben we in de database?
dbListTables(dbConn)

# Het ophalen van de data van de database
All_Orders_original <- as.data.frame(tbl(dbConn, "All_Orders"))
PostOrders_original <- as.data.frame(tbl(dbConn, "PostOrders"))

Items_original <- as.data.frame(tbl(dbConn, "Items"))
PostItems_original <- as.data.frame(tbl(dbConn, "PostItems"))

Subitems_original <- as.data.frame(tbl(dbConn, "SubItems"))
PostSubItems_original <- as.data.frame(tbl(dbConn, "PostSubItems"))

# Nu de data is ingeladen, kunnen we de connectie met de database weer sluiten
RMySQL::dbDisconnect(dbConn)

############### 
#Samenvoegen van oude en  nieuwe data (verdiepingsvraag + data cleaning)
##############

# irrelevante kolommen (volgens de aangeleverde pdf)
irrelevant_PostOrders <- c("_etag", "account", "by", "channel", "dateString_updated",
                           "decimalDigits", "deliveryIsAsap", "hour_updated", "hourAndMinute_updated",
                           "minute_updated", "numberOfCustomers", "orderIsAlreadyPaid", "orderType",
                           "platform", "posCustomerId", "posId", "posLocationId", "weekday_updated")

## Irrelevante kolommen verwijderen
PostOrders <- PostOrders_original %>% 
  select(-irrelevant_PostOrders, -id)

# irrelevante / dubbele kolommen
irrelevant_Items <- c("weekdayPickUpTime", "hourPickUpTime", "minutePickUpTime",
                      "hourAndMinutePickUpTime", "weekday_created", "hour_created",
                      "minute_created", "hourAndMinute_created", "dateStringPickUpTime",
                      "dateString_created", "channelOrderDisplayId", "channelLink",
                      "courierDeliveryBy", "customerName", "customerPhoneNumber", "customerEmail",
                      "deliveryAddressStreet", "deliveryAddressStreetNumber", "deliveryAddressPostalCode",
                      "deliveryAddressCity", "paymentAmount","note", "channelOrderRawId",
                      "serviceCharge", "deliveryCost", "discountTotal", "id", "restaurant")

PostItems <- PostItems_original %>% 
  select(-irrelevant_PostOrders, -irrelevant_Items)

irrelevant_SubItems <- c("itemsName", "itemsPlu", "itemsPrice", "itemsProductType", "itemsQuantity")

PostSubItems <- PostSubItems_original %>% 
  select(-irrelevant_PostOrders, -irrelevant_Items, -irrelevant_SubItems)

## Format van All_Orders aanpassen
All_Orders <- All_Orders_original %>% 
  mutate(channelOrderRawId = NA,
         serviceCharge = NA,
         channelOrderDisplayId = NA) %>% 
  rename(channelOrderId = channelOrderID,
         courierDeliveryBy = platform,
         weekdayPickUpTime = weekday2,
         dateStringPickUpTime = dateString2,
         hourPickUpTime = hour2,
         minutePickUpTime = minute2,
         hourAndMinutePickUpTime = hourAndMinute2,
         weekday_created = weekday,
         dateString_created = dateString,
         hour_created = hour,
         minute_created = minute,
         hourAndMinute_created = hourAndMinute) %>% 
  select(channelOrderRawId,serviceCharge, channelOrderDisplayId,
         weekdayPickUpTime,
         dateStringPickUpTime,
         courierDeliveryBy,
         hourPickUpTime,
         minutePickUpTime,
         hourAndMinutePickUpTime,
         weekday_created,
         hour_created,
         minute_created,
         dateString_created,
         hourAndMinute_created,
         channelLink,
         customerName,
         customerPhoneNumber,
         customerEmail,
         deliveryAddressStreet,
         deliveryAddressStreetNumber,
         deliveryAddressPostalCode,
         deliveryAddressCity,
         paymentAmount,
         note,
         deliveryCost,
         discountTotal,
         restaurant,
         channelOrderId)

## PostOrders en All_orders samenvoegen
new_PostOrders <-  rbind(PostOrders, All_Orders)

## Opschonen datums (cleaning)
## Verkeerd format
summary(grepl("\\b[0-9]{1}\\b", new_PostOrders$minute_created))
## Goede format
summary(grepl("\\b[0-9]{1}\\b", new_PostOrders$minutePickUpTime))
## Verkeerd format
summary(grepl("\\b[0-9]{3}\\b", new_PostOrders$minute_created))
## Goede format
summary(grepl("\\b[0-9]{3}\\b", new_PostOrders$minutePickUpTime))
## Verkeerd format
summary(grepl("\\b[0-9]{1}\\b", new_PostOrders$hour_created))
## Goede format
summary(grepl("\\b[0-9]{1}\\b", new_PostOrders$hourPickUpTime))
## Goede format
summary(grepl("\\b[0-9]{3}\\b", new_PostOrders$hour_created))
## Goede format
summary(grepl("\\b[0-9]{3}\\b", new_PostOrders$hourPickUpTime))

## Format datums verbeteren
new_PostOrders <- new_PostOrders %>% 
  mutate(minute_created = gsub("(\\b[0-9]{1}\\b)", "0\\1", minute_created, perl = T),
         minute_created = gsub("\\b[0]([0-9]{2}\\b)", "\\1", minute_created, perl = T),
         hour_created = gsub("(\\b[0-9]{1}\\b)", "0\\1", minute_created, perl = T),
         hourAndMinutePickUpTime = ifelse(is.na(hourPickUpTime) | is.na(minutePickUpTime), 
                                          NA, paste(hourPickUpTime, minutePickUpTime, sep = ":")),
         hourAndMinute_created = ifelse(is.na(hour_created) | is.na(minute_created), 
                                        NA, paste(hour_created, minute_created, sep = ":")))

## Format controleren
summary(grepl("\\b[0-9]{1}\\b", new_PostOrders$minute_created))
## Format controleren
summary(grepl("\\b[0-9]{3}\\b", new_PostOrders$minute_created))
## Format controleren
summary(grepl("\\b[0-9]{1}\\b", new_PostOrders$hour_created))

## Format Items aanpassen
Items <- Items_original %>% 
  transmute(
    itemsName,
    itemsPlu = ItemsPlu,
    itemsPrice,
    itemsQuantity,
    itemsProductType = NA,
    channelOrderId = OrderID)

## PostItems en Items combineren
new_PostItems <-  rbind(PostItems, Items)

## Format SubItems aanpassen
SubItems <- Subitems_original %>% 
  transmute(
    subItemsName,
    subItemsPlu,
    subItemsPrice,
    subItemsProductType,
    subItemsQuantity,
    channelOrderId = OrderID)

## PostSubItems en SubItems combineren
new_PostSubItems <-  rbind(PostSubItems, SubItems)

# 1 grote dataset joinen (alle dubbele gegevens zijn er uit gehaald (bijvoorbeeld in elke dataset de tijd van bestelling))
totaal = full_join(new_PostItems, new_PostOrders, by = "channelOrderId") %>%
  # Postcode omschrijven naar 1 format (datakwaliteit issue)
  mutate(deliveryAddressPostalCode = gsub(deliveryAddressPostalCode, pattern = ' ', replacement = ''),
         deliveryAddressPostalCode = gsub(pattern = '([[:upper:]])', 
                                          perl = TRUE, replacement = '\\L\\1', deliveryAddressPostalCode),
         # DatestringPickUpTime als datum (datakwaliteit issue)
         dateStringPickUpTime = ymd(dateStringPickUpTime),
         # PaymentAmount als double (datakwaliteit issue)
         paymentAmount = as.double(paymentAmount),
         # DeliveryCost als double (datakwaliteit issue)
         deliveryCost = as.double(deliveryCost),
         # DiscountTotal als double (datakwaliteit issue)
         discountTotal = as.double(discountTotal))

### Opschonen werkgeheugen
keep("new_PostItems", "new_PostOrders", "new_PostSubItems", "totaal", sure = T)

############### 
#Verkennende plots: wat staat er in de data?
##############
# Spreiding van de drukte over de dag
# Alleen de unieke bestel i'ds overhouden (want binnen 1 order kunnen meerdere items besteld zijn)
Spreidingklantdag <- totaal[!duplicated(totaal[,c('channelOrderId')]),] %>%
  group_by(hourPickUpTime) %>%
  count() %>% 
  ungroup() %>% 
  filter(!is.na(hourPickUpTime))

# Spreiding van de drukte over de dag: #NA's uitgefiltert (want je weet niet hoe laat dit was, dus zegt niks)
ggplot(Spreidingklantdag, aes(x = hourPickUpTime, y = n)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Spreiding van het aantal orders over de dag \nper april 2019",
       x = "Tijdstip van ophalen",
       y = "Aantal geplaatste orders") +
  theme_classic()

#### Meest populaire producten (top 5) (en hiermee is dus ook meteen de frites de modus)
PopulaireProducten <- totaal %>% 
  mutate(itemsQuantity = as.integer(itemsQuantity),
         # We zagen dat patat meerdere keren voorkwam, door meerdere restaurants. Deze hebben we samengevoegd
         itemsName = ifelse(str_detect(itemsName, "Fries"), "Frites", ifelse(str_detect(itemsName, "fries"), "Frites",itemsName))) %>% 
  group_by(itemsName) %>% 
  summarise(n = sum(itemsQuantity)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  top_n(n, n = 5)
# Het maken van de plot
ggplot(PopulaireProducten, aes(x = reorder(itemsName, n), y = n)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "De meest bestelde producten \nper april 2019",
       x = "Product",
       y = "Aantal keer besteld",
       caption = "*Alle frites zijn samengevoegd") +
  theme_classic()

##### Gebruik restaurants (aandeel in verkoopaantal)
#### Wanneer was de laatste bestelling van elk restaurant?
totaal%>% 
  mutate(dateStringPickUpTime = ymd(dateStringPickUpTime)) %>% 
  select(dateStringPickUpTime, restaurant) %>% 
  filter(restaurant == "Queens Beyond Burger") %>% 
  arrange(desc(dateStringPickUpTime)) %>% 
  head(n = 1)
# Code is getest op elk van de 7 restaurants
openrestaurants <- c("Vegan Burger Brothers", "Queens Beyond Burger", "Doner Dudes",
                     "Las Vegan", "Holy VEGAN Mac n Cheesus", "Vinnies Vegan Pizza")

# Alleen de unieke bestel id's overhouden en aangeven of restaurant gesloten is of niet
GebruikRestaurantsclean <- totaal[!duplicated(totaal[,c('channelOrderId')]),] %>%
  filter(restaurant != (is.na(restaurant)),
         restaurant != "") %>% 
  group_by(restaurant) %>%
  count() %>% 
  ungroup() %>% 
  mutate(Percentage = (n / sum(n)) * 100,
         # Er zit een stukje HTML in de naam van het restaurant, we halen deze er nu uit
         restaurant = ifelse(str_detect(restaurant, "mpany"), "The Burger Company", restaurant),
         open = ifelse(restaurant %in% openrestaurants, "Open", "Gesloten")) 
# Het maken van de plot
ggplot(GebruikRestaurantsclean, aes(x = reorder(restaurant, Percentage), y = Percentage, fill = open)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Aandeel in aantal orders per restaurant \nper april 2019",
       x = "Restaurant",
       y = "Aandeel in procenten") +
  theme_classic() +
  # Wijzigen titel legenda
  scale_fill_discrete(name = "Status restaurant")

#### Omzet boxplots per open restaurant
omzetperrestaurant <- totaal[!duplicated(totaal[,c('channelOrderId')]),] %>% 
  mutate(restaurant = as.factor(restaurant)) %>% 
  filter(restaurant %in% openrestaurants,
         # Alleen de bedragen onder de 100 euro overhouden. Zitten namelijk een paar enrome uitschieters in
         paymentAmount <= 100) %>% 
  select(restaurant, paymentAmount)

#Plotten data
# Hier halen we uit dat het derde kwantiel iedere keer rond de 25 euro zit > 75% van de bestellingen is <25 euro)
ggplot(omzetperrestaurant, aes(x = restaurant, y = paymentAmount, fill = restaurant)) + 
  # Maak er een boxplot van
  geom_boxplot()+ 
  # Verwijder de automatisch gegenereerde legenda
  theme(legend.position = "none") + 
  # Voeg (as)titels toe
  labs(title = "Bestelbedrag per restaurant per april 2019", 
       x = "Restaurant", y = "Bestelbedrag",
       caption = "*gefilterd op bestelbedragen < 100 euro")

# Wat is het gemiddelde bestelbedrag?
mean(totaal$paymentAmount, na.rm = T) # Gemiddeld bestelbedrag is ???25,32

### Opschonen werkgeheugen
keep("new_PostItems", "new_PostOrders", "new_PostSubItems", "totaal", sure = T)

######## 
# Kaart maken: waar zitten de klanten? (verdiepingsvraag2 + verkennend)
########

# Welke unieke postcodes zijn er allemaal in de data?
Uniekepostcodes = totaal %>%
  # Filter alleen de takeaway's. Want hier heb je alleen informatie over de klanten
  filter(courierDeliveryBy == "takeaway") %>%
  distinct(deliveryAddressPostalCode) %>% 
  arrange(deliveryAddressPostalCode) %>% 
  filter(nchar(deliveryAddressPostalCode) == 6)

# Controleren of alles nu het juiste format heeft
table(grepl(x= Uniekepostcodes$deliveryAddressPostalCode, pattern = "(\\b[0-9]{4}[a-z]{2})", perl = T))

# De API waarvan we de lon en lat van af gaan halen
postalcode_url = "http://prdurbana0001.ict.hva.nl/nominatim/search?format=jsonv2&postalcode="

# Functie die nodig is om m.b.v. API de coördinaten op te halen van de postcodes
coordinatenPostcode <- function(location) {
  # Functie om de lon en lat van een plaats te berekenen met behulp van open street maps API
  out <- tryCatch({
    d <- jsonlite::fromJSON(url(paste0(postalcode_url, location)))[1,]
  },
  # Aangeven dat de functie error moet teruggeven als hij geen respons krijgt
  error = function(c) return(d = data.frame(lat = NA,lon = NA))
  )
  # We willen een data frame terug krijgen: lat = ... lon = ... postcode = ...
  return (data.frame(lat=as.numeric(as.character(out['lat'])), lon=as.numeric(as.character(out['lon'])), postcode= as.character(location), stringsAsFactors = FALSE))
}

# Bereken lon en lat voor de postcodes en voeg deze aan het dataframe toe
# Dit geef 12 warnings: 6 postcodes die de API niet kan vinden (dit klopt dus)
lonlat <- lapply(Uniekepostcodes$deliveryAddressPostalCode,coordinatenPostcode)

# Het maken van een dataframe van de lon, lat en postcode
df_lonlat <- data.frame(matrix(unlist(lonlat), nrow=length(lonlat), byrow=T), stringsAsFactors = F)
names(df_lonlat) = c('Lat','Lon', 'Postcode')

# Missende postcodes. 2 lijken er wel te bestaan, maar niet op te halen met de API
df_lonlat[is.na(df_lonlat$Lat),]

# Handmatig opgezochte coordinaten
post1053vv <- c(52.368490, 4.869990, "1053vv")
post1072xr <- c(52.356770, 4.890880, "1053xr")

# Vervangen van deze postcodes
df_lonlat[which(grepl("1053vv", df_lonlat$Postcode)),] <- post1053vv
df_lonlat[which(grepl("1072xr", df_lonlat$Postcode)),] <- post1072xr

# Filteren van de data voor de takeaway (die hebben alleen coordinaten) en klaarmaken op te joinen 
filterTakeaway <- totaal %>%  
  filter(courierDeliveryBy == "takeaway")

# Het joinen van de data
filterTakeaway_incl_coord <- left_join(filterTakeaway,df_lonlat, 
                                       by = c("deliveryAddressPostalCode" = "Postcode")) %>% 
  mutate(Lon = as.double(Lon),
         Lat = as.double(Lat))%>% 
  # De ontbrekende postcodes uit de eerder opdracht er uit filteren
  filter(Lon != is.na(Lon))

# Hier filteren we nog op het unieke aantal transacties 
# We willen namelijk alleen naar het aantal bestellingen kijken en niet naar het aantal producten die besteld zijn
filterTakeaway_incl_coord_unique <- filterTakeaway_incl_coord[!duplicated(filterTakeaway_incl_coord [,c('channelOrderId')]),]

# We hebben op Google Maps gekeken hoe ver een punt hemelsbreed is vanaf het restaurant, dit als marker op de kaart
# geplot en vervolgens de radius door trial and error te plotten.
# We gaan bewust uit van worst case (2km): zelfs dan willen we overlap hebben tussen de restaurants (mocht de straal verkleint worden)
kilometer2 = 2170

coordinaatRestaurant <- c(52.3713687,4.8776602,20)
coordinaatNieuweLoc <- c(52.3567754,4.8908797)
coordinaatPotentieel <- c(52.360271, 4.9328693)

# Het plotten van de kaart: Waar komen de bestellingen vandaan? (Let op: dit gaat alleen om 'Takeaway')
leaflet(data = filterTakeaway_incl_coord_unique,
        options = leafletOptions(minZoom = 12.4, maxZoom = 16)) %>%
  # Kleur van de kaart kiezen
  addProviderTiles("nlmaps.standaard")  %>%
  # Vastzetten van het beginpunt van de kaart
  setView(lng = coordinaatRestaurant[2] , lat =  coordinaatRestaurant[1] , zoom = 12.4) %>%
  # Toevoegen cirkelvormige markeringen
  addMarkers(lng =  ~Lon,
             lat = ~Lat,
             clusterOptions = markerClusterOptions()) %>% 
  # Toevoegen van restaurant locatie
  addMarkers(lat = coordinaatRestaurant[1],
             lng = coordinaatRestaurant[2],
             popup = "Bright Kitchen") %>% 
  # Toevoegen van straal 2 kilometer
  addCircles(lng = coordinaatRestaurant[2], 
             lat = coordinaatRestaurant[1], radius = kilometer2, color = "red") %>% 
  # Toevoegen van De Pijp locatie
  addMarkers(lat = coordinaatNieuweLoc[1],
             lng = coordinaatNieuweLoc[2],
             popup = "Locatie2") %>% 
  # Toevoegen van straal 2 kilometer
  addCircles(lng = coordinaatNieuweLoc[2],
             lat = coordinaatNieuweLoc[1], radius = kilometer2, color = "green") %>% 
  # Toevoegen van Potentiële locatie
  addMarkers(lat = coordinaatPotentieel[1],
             lng = coordinaatPotentieel[2],
             popup = "Potentieel locatie 3") %>%  
  # Toevoegen van straal 2 kilometer
  addCircles(lng = coordinaatPotentieel[2],
             lat = coordinaatPotentieel[1], radius = kilometer2, color = "blue")

# Opschonen werkgeheugen
keep("new_PostItems", "new_PostOrders", "new_PostSubItems", "totaal", "filterTakeaway_incl_coord_unique", sure = T)

######## 
# Het bouwen van het voorspellingsmodel
########

# Ook hier willen we alleen kijken naar de order alleen (en niet de omvang van de order)
Voorspellingsdataschoon <- totaal[!duplicated(totaal[,c('channelOrderId')]),] %>%
  group_by(dateStringPickUpTime) %>% 
  summarise(Total_orders = n()) %>% 
  mutate(Dagweek = wday(dateStringPickUpTime, label = T)) %>% 
  mutate(nummer = wday(dateStringPickUpTime)) %>% 
  mutate(weekend = ifelse(nummer >=2 & nummer <= 5, 0, 1))

#### KNMI data ophalen
# maak een variabele genaamd 'start', met het beginmoment van je grafiek
start<-"2019-01-01,00:00"

# maak een variabele genaamd 'eind', met het eindmoment van je grafiek
end<-"2020-12-31,23:59"

# zet de starttijd en eindtijd om in het formaat van de KNMI API
knmistart<-unlist(strsplit(start, ","))[1]
knmistart<-gsub("-", "", knmistart)
knmiend<-unlist(strsplit(end, ","))[1]
knmiend<-gsub("-", "", knmiend)

#ophalen van data KNMI
knmidata <- read.table(paste("http://projects.knmi.nl/klimatologie/uurgegevens/getdata_uur.cgi?stns=260&vars=T:RH&start=",knmistart,"01&end=",knmiend,"24",sep=""), sep=",", header=F)
colnames(knmidata) <- c("station", "dag", "uur", "temperatuur", "neerslag")

# KNMI regen
knmidataregen <- knmidata %>%
  mutate(neerslag = gsub("-1", "0", knmidata$neerslag)) %>%
  mutate(neerslag = as.numeric(neerslag) / 10) %>% 
  mutate(dag = ymd(dag)) %>%
  select(dag, neerslag) %>%
  group_by(dag) %>%
  mutate(neerslag = sum(neerslag)) %>%
  unique()
# KNMI temperatuur (we kijken naar de gemiddelde temperatuur over de dag (ipv rond etenstijd) > bleek meeste invloed te hebben)
knmidatatemperatuur <- knmidata %>% 
  select(dag, uur, temperatuur) %>% 
  mutate(uur = as.integer(uur),
         dag = ymd(dag)) %>%
  group_by(dag) %>% 
  summarise(temperatuur = mean(temperatuur)/10) 

### Discount toevoegen
DayOrders <- totaal[!duplicated(totaal[,c('channelOrderId')]),] %>%
  select(dateStringPickUpTime, discountTotal) %>%
  group_by(dateStringPickUpTime) %>%
  summarise(Total_orders = n())

TotalDiscount <- totaal[!duplicated(totaal[,c('channelOrderId')]),] %>% 
  select(dateStringPickUpTime, discountTotal) %>% 
  group_by(dateStringPickUpTime) %>% 
  summarise(Total_Discount = sum(discountTotal))

Average_Discount <- DayOrders %>% 
  left_join(TotalDiscount, by = "dateStringPickUpTime") %>% 
  mutate(Total_Discount = abs(Total_Discount)) %>% 
  na.omit(new_PostOrders) %>% 
  select(-Total_orders)

### De kolommen samenvoegen zodat er 1 overzichtelijk samengevat dataframe overblijft
Voorspeldataa <- left_join(Voorspellingsdataschoon, knmidataregen, by = c("dateStringPickUpTime" = "dag"))
Voorspeldataa1 <- left_join(Voorspeldataa, knmidatatemperatuur, by = c("dateStringPickUpTime" = "dag"))
Voorspeldataa2 <- left_join(Voorspeldataa1, Average_Discount, by = c("dateStringPickUpTime")) %>%
  # 1 januari outlier er uit filteren
  filter(dateStringPickUpTime != "2020-01-01")

# Het opdelen van de data in 75% train (model op bouwen) en 25% test set
set.seed(100)
sample <- sample.int(n = nrow(Voorspeldataa2), size = floor(.75*nrow(Voorspeldataa2)), replace =F)
train <- Voorspeldataa2[sample, ]
test <- Voorspeldataa2[-sample, ]

# Hoe sterk is de correlaties tussen de variabelen?
traincor <- train %>% 
  select(-dateStringPickUpTime, - Dagweek)
m <- cor(traincor)
corrplot(m, method = "circle")

# Controleren van de correlaties
cor(train$Total_orders, train$weekend) # correlatie van 0.58 (goed) > weekend: meer bestellingen
cor(train$Total_orders, train$temperatuur) # correlatie van -0.38 (goed) > hoge temp: minder bestellingen
cor(train$Total_orders, train$neerslag) # corrlatie van 0.01 (niet goed) > meer neerslag: meer bestellingen
cor(train$Total_orders, train$Total_Discount) # correlatie van 0.39 (goed) > meer korting: meer bestellingen
# Nummer komt erg overeen met weekend, wel correlatie: maar niet in het model ('kans op overfitting')
cor(train$Total_orders, train$nummer) # correlatie van 0.19 (matig)

# Bevestiging: temperatuur heeft invloed
ggplot(train, aes(x = temperatuur, y = Total_orders)) + geom_point() + geom_smooth(method = "lm") +
  labs(title = "De invloed van temperatuur op het aantal orders", 
       x = "Gemiddelde temperatuur in graden Celcius", y  = "Aantal orders")
# Bevestiging: Total_Discount heeft invloed
# We krijgen een warning: dit klopt. We zoomen in op onder de 100 euro (dus er vallen nu punten buiten)
ggplot(train, aes(x = Total_Discount, y = Total_orders)) + geom_point() + geom_smooth(method = "lm") +
  xlim(0,120) + 
  labs(title = "De invloed van korting op het aantal orders", 
       x = "Korting in euro's", y  = "Aantal orders",
       caption = "*Gefilterd op korting < 120 euro")
# Bevestiging: neerslag heeft geen invloed
ggplot(train, aes(x = neerslag, y = Total_orders)) + geom_point() + geom_smooth(method = "lm")  + 
  labs(title = "De invloed van neerslag op het aantal orders", 
       x = "Neerslag in milimeters's", y  = "Aantal orders")

# We kiezen dus voor temperatuur, weekend en discount
model <- lm(Total_orders ~ temperatuur + weekend + Total_Discount, data = train)

# De samenvatting van het model: 3 enorm lage p-waarden (en veel *, dat is goed). 
# Dit betekent wanneer je deze variabele er uit zou laten dat het model dan minder goed wordt.
# Gemiddelde residu van 19 (gemiddeld zit het model er 19 orders naast). 
# R^2 van 0.5054 -> Ongeveer 50% van de variatie wordt verklaard door de variabelen.
# De mediaan van de residuën ligt rond de 0 (goed).
summary(model)

## Test : de residuen zijn normaal verdeeld rond 0 TRUE
ggplot(data = train, aes(model$residuals)) + 
  geom_histogram(color = "black", fill = "purple4", bins = 25) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(),
        axis.line.y = element_line()) +
  ggtitle("Histogram van de residuen van het model") +
  labs(y = "Aantal", x = "Residu")

# Het maken van de qplot: hoe dichterbij de blauwe lijn op de rode zit hoe beter (erg goed dus)
ggplot(data = train, aes(sample = Total_orders)) +
  stat_qq(size = 0.5, col = "blue") +
  stat_qq_line(col = "red") +
  ggtitle("QQ-Plot totaal aantal orders", subtitle = "Theoretisch normaal")

# De uiteindelijke voorspelling doen voor alle restaurants
voors <- predict(model, test)
df_test <- data.frame(test, verwacht = voors) %>% 
  mutate(verschil = verwacht -Total_orders)

# Schets van voorspelling en verwachting (de NA wordt er uit gefilterd > komt een warning van)
#### Test onafhankelijkheid en homescredastisch: JA
ggplot(df_test, aes(x = dateStringPickUpTime)) +
  geom_point(aes(y = verschil, color = ifelse(verschil <= 20 & verschil >= -20, "red", "green"))) +
  # Verwijder de automatisch gegenereerde legenda
  theme(legend.position = "none") +
  geom_hline(yintercept = 0)  + 
  labs(title = "Het verschil in orders tussen de werkelijkheid \nen het voorspellingsmodel", 
       x = "Datum", y  = "Verschil in orders",
       caption = "*kleur op basis van meer of minder dan 20 verschil")

###### 
# Voorspellen totaal aantal orders op basis van het model
######

# Ophalen van de weerdata voor over 1, 2 en 7 dagen
weather_forecast_api <- GET("https://api.weatherbit.io/v2.0/forecast/daily?city=Amsterdam&country=NL&key=65cd58443c704ca783e9936bea474e34")
weather_forecast_api <- fromJSON(rawToChar(weather_forecast_api$content))

weatherforecast_data <- data.frame(matrix(unlist(weather_forecast_api$data), nrow=length(weather_forecast_api$data), byrow=T))
colnames(weatherforecast_data) <- c("moonrise_ts", "wind_cidr", "rh", "pres", "sunset_ts", "ozone", "moon_phase", "wind_gust_spd", "snow_depth", "clouds", "ts", "sunrise_ts", "app_min_temp", "wind_spd", "pop", "wind_cdir_full", "slp", "valid_date", "app_max_temp", "vis", "dewpt", "snow", "uv", "weather_icon", "weather_code", "weather_description", "wind_dir", "max_dhi", "clouds_hi", "precip", "low_temp", "max_temp", "moonset_ts", "datetime", "temp", "min_temp", "clouds_mid", "clouds_low")

weatherforecast_data <- weatherforecast_data %>%
  select(datetime, temp, precip) %>%
  rename(neerslag = precip,
         temperatuur = temp) %>%
  mutate(datetime = as.Date(datetime),
         temperatuur = as.numeric(temperatuur))

# Alle variabelen aanmaken voor de toekomst
Datum <- c(weatherforecast_data$datetime[2],weatherforecast_data$datetime[3],weatherforecast_data$datetime[8])
Total_Discount <- c(0, 0, 0)

# Aanmaken dataframe met weerinformatie, dag van de week en discount
Toekomst_Frame_Totaal <- data.frame(Datum, Total_Discount)
Toekomst_Frame_Totaal <- left_join(Toekomst_Frame_Totaal, weatherforecast_data, by = c("Datum" = "datetime")) %>% 
  mutate(nummer = wday(Datum), 
         Dagweek = weekdays(Datum), 
         weekend = as.numeric(isWeekend(Datum, wday = 1:4)))

Voorspelling_Orders_Totaal <- predict(model, Toekomst_Frame_Totaal)
# Omdat we gebruik maken van een API, kan deze code elke dag gerund worden en vult deze automatisch de volgende dagen in
Voorspelling_Orders_Totaal_data_frame <- data.frame(Toekomst_Frame_Totaal, Voorspelling_Orders_Totaal)
# De daadwerkelijke voorspelling:
Voorspelling_Orders_Totaal_data_frame

# Opschonen werkgeheugen
keep("new_PostItems", "new_PostOrders", "new_PostSubItems", "totaal",
     "filterTakeaway_incl_coord_unique", "df_test", "model",
     "knmidatatemperatuur", "knmidataregen", "Average_Discount",
     "Voorspelling_Orders_Totaal_data_frame", "weatherforecast_data",sure = T)



########
#UITSPLITSING MODEL PER RESTAURANT 
########

### Eerst filteren we op de data per restaurant
Voorspellingsdataschoon_Restaurants <- totaal[!duplicated(totaal[,c('channelOrderId')]),] %>% 
  mutate(Dagweek = wday(dateStringPickUpTime, label = T)) %>% 
  mutate(nummer = wday(dateStringPickUpTime)) %>% 
  mutate(weekend = ifelse(nummer >=2 & nummer <= 5, 0, 1))

Voorspellingsdata_Brothers <- Voorspellingsdataschoon_Restaurants %>%
  filter(restaurant == "Vegan Burger Brothers") %>%
  group_by(dateStringPickUpTime) %>% 
  mutate(Total_orders = n()) %>% 
  select(dateStringPickUpTime, Total_orders, Dagweek, nummer, weekend, restaurant)

Voorspellingsdata_LasVegan <- Voorspellingsdataschoon_Restaurants %>%
  filter(restaurant == "Las Vegan") %>%
  group_by(dateStringPickUpTime) %>% 
  mutate(Total_orders = n()) %>% 
  select(dateStringPickUpTime, Total_orders, Dagweek, nummer, weekend, restaurant)

Voorspellingsdata_Queens <- Voorspellingsdataschoon_Restaurants %>%
  filter(restaurant == "Queens Beyond Burger") %>%
  group_by(dateStringPickUpTime) %>% 
  mutate(Total_orders = n()) %>% 
  select(dateStringPickUpTime, Total_orders, Dagweek, nummer, weekend, restaurant)

Voorspellingsdata_Dudes <- Voorspellingsdataschoon_Restaurants %>%
  filter(restaurant == "Doner Dudes") %>%
  group_by(dateStringPickUpTime) %>% 
  mutate(Total_orders = n()) %>% 
  select(dateStringPickUpTime, Total_orders, Dagweek, nummer, weekend, restaurant)

Voorspellingsdata_Holy <- Voorspellingsdataschoon_Restaurants %>%
  filter(restaurant == "Holy VEGAN Mac n Cheesus") %>%
  group_by(dateStringPickUpTime) %>% 
  mutate(Total_orders = n()) %>% 
  select(dateStringPickUpTime, Total_orders, Dagweek, nummer, weekend, restaurant)

Voorspellingsdata_Vinnies <- Voorspellingsdataschoon_Restaurants %>%
  filter(restaurant == "Vinnies Vegan Pizza") %>%
  group_by(dateStringPickUpTime) %>% 
  mutate(Total_orders = n()) %>% 
  select(dateStringPickUpTime, Total_orders, Dagweek, nummer, weekend, restaurant)

#### Het joinen van alle datasets zodat er 1 samenvattend dataframe komt per restaurant
leftJoin1 <- function(data1, data2) {
  data <- left_join(data1, data2, by = c("dateStringPickUpTime" = "dag"))
  return(data)
}

leftJoin2 <- function(data1, data2) {
  data <- left_join(data1, data2, by = c("dateStringPickUpTime"))
  return(data)
}

Brothers <- leftJoin1(Voorspellingsdata_Brothers, knmidataregen)
Brothers2 <- leftJoin1(Brothers, knmidatatemperatuur)
Voorspeldata_Brothers <- leftJoin2(Brothers2, Average_Discount)

Queens <- leftJoin1(Voorspellingsdata_Queens, knmidataregen)
Queens2 <- leftJoin1(Queens, knmidatatemperatuur)
Voorspeldata_Queens <- leftJoin2(Queens2, Average_Discount)

Dudes <- leftJoin1(Voorspellingsdata_Dudes, knmidataregen)
Dudes2 <- leftJoin1(Dudes, knmidatatemperatuur)
Voorspeldata_Dudes <- leftJoin2(Dudes2, Average_Discount)

Holy <- leftJoin1(Voorspellingsdata_Holy, knmidataregen)
Holy2 <- leftJoin1(Holy, knmidatatemperatuur)
Voorspeldata_Holy <- leftJoin2(Holy2, Average_Discount)

LasVegan <- leftJoin1(Voorspellingsdata_LasVegan, knmidataregen)
LasVegan2 <- leftJoin1(LasVegan, knmidatatemperatuur)
Voorspeldata_LasVegan <- leftJoin2(LasVegan2, Average_Discount)

Vinnies <- leftJoin1(Voorspellingsdata_Vinnies, knmidataregen)
Vinnies2 <- leftJoin1(Vinnies, knmidatatemperatuur)
Voorspeldata_Vinnies <- leftJoin2(Vinnies2, Average_Discount)

# Opschonen werkgeheugen
keep("new_PostItems", "new_PostOrders", "new_PostSubItems", "totaal",
     "filterTakeaway_incl_coord_unique", "df_test", "model",
     "Voorspelling_Orders_Totaal_data_frame", "weatherforecast_data",
     "Voorspeldata_Vinnies", "Voorspeldata_LasVegan", "Voorspeldata_Holy", 
     "Voorspeldata_Dudes", "Voorspeldata_Queens", "Voorspeldata_Brothers", sure = T)

# Per restaurant wordt er vervolgens een model gemaakt op basis van de trainset
######## QUEENS BEYOND BURGER #######################################################################################
set.seed(101)
sample <- sample.int(n = nrow(Voorspeldata_Queens), size = floor(.75*nrow(Voorspeldata_Queens)), replace =F)
train_Queens <- Voorspeldata_Queens[sample, ]
test_Queens <- Voorspeldata_Queens[-sample, ]

model_Queens <- lm(Total_orders ~ temperatuur + weekend + Total_Discount +nummer, data = train_Queens)
summary(model_Queens)
######## BURGER BROTHERS #######################################################################################
set.seed(101)
sample <- sample.int(n = nrow(Voorspeldata_Brothers), size = floor(.75*nrow(Voorspeldata_Brothers)), replace =F)
train_Brothers <- Voorspeldata_Brothers[sample, ]
test_Brothers <- Voorspeldata_Brothers[-sample, ]

model_Brothers <- lm(Total_orders ~ temperatuur + weekend + Total_Discount +nummer, data = train_Brothers)
summary(model_Brothers)

######## LAS VEGAN #######################################################################################
set.seed(101)
sample <- sample.int(n = nrow(Voorspeldata_LasVegan), size = floor(.75*nrow(Voorspeldata_LasVegan)), replace =F)
train_LasVegan <- Voorspeldata_LasVegan[sample, ]
test_LasVegan <- Voorspeldata_LasVegan[-sample, ]

# Nummer blijkt geen invloed te hebben: die halen we uit
model_LasVegan <- lm(Total_orders ~ temperatuur + weekend + Total_Discount, data = train_LasVegan)
summary(model_LasVegan)

######## DONER DUDES #######################################################################################
set.seed(101)
sample <- sample.int(n = nrow(Voorspeldata_Dudes), size = floor(.75*nrow(Voorspeldata_Dudes)), replace =F)
train_Dudes <- Voorspeldata_Dudes[sample, ]
test_Dudes <- Voorspeldata_Dudes[-sample, ]

model_Dudes <- lm(Total_orders ~ temperatuur + weekend + Total_Discount +nummer, data = train_Dudes)
summary(model_Dudes)
######## HOLY MAC N CHEESUS #######################################################################################
set.seed(101)
sample <- sample.int(n = nrow(Voorspeldata_Holy), size = floor(.75*nrow(Voorspeldata_Holy)), replace =F)
train_Holy <- Voorspeldata_Holy[sample, ]
test_Holy <- Voorspeldata_Holy[-sample, ]

model_Holy <- lm(Total_orders ~ temperatuur + weekend + Total_Discount +nummer, data = train_Holy)
summary(model_Holy)
######## VINNIES VEGAN PIZZA #######################################################################################
set.seed(101)
sample <- sample.int(n = nrow(Voorspeldata_Vinnies), size = floor(.75*nrow(Voorspeldata_Vinnies)), replace =F)
train_Vinnies <- Voorspeldata_Vinnies[sample, ]
test_Vinnies <- Voorspeldata_Vinnies[-sample, ]

model_Vinnies <- lm(Total_orders ~ temperatuur + weekend + Total_Discount +nummer, data = train_Vinnies)
summary(model_Vinnies)
# Opschonen werkgeheugen
keep("new_PostItems", "new_PostOrders", "new_PostSubItems", "totaal",
     "filterTakeaway_incl_coord_unique", "df_test", "model",  "Voorspelling_Orders_Totaal_data_frame", "weatherforecast_data",
     "model_Vinnies", "model_Holy", "model_Dudes","model_LasVegan","model_Brothers","model_Queens",sure = T)


######## Toekomstig dataframe #######################################################################################

# Alle variabelen aanmaken voor de toekomst
Datum <- c(weatherforecast_data$datetime[2],weatherforecast_data$datetime[3],weatherforecast_data$datetime[8])
Total_Discount <- c(0, 0, 0)

# Aanmaken dataframe met weerinformatie, dag van de week en discount
Toekomst_Frame <- data.frame(Datum, Total_Discount)
Toekomst_Frame <- left_join(Toekomst_Frame, weatherforecast_data, by = c("Datum" = "datetime")) %>% 
  mutate(nummer = wday(Datum), 
         Dagweek = weekdays(Datum), 
         weekend = as.numeric(isWeekend(Datum, wday = 1:4)))

# De voorspellingen op basis van de toekomstige data doen
Vegan_Burger_Brothers <- predict(model_Brothers, Toekomst_Frame)
Quens_Beyond_Burger <- predict(model_Queens, Toekomst_Frame)
Doner_Dudes <- predict(model_Dudes, Toekomst_Frame)
Las_Vegan <- predict(model_LasVegan, Toekomst_Frame)
Holy_Vegan_MnC <- predict(model_Holy, Toekomst_Frame)
Vinnies_Vegan_Pizza <- predict(model_Vinnies, Toekomst_Frame)

# 1 Groot dataframe, samenvattend: hoe veel orders worden er verwacht en hoe veel wordt er in totaal verwacht
Toekomst_Frame_alle_rest <- data.frame(Toekomst_Frame, Vegan_Burger_Brothers, Quens_Beyond_Burger, Doner_Dudes, Las_Vegan, Holy_Vegan_MnC, Vinnies_Vegan_Pizza) %>% 
  mutate(Totaal = Vegan_Burger_Brothers + Quens_Beyond_Burger + Doner_Dudes + Las_Vegan + Holy_Vegan_MnC + Vinnies_Vegan_Pizza)
Toekomst_Frame_alle_rest

