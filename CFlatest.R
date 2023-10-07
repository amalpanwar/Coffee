# Installed Libraries
library(readr)
library(lubridate)
library(readxl)
library(maps)
library(mapdata)
install.packages("geobr")
install.packages("geosf")
library(geobr)
library(sf)
library(patchwork)
library(corrplot)
library(dbplyr)
library(ggplot2)
library(cowplot)
library(tidytext)
library(DBI)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(TSA)
library(tseries)
library(ggfortify)
library(zoo)
library(astsa)
library(GGally)
library(TSstudio)
library(tsibble)
library(MTS)
library(fUnitRoots)
library(forecast)
library(faraway)
library(xts)
library(car)
library(fable)
library(feasts)
library(tsfeatures)
library(urca)
library(timeSeries)
library(lmtest)
library(quantmod)
library(broom)
library(forecastHybrid)
library(DCCA)
library(Metrics)
library(MASS)


#Production and Weather Data load for Brazil and Colombia
Coffee_production_Brazil <- read_excel("Coffee production Brazil.xlsx")
weather_br2 <- read_delim("conventional_weather_stations_inmet_brazil_1961_2019.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Colo_Production_monthly <- read_excel("Colo_Production_monthly.xlsx")
colo_daily_weather <- read_csv("col_weather_triangle.csv")
colBR_monthly_weather <- read_excel("col_monthly_weather.xlsx")
ws_c0 <- read_delim("CatalogoEstaçõesConvencionais.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
weather_stations_codes0 <- read_csv("weather_stations_codes.csv")
WeatherStationCode <- read_excel("WeatherStationCode.xlsx")
BRA_annual_climate <- read_excel("Annual_climate_Brazil_1901-2021_BRA.xls")
ES <- read_csv("ES2.csv")
Fertilizer_usage <- read_excel("Fertilizer_usage.xlsx")
ES_1984_85 <- read_delim("generatedBy_react-csv (5).csv", 
                         delim = ";", quote = "\\\"", escape_backslash = TRUE, 
                         locale = locale(), trim_ws = TRUE)

#Commodity Data load

Commodity_data <- read_excel("Commodity data.xlsx")
US_Coffee_C_Futures <- read_csv("US Coffee C Futures Historical Data (2).csv")
coffee_future_1970s <- read_csv("coffee-prices-historical-chart-data.csv", 
                                skip = 13)
CF_ICE_KC1 <- read_csv("CHRIS-ICE_KC1 (1).csv")
USDI <- read_csv("US Dollar Index Historical Data.csv")
GBP_USD <- read_csv("GBP_USD Historical Data.csv")
View(Coffee_production_Brazil)
summary(Coffee_production_Brazil)


#Weather Data Preprocessing 
head(Colo_Production_monthly)
summary(colo_daily_weather)
summary(colBR_monthly_weather)
head(colBR_monthly_weather)
col_monthly_weather2=subset(colBR_monthly_weather, select = c(Year,Month, Colombia_pr, colombia_temp_avg))
colo_daily_weathernew=subset(colo_daily_weather, select = c(DATE,STATION, NAME, LATITUDE, LONGITUDE, PRCP,TAVG,TMAX,TMIN,SNWD))
colo_daily_weathernew$DATE <- as.Date(colo_daily_weathernew$DATE, format = "%d/%m/%y")
colo_daily_weathernew$yearmonth <- floor_date(colo_daily_weathernew$DATE, unit = "month")
colo_daily_weathernew$year <- year(colo_daily_weathernew$DATE)
head(colo_daily_weathernew)
ws_c0 <- ws_c0 %>% rename(Name = DC_NOME, Code = 'CD_ESTACAO',
                          Latitude = 'VL_LATITUDE', Longitude = 'VL_LONGITUDE', Altitude = "VL_ALTITUDE")
ws_c0 <- subset(ws_c0,Code==83013)
ws_c0$Latitude <- ws_c0$Latitude/100000000
ws_c0$Longitude <- ws_c0$Longitude/100000000
ws_c0=subset(ws_c0, select = c(Name, Code, Latitude, Longitude, Altitude))
weather_sc0=subset(weather_stations_codes0, select = c(Nome, Código, Latitude, Longitude, Altitude))
weather_sc0 <- weather_sc0 %>% rename(Name = Nome, Code = 'Código')
ES$DATE <- as.Date(paste(ES$DATE, "01", sep = "-"))
weather_sc=subset(WeatherStationCode, select = c(Name, Code, Latitude, Longitude, Altitude))

## Removing null from Colombia Weather Data
colo_daily_weathernew$TAVG[is.na(colo_daily_weathernew$TAVG)] <- 
  (colo_daily_weathernew$TMAX[is.na(colo_daily_weathernew$TAVG)] +
     colo_daily_weathernew$TMIN[is.na(colo_daily_weathernew$TAVG)]) / 2

## Calculating monthly climate data of Colombia
weather_Col_monthly <- colo_daily_weathernew %>%
  group_by(STATION,year, yearmonth,LATITUDE,LONGITUDE,NAME) %>%
  summarise(
    PrecipCol_total = sum(PRCP, na.rm = TRUE),
    SNWDCol_total = sum(SNWD, na.rm = TRUE),
    TmaxCol_mean = mean(TMAX, na.rm = TRUE),
    TminCol_mean = mean(TMIN, na.rm = TRUE),
    TavgCol_mean = mean(TAVG, na.rm = TRUE)
  )

## Calculating yearly climate data of Colombia
weather_Col_yearly <- colo_daily_weathernew %>%
  group_by(STATION, year,LATITUDE,LONGITUDE,NAME) %>%
  summarise(
    PrecipCol_total = sum(PRCP, na.rm = TRUE),
    SNWDCol_total = sum(SNWD, na.rm = TRUE),
    TmaxCol_mean = mean(TMAX, na.rm = TRUE),
    TminCol_mean = mean(TMIN, na.rm = TRUE),
    TavgCol_mean = mean(TAVG, na.rm = TRUE)
  )

# Removing null record if any:
weather_Col_yearly$TavgCol_mean[is.na(weather_Col_yearly$TavgCol_mean)] <- 
  (weather_Col_yearly$TmaxCol_mean[is.na(weather_Col_yearly$TavgCol_mean)] +
     weather_Col_yearly$TminCol_mean[is.na(weather_Col_yearly$TavgCol_mean)]) / 2

## Calculating quarterly climate data of colombia 
weather_Col_monthly$quarter=quarter(weather_Col_monthly$yearmonth)

##Transforming Quarterly Climate data of colombia
head(col_monthly_weather2,12)
weather_br3_monthlylatest <- col_monthly_weather2 %>%
  mutate(quarter = ifelse(Month %in% c('Jan','Feb','Mar'), 'Q1',
                          ifelse(Month %in% c('Apr','May','Jun'), "Q2",
                                 ifelse(Month %in% c('Jul','Aug','Sep'), "Q3",
                                        "Q4"))))

weather_Col_quarterly2 <- weather_br3_monthlylatest %>%
  group_by(Year,quarter) %>%
  summarise(
    PrecipCol_total = sum(Colombia_pr, na.rm = TRUE),
    TavgCol_mean = mean(colombia_temp_avg, na.rm = TRUE)
  )


### Extracting year from colombia daily weather data
colo_daily_weathernew$year <- year(colo_daily_weathernew$DATE)

### Extracting year from Colombia Production data
Colo_Production_monthly$year <- year(Colo_Production_monthly$MonthYear)
head(Colo_Production_monthly)

## Transforming monthly production into colombia yearly data
weather_Col_prod_yearly <- Colo_Production_monthly %>%
  group_by(year) %>%
  summarise(
    Prod_total = sum(Production, na.rm = TRUE)
  )


# Time alignment of Climate Data
weather_Col_prod_yearly2=subset(weather_Col_prod_yearly, year >= "1974" & year <= "2019")

### Quartely data analysing with station
weather_Col_quarterly <- weather_Col_monthly %>%
  group_by(STATION,year,quarter) %>%
  summarise(
    PrecipCol_total = sum(PrecipCol_total, na.rm = TRUE),
    SNWDCol_total = sum(SNWDCol_total, na.rm = TRUE),
    TmaxCol_mean = mean(TmaxCol_mean, na.rm = TRUE),
    TminCol_mean = mean(TminCol_mean, na.rm = TRUE),
    TavgCol_mean = mean(TavgCol_mean, na.rm = TRUE)
  )

### Quartely data analysing without station
weather_Col_quarterly_wostn <- weather_Col_quarterly %>%
  group_by(year,quarter) %>%
  summarise(
    PrecipCol_total = mean(PrecipCol_total, na.rm = TRUE),
    TmaxCol_mean = mean(TmaxCol_mean, na.rm = TRUE),
    SNWDCol_total = mean(SNWDCol_total, na.rm = TRUE),
    TminCol_mean = mean(TminCol_mean, na.rm = TRUE),
    TavgCol_mean = mean(TavgCol_mean, na.rm = TRUE)
  )

### Pivoting weather data
weather_Col_quarterly_new <- weather_Col_quarterly2 %>%
  pivot_wider(
    names_from = quarter,
    values_from = c(3:4)
  )

### Time alignment of quarterly weather data of colombia
weather_Col_quarterly_new_f=subset(weather_Col_quarterly_new, Year >= "1974" & Year <= "2019")

## Merging Production data with quarterly colombia data
Colombia_Prod_year <- merge(weather_Col_quarterly_new_f, weather_Col_prod_yearly2, by.x = "Year",by.y='year', all = TRUE)
head(Colombia_Prod_year)

## Transform Fertiliser Data
Fertilizer_usage2=rename(Fertilizer_usage, c(Fertilizer_consumption_perct_of_prod = 'Fertilizer consumption (% of fertilizer production)',
                                             Fertilizer_consumption_kg_per_ha='Fertilizer consumption (kilograms per hectare of arable land)',
                                             country='Country Name',country_code='Country Code'))

Fertilizer_usage2$Fertilizer_consumption_kg_per_ha <- as.numeric(Fertilizer_usage2$Fertilizer_consumption_kg_per_ha)
Fertilizer_usage2$Fertilizer_consumption_perct_of_prod <- as.numeric(Fertilizer_usage2$Fertilizer_consumption_perct_of_prod)

### Time alignment of Fertiliser Data and Visualisation of Brazil and Colombia fertiliser consumption
Plot_br=Fertilizer_usage2 %>% subset(country=='Brazil' & (Year >= 1974 & Year<=2019)) %>%
  ggplot() + 
  geom_line(aes(x=Year,y=Fertilizer_consumption_kg_per_ha))+ 
  xlab("Years") + 
  ylab("Fertilizer consumption (kg/hectare of arable land)")+
  ggtitle("Fertilizer consumption in Brazil")



plot_col=Fertilizer_usage2 %>% subset(country=='Colombia' & (Year >= 1974 & Year<=2019)) %>%
  ggplot() + 
  geom_line(aes(x=Year,y=Fertilizer_consumption_kg_per_ha))+ 
  xlab("Years") + 
  ylab("Fertilizer consumption (kg/hectare of arable land)")+
  ggtitle("Fertilizer consumption in Colombia")

Plot_br/plot_col

## Filter Colombia fertiliser conumption
Fertilizer_usage2_Col=Fertilizer_usage2 %>% subset(country=='Colombia' & (Year >= 1974 & Year<=2019))

## Merging fertiliser data with production and weather data
Col_Prod_year_fert <- merge(Fertilizer_usage2_Col, Colombia_Prod_year, by.x = "Year", by.y = "Year", all.x = TRUE)

### Removing date and categorical column
Colombia_Prod_year2 = subset(Col_Prod_year_fert,select = -c(1:3))

## Correlation Matrix of Colombia data
corr_plotCol <- ggcorr(Colombia_Prod_year2, method = c("pairwise", "spearman"),label = TRUE, label_size = 3, hjust = 1, layout.exp = 6, low = "#cc0099", high = "black") +
  ggtitle('Correlation Matrix')
corr_plotCol

### Time alignment
weather_Col_quarterly_wostn_f=subset(weather_Col_quarterly2, Year >= "1974" & Year <= "2019")

## Visualisation of Production Data of Colombia
ggplot(weather_Col_prod_yearly2, aes(x=year,y=Prod_total)) + 
  geom_bar(fill='blue',stat = "identity")+
  ylab('Production(1000 bags of 60kg each)')

# Visualisation of Quarterly climate data of colombia
ggplot(weather_Col_quarterly_wostn_f, aes(x = Year, y = TavgCol_mean, group = quarter)) +
  geom_line() +
  facet_wrap(~quarter, ncol = 2, scales = "free") +
  labs(x = "Year", y = "Average Temperature", title = "Average Temperature by quarter") +
  theme_minimal()

# ggplot(weather_Col_quarterly_wostn_f, aes(x = year, y = TminCol_mean, group = quarter)) +
#   geom_line() +
#   facet_wrap(~quarter, ncol = 2, scales = "free") +
#   labs(x = "Year", y = "Minimum Temperature", title = "Minimum Temperature by crop cycle") +
#   theme_minimal()
# 
# ggplot(weather_Col_quarterly_wostn_f, aes(x = year, y = TmaxCol_mean, group = quarter)) +
#   geom_line() +
#   facet_wrap(~quarter, ncol = 2, scales = "free") +
#   labs(x = "Year", y = "Maximum Temperature", title = "Maximum Temperature by crop cycle") +
#   theme_minimal()

ggplot(weather_Col_quarterly_wostn_f, aes(x = Year, y = PrecipCol_total, group = quarter)) +
  geom_line() +
  facet_wrap(~quarter, ncol = 2, scales = "free") +
  labs(x = "Year", y = "Total precipitation", title = "Total precipitation by quarter") +
  theme_minimal()


# Time alignment for Colombia Map 
weather_Col_yearly_1991<- subset(weather_Col_yearly, year == 1991)
weather_Col_yearly_2021<- subset(weather_Col_yearly, year == 2021)

## Colombia state
colombia_states <- st_read("COL_adm1.shp")

colombia_states_sf <- st_as_sf(colombia_states)

### Filtering desired state
filtered_states <- subset(colombia_states_sf, NAME_1 %in% c("Quindío", "Caldas", "Risaralda"))

plot1991<-ggplot() +
  geom_sf(data = colombia_states_sf, aes(geometry = geometry,fill=NAME_1)) +
  geom_point(data = weather_Col_yearly_1991, aes(x = LONGITUDE, y = LATITUDE, color = TavgCol_mean), size = 1) +
  scale_color_gradient(low = "blue", high = "red") +
  #scale_fill_gradient(name = "Avg_temp", low = "blue", high = "red", na.value = "transparent",limits = c(overall_min, overall_max))  +
  theme(axis.text.x=element_text(size=8))+ ggtitle("Map of colombia for Year 1991") +
  labs(subtitle="States", size=8)+
  coord_sf(datum = sf::st_crs("+proj=latlong +datum=WGS84"))
plot1991

plot1991<-ggplot() +
  geom_sf(data = filtered_states, aes(geometry = geometry,fill=NAME_1)) +
  geom_point(data = weather_Col_yearly_1991, aes(x = LONGITUDE, y = LATITUDE, color = TavgCol_mean), size = 1) +
  scale_color_gradient(low = "blue", high = "red") +
  #scale_fill_gradient(name = "Avg_temp", low = "blue", high = "red", na.value = "transparent",limits = c(overall_min, overall_max))  +
  theme(axis.text.x=element_text(size=8))+ ggtitle("Map of colombia for Year 1991") +
  labs(subtitle="States", size=8)+
  coord_sf(datum = sf::st_crs("+proj=latlong +datum=WGS84"))
plot1991

plot2021<-ggplot() +
  geom_sf(data = filtered_states, aes(geometry = geometry,fill=NAME_1)) +
  geom_point(data = weather_Col_yearly_2021, aes(x = LONGITUDE, y = LATITUDE, color = TavgCol_mean), size = 1) +
  scale_color_gradient(low = "blue", high = "red") +
  #scale_fill_gradient(name = "Avg_temp", low = "blue", high = "red", na.value = "transparent",limits = c(overall_min, overall_max))  +
  theme(axis.text.x=element_text(size=8))+ ggtitle("Map of colombia for Year 1991") +
  labs(subtitle="States", size=8)+
  coord_sf(datum = sf::st_crs("+proj=latlong +datum=WGS84"))
plot2021



#Brazil Analysis *************************************************************************

## Adding missing weather station in Brazil
weather_sc2 <- weather_sc %>% 
  ungroup() %>% 
  add_row(ws_c0)

## Transforming Espirito Santos data
ES$year=year(ES$DATE)
ES <- ES %>% rename(Stationid = STATION, date = DATE, PrecipBR = PRCP,
                    Latitude = LATITUDE, Longitude = LONGITUDE,Name=NAME)

#Weather station map
states <- read_state()
states

#Data cleansing missing of ES data
ES_1984_85 <- ES_1984_85 %>% rename(date = '"Data', PrecipBR = '""Chuva [Diaria] (mm)""",',
                                    TmaxBR = '""Temp. Max. [Diaria] (h)""', TminBr = '""Temp. Min. [Diaria] (h)""', TavgBR = '""Temp. [Hora] (C)""')

ES_1984_85_new=subset(ES_1984_85, select = c(date, TmaxBR, TminBr, TavgBR))

## Data Transformation of missing ES data
ES_1984_85_new$date <- as.Date(gsub("\"", "", ES_1984_85_new$date), format = "%d/%m/%Y")

# Convert numeric columns to numeric format
ES_1984_85_new$TmaxBR <- as.numeric(gsub("\"|,", "", ES_1984_85_new$TmaxBR))/10
ES_1984_85_new$TminBr <- as.numeric(gsub("\"|,", "", ES_1984_85_new$TminBr))/10
ES_1984_85_new$TavgBR <- as.numeric(gsub("\"|,", "", ES_1984_85_new$TavgBR))/10
ES_1984_85_new$yearmonth <- floor_date(ES_1984_85_new$date, unit = "month")
ES_1984_85_new$year <- year(ES_1984_85_new$date)

## Calculating Daily average Temperature missing data
ES_1984_85_new_daily <- ES_1984_85_new %>%
  group_by(date,yearmonth,year) %>%
  summarise(
    TmaxBR_mean = mean(TmaxBR, na.rm = TRUE),
    TminBr_mean = mean(TminBr, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR, na.rm = TRUE)
  )

### Removing Null record from Espiritos Santos daily weather data 
ES_1984_85_new_daily$TavgBR_mean[is.na(ES_1984_85_new_daily$TavgBR_mean)] <- 
  (ES_1984_85_new_daily$TmaxBR_mean[is.na(ES_1984_85_new_daily$TavgBR_mean)] +
     ES_1984_85_new_daily$TminBr_mean[is.na(ES_1984_85_new_daily$TavgBR_mean)]) / 2

### Adding missing weather station id for ES data
ES_1984_85_new_daily$Stationid=83013

## Data Cleansing of Brasil weather data
weather_br2 <- weather_br2 %>% rename(Stationid = Estacao, date = Data, PrecipBR = Precipitacao,
                                      TmaxBR = TempMaxima, TminBr = TempMinima, TavgBR = "Temp Comp Media")

## summarise weather data
summary(weather_br2)

## subset of weather Brazil Data
weather_br3=subset(weather_br2, select = c(Stationid, date, PrecipBR, TmaxBR, TminBr, TavgBR))

# Convert the "date" column to date format
weather_br3$date <- as.Date(weather_br3$date, format = "%d/%m/%Y")
weather_br3$PrecipBR <- as.numeric(weather_br3$PrecipBR)
weather_br3$TminBr <- as.numeric(weather_br3$TminBr)

# Create a new column for the year and month
weather_br3$yearmonth <- floor_date(weather_br3$date, unit = "month")
weather_br3$year <- year(weather_br3$date)

# Calculate daily climate data of Brazil
weather_br3_daily <- weather_br3 %>%
  group_by(Stationid,date,yearmonth,year) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR, na.rm = TRUE),
    TminBr_mean = mean(TminBr, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR, na.rm = TRUE)
  )

## Removing Null record from Brazil daily data
weather_br3_daily$TavgBR_mean[is.na(weather_br3_daily$TavgBR_mean)] <- 
  (weather_br3_daily$TmaxBR_mean[is.na(weather_br3_daily$TavgBR_mean)] +
     weather_br3_daily$TminBr_mean[is.na(weather_br3_daily$TavgBR_mean)]) / 2

### Adding missing Espirito Santos daily data 
weather_br3_daily2 <- weather_br3_daily %>% 
  ungroup() %>% 
  add_row(ES_1984_85_new_daily)


## Group the Brazil weather data by stationid and yearmonth
weather_br3_monthly <- weather_br3_daily2 %>%
  group_by(Stationid, yearmonth) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

## Creating phenological coffee cycle from Brazil monthly weather data
weather_br3_monthly$month=month(weather_br3_monthly$yearmonth)
head(weather_br3_monthly)
weather_br3_monthlylatest <- weather_br3_monthly %>%
  mutate(phenology = ifelse(month >= 9 & month <= 11, "blooming",
                            ifelse(month %in% c(12,1,2,3,4,5), "ripening",
                                   "harvesting")))

## derived month from Espiritos santos data
ES$month=month(ES$date)

### Creating phenological coffee cycle from newly added ES monthly weather data
ES_latest <- ES %>%
  mutate(phenology = ifelse(month >= 9 & month <= 11, "blooming",
                            ifelse(month %in% c(12,1,2,3,4,5), "ripening",
                                   "harvesting")))

unique(weather_sc$Name)

#joining weather station for three state

weather_br3_monthlylatest2 <- weather_br3_monthlylatest %>% inner_join( weather_sc2, 
                                                                        by=c('Stationid'='Code'))

#joining weather station for all state
weather_br3_monthlylatest_all <- weather_br3_monthlylatest %>% inner_join( weather_sc0, 
                                                                           by=c('Stationid'='Code'))


## Extracting year from transformed monthly data
weather_br3_monthlylatest2$year=year(weather_br3_monthlylatest2$yearmonth)

## Labled monthly data into monthname
weather_br3_monthly_name <- weather_br3_monthlylatest2 %>%
  mutate(month_name = month(1:12, label = TRUE, abbr = TRUE)[month])
head(weather_br3_monthly_name)

### grouped climate monthly data by monthname 
weather_br3_yearly_mn <- weather_br3_monthly_name %>%
  group_by(Name,year,month_name) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

## Extracting coffee belt's monthly weather data
weather_br3_yearly_mn$State <- ifelse(grepl("- MG", weather_br3_yearly_mn$Name), "Minas Gerais",
                                      ifelse(grepl("- SP", weather_br3_yearly_mn$Name), "São Paulo", "Espírito Santo"))

## grouped monthly weather data state wise
weather_br3_yearly_mn_wostn <- weather_br3_yearly_mn %>%
  group_by(State,year,month_name) %>%
  summarise(
    PrecipBR_total = mean(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

### labeling latest Espiritos santos monthly data as month name
ES_latest_monthly_name <- ES_latest %>%
  mutate(month_name = month(1:12, label = TRUE, abbr = TRUE)[month])

### labeling state name
ES_latest_monthly_name$State <- "Espírito Santo"

### Grouped Espiritos Santo by month name
ES_latest_monthly_name_wostn <- ES_latest_monthly_name %>%
  group_by(State,year,month_name) %>%
  summarise(
    PrecipBR_total = mean(PrecipBR, na.rm = TRUE))

## Added Espiritos santos data to the original monthly weather data
weather_br3_yearly_mn_wostn2 <- weather_br3_yearly_mn_wostn %>% 
  ungroup() %>% 
  add_row(ES_latest_monthly_name_wostn)

## Grouped Brazil monthly weather data year and month wise
weather_br3_yearly_mn_wostn3 <- weather_br3_yearly_mn_wostn2 %>%
  group_by(State,year,month_name) %>%
  summarise(
    PrecipBR_total = mean(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )


## Extract required feature like Average Temperature and Total Precipitation from Brazily monthly data
weather_br3_yearly_mn_wostn3_TP <- subset(weather_br3_yearly_mn_wostn3,
                                          select=c(State,year,month_name,PrecipBR_total,TavgBR_mean))

## Pivoting monthly weather data to yearly, month as a column name
weather_br3_yearly_mn_new <- weather_br3_yearly_mn_wostn3_TP %>%
  pivot_wider(
    names_from = month_name,
    values_from = c(4:5)
  )


### Extracting Minas Greais monthly weather data from Brazil weather data
weather_br3_monthlylatest2_MG <- weather_br3_monthlylatest2[grepl("MG", weather_br3_monthlylatest2$Name, fixed = TRUE), ]

### Grouped MG mothly weather data by year and phenological coffee cycle
weather_br3_phenology_MG <- weather_br3_monthlylatest2_MG %>%
  group_by(Stationid,year,phenology) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

### Grouped MG mothly weather data by year and phenological coffee cycle without station
weather_br3_phenology_MG_wostn <- weather_br3_phenology_MG %>%
  group_by(year,phenology) %>%
  summarise(
    PrecipBR_total = mean(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

### Pivoting Minas Greais pheological coffee cycle into column
weather_br3_phenology_MGNew <- weather_br3_phenology_MG_wostn %>%
  pivot_wider(
    names_from = phenology,
    values_from = c(3:6)
  )

### Time Alignment of MG coffee data
weather_br3_phenology_MGNew2=subset(weather_br3_phenology_MGNew,year >= "1974" & year <= "2019")

### Time Alignment of MG coffee data without station
weather_br3_phenology_MGNew_f=subset(weather_br3_phenology_MG_wostn,year >= "1974" & year <= "2019")

### Visualisation of MG Average Temp Data
ggplot(weather_br3_phenology_MGNew_f, aes(x = year, y = TavgBR_mean, group = phenology)) +
  geom_line() +
  facet_wrap(~phenology, ncol = 2, scales = "free") +
  labs(x = "Year", y = "Average Annual Temperature", title = "Average Annual Temperature by Coffee phenological stage") +
  theme_minimal()

### Visualisation of MG Total Precipitation Data
ggplot(weather_br3_phenology_MGNew_f, aes(x = year, y = PrecipBR_total, group = phenology)) +
  geom_line() +
  facet_wrap(~phenology, ncol = 2, scales = "free") +
  labs(x = "Year", y = "Total Annual precipitation (mm)", title = "Total Annual precipitation by Coffee phenological stage") +
  theme_minimal()

### Extracting Sao Paulo data from Brazil monthly weather data
weather_br3_monthlylatest2_SP <- weather_br3_monthlylatest2[grepl("- SP", weather_br3_monthlylatest2$Name, fixed = TRUE), ]

### Grouped Sao paulo yearly weather data by phenological coffee cycle
weather_br3_phenology_SP <- weather_br3_monthlylatest2_SP %>%
  group_by(Stationid,year,phenology) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

### Grouped Sao paulo yearly weather data by phenological coffee cycle without station
weather_br3_phenology_SP_wostn <- weather_br3_phenology_SP %>%
  group_by(year,phenology) %>%
  summarise(
    PrecipBR_total = mean(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

### Pivoting Sao Paulo pheological coffee cycle into column
weather_br3_phenology_SPNew <- weather_br3_phenology_SP_wostn %>%
  pivot_wider(
    names_from = phenology,
    values_from = c(3:6)
  )

### Time alignment of SP data
weather_br3_phenology_SPNew2=subset(weather_br3_phenology_SPNew,year >= "1974" & year <= "2019")

### Time alignment of SP data without station
weather_br3_phenology_SPNew_f=subset(weather_br3_phenology_SP_wostn,year >= "1974" & year <= "2019")

### Visualisation of SP Average Temp Data
ggplot(weather_br3_phenology_SPNew_f, aes(x = year, y = TavgBR_mean, group = phenology)) +
  geom_line() +
  facet_wrap(~phenology, ncol = 2, scales = "free") +
  labs(x = "Year", y = "Average Annual Temperature", title = "Average Annual Temperature by Coffee phenological stage") +
  theme_minimal()

### Visualisation of SP Total Precipitation Data
ggplot(weather_br3_phenology_SPNew_f, aes(x = year, y = PrecipBR_total, group = phenology)) +
  geom_line() +
  facet_wrap(~phenology, ncol = 2, scales = "free") +
  labs(x = "Year", y = "Total Annual precipitation (mm)", title = "Total Annual precipitation by Coffee phenological stage") +
  theme_minimal()



### Extracting Espiritos Santos weather data from Brazil weather dataset
weather_br3_monthlylatest2_ES <- weather_br3_monthlylatest2[grepl("- ES$|^MUNIZ FREIRE$", weather_br3_monthlylatest2$Name), ]

### Grouped ES yearly weather data by phenological coffee cycle
weather_br3_phenology_ES <- weather_br3_monthlylatest2_ES %>%
  group_by(Stationid,year,phenology) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

### Grouped newly added ES data by phenological coffee cycle
ES_latest_phenology <- ES_latest %>%
  group_by(Stationid,year,phenology) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR, na.rm = TRUE) )

### Adding newly added ES data to the exisitng ES data in order to compensate missing value
ES_latest_phenology$Stationid <- as.numeric(ES_latest_phenology$Stationid)
weather_br3_phenology_ES2 <- weather_br3_phenology_ES %>% 
  ungroup() %>% 
  add_row(ES_latest_phenology)


### Grouped ES yearly weather data by phenological coffee cycle without station
weather_br3_phenology_ES_wostn <- weather_br3_phenology_ES2 %>%
  group_by(year,phenology) %>%
  summarise(
    PrecipBR_total = mean(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

### Pivoting ES pheological coffee cycle into column 
weather_br3_phenology_ESNew <- weather_br3_phenology_ES_wostn %>%
  pivot_wider(
    names_from = phenology,
    values_from = c(3:6)
  )

### Time Alignment of ES yearly weather data
weather_br3_phenology_ESNew2=subset(weather_br3_phenology_ESNew,year >= "1974" & year <= "2019")


### Time Alignment of ES yearly weather data without station
weather_br3_phenology_ESNew_f=subset(weather_br3_phenology_ES_wostn,year >= "1974" & year <= "2019")

### Visualisation of SP Average Temp Data
ggplot(weather_br3_phenology_ESNew_f, aes(x = year, y = TavgBR_mean, group = phenology)) +
  geom_line() +
  facet_wrap(~phenology, ncol = 2, scales = "free") +
  labs(x = "Year", y = "Average Temperature", title = "Average Temperature by crop cycle") +
  theme_minimal()

### Visualisation of SP Total Precipitation Data
ggplot(weather_br3_phenology_ESNew_f, aes(x = year, y = PrecipBR_total, group = phenology)) +
  geom_line() +
  facet_wrap(~phenology, ncol = 2, scales = "free") +
  labs(x = "Year", y = "Total precipitation", title = "Total precipitation by crop cycle") +
  theme_minimal()


weather_br3_phenology <- weather_br3_monthlylatest2 %>%
  group_by(Stationid,year,phenology) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

## Extracting Brazil's coffee belt production data
Coffee_production_Brazilnew <- subset(Coffee_production_Brazil,State %in% c("Minas Gerais","Espírito Santo","São Paulo") )

## Cleansing of Brazil total production data
Coffee_production_Br3 <- Coffee_production_Brazilnew %>%
  mutate(`Quantity(tones)` = as.numeric(`Quantity(tones)`)) %>%
  group_by(Year,State) %>%
  summarise(
    production_total = sum(`Quantity(tones)`, na.rm = TRUE)
  )
head(Coffee_production_Br3)

### Extracting ES total Production from Brazil production data
Coffee_production_Br3_ES <- subset(Coffee_production_Br3,State %in% c("Espírito Santo") )
Coffee_production_Br3_ES_f <- subset(Coffee_production_Br3_ES,Year >= "1974" & Year <= "2019")

### Visualisation of ES total production
ggplot(Coffee_production_Br3_ES_f, aes(x=Year,y=production_total)) + 
  geom_bar(fill='blue',stat = "identity")+
  ylab('Production(in tons)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Extracting MG total Production from Brazil production data
Coffee_production_Br3_MG <- subset(Coffee_production_Br3,State %in% c("Minas Gerais") )
Coffee_production_Br3_MG_f <- subset(Coffee_production_Br3_MG,Year >= "1974" & Year <= "2019")

### Visualisation of MG total production
ggplot(Coffee_production_Br3_MG_f, aes(x=Year,y=production_total)) + 
  geom_bar(fill='blue',stat = "identity")+
  ylab('Production(in tons)') +
  ggtitle("Yearly Coffee Production in Minas Gerais") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Extracting SP total Production from Brazil production data
Coffee_production_Br3_SP <- subset(Coffee_production_Br3,State %in% c("São Paulo") )
Coffee_production_Br3_SP_f <- subset(Coffee_production_Br3_SP,Year >= "1974" & Year <= "2019")

### Visualisation of SP total production
ggplot(Coffee_production_Br3_SP_f, aes(x=Year,y=production_total)) + 
  geom_bar(fill='blue',stat = "identity")+
  ylab('Production(in tons)') +
  ggtitle("Yearly Coffee Production in Sao Paulo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### labeling state name of coffee belt
weather_br3_phenology_ESNew2$State='Espírito Santo'
weather_br3_phenology_SPNew2$State='São Paulo'
weather_br3_phenology_MGNew2$State='Minas Gerais'

### Binding all three states weather data together
weather_br3_phenologyall <- bind_rows(weather_br3_phenology_MGNew2, weather_br3_phenology_SPNew2, weather_br3_phenology_ESNew2)

### Merge the binded date with production data based on state and year
Brazil_Prod_year <- merge(weather_br3_phenologyall, Coffee_production_Br3, by.x = c("year",'State'),by.y=c('Year','State'), all = TRUE)

### calculate perivious year lag of blooming phase
Brazil_Prod_yearNew <- Brazil_Prod_year %>%
  arrange(year) %>%
  mutate(
    PrecipBR_total_blooming_PY = lag(PrecipBR_total_blooming),
    TmaxBR_mean_blooming_PY = lag(TmaxBR_mean_blooming),
    TminBr_mean_blooming_PY = lag(TminBr_mean_blooming),
    TavgBR_mean_blooming_PY = lag(TavgBR_mean_blooming)
    
  )

### removing Null if any
Brazil_Prod_yearNew=Brazil_Prod_yearNew[complete.cases(Brazil_Prod_yearNew),]

### Preparing data for correlation plot
Brazil_Prod_year3=subset(Brazil_Prod_yearNew,select=c(year,production_total,TavgBR_mean_harvesting,TavgBR_mean_ripening,
                                                      TavgBR_mean_blooming,PrecipBR_total_blooming,PrecipBR_total_harvesting,
                                                      PrecipBR_total_ripening,PrecipBR_total_blooming_PY,TavgBR_mean_blooming_PY
))

### Removing Year column from Brazil data
Brazil_Prod_year33 = subset(Brazil_Prod_year3,select = -c(year))

### Correlation of Brazil total production and phenological weather data
corr_plotBRp <- ggcorr(Brazil_Prod_year33, label = TRUE, label_size = 3, hjust = 1, layout.exp = 6, low = "#cc0099", high = "black") +
  ggtitle('Correlation Matrix')
corr_plotBRp

### Time alignment of monthly data where monthname as a column name
weather_br3_yearly_mn_new <- subset(weather_br3_yearly_mn_new,year >= "1974" & year <= "2019")
Brazil_Prod_year2 <- merge(weather_br3_yearly_mn_new, Coffee_production_Br3, by.x = c("year",'State'),by.y=c('Year','State'), all = TRUE)

### removing year from the data
Brazil_Prod_year22 = subset(Brazil_Prod_year2,select = -c(year,State))

### Correlation of Brazil total production and Monthly weather data
corr_plotBRm <- ggcorr(Brazil_Prod_year22, method = c("pairwise", "spearman"),label = TRUE, label_size = 3, hjust = 1, layout.exp = 6, low = "#cc0099", high = "black") +
  ggtitle('Correlation Matrix')
corr_plotBRm


#Line graph

weather_br3_monthlylatest2_f=subset(weather_br3_monthlylatest2,yearmonth >= "1974-01-01" & yearmonth <= "2019-12-01")

weather_br3_monthlylatest2_f_wostn <- weather_br3_monthlylatest2_f %>%
  group_by(yearmonth) %>%
  summarise(
    PrecipBR_total = mean(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

head(weather_br3_monthlylatest2_f_wostn)
summary(weather_br3_monthlylatest2_f_wostn)



#yearly Brazil weather data with lat,long and selected station in three states of Coffee belt
weather_br3_yearlynew <- weather_br3_monthlylatest2 %>%
  group_by(Stationid, year,Latitude,Longitude,Name) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

#Yearly Brazil weather data with lat, long and for all station in Brasil
weather_br3_monthlylatest_all$year= year(weather_br3_monthlylatest_all$yearmonth)
weather_br3_yearlynew_all <- weather_br3_monthlylatest_all %>%
  group_by(Stationid, year,Latitude,Longitude,Name) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR_total, na.rm = TRUE),
    TmaxBR_mean = mean(TmaxBR_mean, na.rm = TRUE),
    TminBr_mean = mean(TminBr_mean, na.rm = TRUE),
    TavgBR_mean = mean(TavgBR_mean, na.rm = TRUE)
  )

### yearly ES weather data with lat,long and station
ES_yearly<- ES %>%
  group_by(Stationid, year,Latitude,Longitude,Name) %>%
  summarise(
    PrecipBR_total = sum(PrecipBR, na.rm = TRUE))

### Extracting necessary features from weather data
ES_yearly<-subset(ES_yearly,select=c(year,Latitude,Longitude,Name,PrecipBR_total))

### Adding ES data to Brasil existing dataset for selected station in three states of coffee belt
weather_br3_yearlynew2 <- weather_br3_yearlynew %>% 
  ungroup() %>% 
  add_row(ES_yearly)

### Adding ES data to Brasil existing dataset for all stations in Brasil
weather_br3_yearlynew_all2 <- weather_br3_yearlynew_all %>% 
  ungroup() %>% 
  add_row(ES_yearly)



## Time alignment of Brasil weather data for all stations in Brasil
weather_br3_1991<- subset(weather_br3_yearlynew_all2, year == '1991') 
weather_br3_2016<- subset(weather_br3_yearlynew_all2, year == '2016')

weather_br3_1991$TavgBR_mean <- as.numeric(weather_br3_1991$TavgBR_mean)
weather_br3_2016$TavgBR_mean <- as.numeric(weather_br3_2016$TavgBR_mean)

## Calculating Temp range
overall_min <- min(c(weather_br3_1991$TavgBR_mean, weather_br3_2016$TavgBR_mean), na.rm = TRUE)
overall_max <- max(c(weather_br3_1991$TavgBR_mean, weather_br3_2016$TavgBR_mean), na.rm = TRUE)

## Preparing Brasil map visuals
class(states)
head(states)
Brazil_states <- as.data.frame(states)
filtered_states_Br <- subset(Brazil_states, name_state %in% c("Minas Gerais", "Espirito Santo", "São Paulo"))
filtered_states_sf <- sf::st_as_sf(filtered_states_Br)

## Extract the state label coordinates
filtered_states_centroids <- sf::st_centroid(filtered_states_sf)
label_coords <- sf::st_coordinates(filtered_states_centroids)

## Create a data frame with the coordinates and state abbreviations
label_df <- data.frame(x = label_coords[, "X"], y = label_coords[, "Y"], abbrev_state = filtered_states_sf$abbrev_state)
label_df$x <- as.numeric(label_df$x)
label_df$y <- as.numeric(label_df$y)

## Visualisation of Brasil temp data of 1991 in map
plot_1991 <- ggplot() +
  geom_sf(data=states, fill ="#2D3E50", color="#FEBF57", size=.15,show.legend=FALSE) +
  geom_point(data = weather_br3_1991, aes(x = Longitude, y = Latitude,color=TavgBR_mean),  size = 1.5) +
  geom_text(data = label_df, aes(label = abbrev_state,x = x, y = y), size = 2.5, color = "white") +
  scale_colour_viridis_c(name="Avg Temperature (deg Celsius)", option="C",limits = c(overall_min, overall_max),na.value = "transparent") +
  theme(axis.text.x=element_text(size=8))+ ggtitle("Brazil Temperature 1991") +
  labs(subtitle="States", size=8) +
  theme_minimal()+
  coord_sf(datum = sf::st_crs("+proj=latlong +datum=WGS84"))

## Visualisation of Brasil temp data of 2016 in map
plot_2016 <- ggplot() +
  geom_sf(data = states, fill = "#2D3E50", color = "#FEBF57", size = 0.15, show.legend = FALSE) +
  geom_point(data = weather_br3_2016, aes(x = Longitude, y = Latitude, color = TavgBR_mean), size = 1.5) +
  geom_text(data = label_df, aes(label = abbrev_state, x = x, y = y), size = 2.5, color = "white") +
  scale_colour_viridis_c(name = "Avg Temperature (deg Celsius) ", option = "C", limits = c(overall_min, overall_max), na.value = "transparent") +
  theme(axis.text.x = element_text(size = 8)) +
  ggtitle("Brazil Temperature 2016") +
  labs(subtitle = "States", size = 8) +
  theme_minimal() +
  coord_sf(datum = sf::st_crs("+proj=latlong +datum=WGS84"))


# Arrange the plots vertically
combined_plot <- plot_1991 / plot_2016

# Display the combined plot
combined_plot

## Calculating Precip range
overall_minp <- min(c(weather_br3_1991$PrecipBR_total, weather_br3_2016$PrecipBR_total), na.rm = TRUE)
overall_maxp <- max(c(weather_br3_1991$PrecipBR_total, weather_br3_2016$PrecipBR_total), na.rm = TRUE)

## Visualisation of Brasil precipitation data of 1991 in map
plot_1991p <- ggplot() +
  geom_sf(data=states, fill="#2D3E50", color="#FEBF57", size=.15) +
  geom_point(data = weather_br3_1991, aes(x = Longitude, y = Latitude,color=PrecipBR_total),  size = 1) +
  geom_text(data = label_df, aes(label = abbrev_state, x = x, y = y), size = 2.5, color = "white") +
  scale_colour_viridis_c(name="Precipitation Total (mm)", option="C",limits = c(overall_minp, overall_maxp),direction=-1,na.value = "transparent") +
  theme(axis.text.x=element_text(size=8))+ ggtitle("Brazil Precipitation 1991") +
  labs(subtitle="States", size=8,color="Precipitation Total (mm)") +
  theme_minimal()+
  coord_sf(datum = sf::st_crs("+proj=latlong +datum=WGS84"))

## Visualisation of Brasil precipitation data of 2016 in map
plot_2016p <- ggplot() +
  geom_sf(data=states, fill="#2D3E50", color="#FEBF57", size=.15) +
  geom_point(data = weather_br3_2016, aes(x = Longitude, y = Latitude,color=PrecipBR_total),size = 1) +
  geom_text(data = label_df, aes(label = abbrev_state, x = x, y = y), size = 2.5, color = "white") +
  scale_colour_viridis_c(name="Precipitation Total (mm)", option="C",limits = c(overall_minp, overall_maxp),direction=-1,na.value = "transparent") +
  theme(axis.text.x=element_text(size=8))+ ggtitle("Brazil Precipitation 2016") +
  labs(subtitle="States", size=8,color="Precipitation Total (mm)") +
  theme_minimal()+
  coord_sf(datum = sf::st_crs("+proj=latlong +datum=WGS84"))

# Arrange the plots vertically
combined_plotp <- plot_1991p / plot_2016p

# Displaying the combined plot
combined_plotp

### merge Brasil Production data with the states of Brazil
brazil_states_production <- merge(states, Coffee_production_Brazil, by.x = "code_state", by.y = "Code", all.x = TRUE)
brazil_states_productionNew <- as.data.frame(brazil_states_production)

### remove null record
brazil_states_productionNew <- na.omit(brazil_states_productionNew)

### renaming column name
brazil_states_productionNew=rename(brazil_states_productionNew, Production_total = 'Quantity(tones)')

### Extracting 1991 and 2016 production data
brazil_states_productionNew_1991<- subset(brazil_states_productionNew, Year == '1991')
brazil_states_productionNew_2016<- subset(brazil_states_productionNew, Year == '2016')
brazil_states_productionNew_1991$Production_total <- as.numeric(brazil_states_productionNew_1991$Production_total)
brazil_states_productionNew_2016$Production_total <- as.numeric(brazil_states_productionNew_2016$Production_total)

### Visualising 1991 coffee production in Brasil map
plot1991_prod<-ggplot() +
  geom_sf(data = brazil_states_productionNew_1991, aes(geometry = geometry,fill = Production_total)) +
  scale_fill_gradient(name = "Production(in tons)", 
                      low = "yellow", high = "brown", 
                      na.value = "transparent",
                      limits = c(1,2000000),
                      labels = function(x) format(x, scientific = FALSE))  +
  geom_text(data = label_df, aes(label = abbrev_state, x = x, y = y), size = 2.5, color = "white") +
  theme(axis.text.x=element_text(size=8))+ ggtitle("Map of Brazil for Year 1991") +
  labs(subtitle="States", size=8,x='Longitude',y='Latitude')

### Visualising 2016 coffee production in Brasil map
plot2016_prod<-ggplot() +
  geom_sf(data = brazil_states_productionNew_2016, aes(geometry = geometry,fill = Production_total)) +
  scale_fill_gradient(name = "Production(in tons)", 
                      low = "yellow", high = "brown", 
                      na.value = "transparent",
                      limits = c(1,2000000),
                      labels = function(x) format(x, scientific = FALSE))  +
  geom_text(data = label_df, aes(label = abbrev_state, x = x, y = y), size = 2.5, color = "white") +
  theme(axis.text.x=element_text(size=8))+ ggtitle("Map of Brazil for Year 2016") +
  labs(subtitle="States", size=8,x='Longitude',y='Latitude')

# Arrange the plots vertically
combined_plot_prod <- plot1991_prod / plot2016_prod
# Displaying combined plot
combined_plot_prod

### Filter Brasil fertiliser consumption
Fertilizer_usage2_BR=Fertilizer_usage2 %>% subset(country=='Brazil' & (Year >= 1974 & Year<=2019))

### merging Brasil Fertiliser data with production and weather data
Brazil_Prod_year_fert3 <- merge(Fertilizer_usage2_BR, Brazil_Prod_year3, by.x = "Year", by.y = "year", all.x = TRUE)

### Preparing Dataset for Correlation matrix
Brazil_Prod_year_Fert = subset(Brazil_Prod_year_fert3,select = c(production_total,TavgBR_mean_harvesting,TavgBR_mean_ripening,
                                                                 TavgBR_mean_blooming,PrecipBR_total_blooming,PrecipBR_total_harvesting,
                                                                 PrecipBR_total_ripening,PrecipBR_total_blooming_PY,TavgBR_mean_blooming_PY,
                                                                 Fertilizer_consumption_perct_of_prod,Fertilizer_consumption_kg_per_ha
))

### Correlation matrix of Brasil data
corr_plotBR <- ggcorr(Brazil_Prod_year_Fert, method=c('pairwise','spearman'),label = TRUE, label_size = 3, hjust = 1, layout.exp = 6, low = "green", high = "darkgreen") +
ggtitle('Correlation Matrix')
corr_plotBR



#Coffee Future Preprocessing

CF_ICE_KC1$Year <- format(CF_ICE_KC1$Date, "%Y")
CF_ICE_KC1$Month <- format(CF_ICE_KC1$Date, "%m")
CF_ICE_KC1$Date <- floor_date(CF_ICE_KC1$Date, unit = "month")
CF_ICE_KC1_new=CF_ICE_KC1 %>% subset(Year >= 1974 & Year<=1979)
CF_ICE_KC1_new2 = subset(CF_ICE_KC1_new,select = c("Date","Settle"))
CF_ICE_KC1_new2=rename(CF_ICE_KC1_new2, c(Price='Settle'))
CF_ICE_KC1_new2= CF_ICE_KC1_new2 %>% arrange(CF_ICE_KC1_new2$Date)

## Correcting commodity date format
US_Coffee_C_Futures$Date <- as.Date(US_Coffee_C_Futures$Date,format = "%d/%m/%Y")
USDI$Date <- as.Date(USDI$Date,format = "%d/%m/%Y")
GBP_USD$Date <- as.Date(GBP_USD$Date,format = "%d/%m/%Y")
US_Coffee_C_Futures2=US_Coffee_C_Futures %>% subset(Date > '1979-12-01')

### appendinf missing future data
US_Coffee_C_Futures3 <- US_Coffee_C_Futures2 %>% add_row(CF_ICE_KC1_new2, 
                                                         .before=1)

### Extracting date and price from commodity data
US_Coffee_C_Futures_new = subset(US_Coffee_C_Futures3,select = c("Date","Price"))
USDI_new = subset(USDI,select = c("Date","Price"))
GBP_USD_new = subset(GBP_USD,select = c("Date","Price"))

### Renaming column name
US_Coffee_C_Futures_new=rename(US_Coffee_C_Futures_new, c(COFFEE_FUTURE='Price'))
USDI_new=rename(USDI_new, c(USDI='Price'))
GBP_USD_new=rename(GBP_USD_new, c(GBP_USD='Price'))

### Commodity data transformation
Commodity_data <- Commodity_data %>% 
  mutate_at(vars(CRUDE_OIL_AVG, COCOA, COFFEE_ARABIC,TEA_AVG,SUGAR_WLD,UREA_EE_BULK,DAP), as.numeric)

Commodity_data$YearMonth <- ymd(paste0(Commodity_data$YearMonth, '01'))

### Removing null record
Commodity_data=Commodity_data[complete.cases(Commodity_data),]

summary(Commodity_data)

# Conversion factor from usd/kg to usd/lbs
conversion_factor <- 2.20462  # 1 kilogram = 2.20462 pounds

# Convert specific columns from usd/kg to usd/lbs
Commodity_data <- Commodity_data %>%
  mutate(across(c(3:6), ~ as.numeric(.) / conversion_factor))


### Time alignment of data
Commodity_data_New<- subset(Commodity_data, YearMonth >= '1974-01-01')
Commodity_data_New=rename(Commodity_data_New, c(COFFEE_SPOT='COFFEE_ARABIC'))

### merging Future data with commodity market indicator
Commodity_data_New0 <- merge(US_Coffee_C_Futures_new, Commodity_data_New, by.x = "Date", by.y = "YearMonth", all.x = TRUE)
Commodity_data_New1 <- merge(USDI_new, Commodity_data_New0, by.x = "Date", by.y = "Date", all.x = TRUE)
Commodity_data_New3 <- merge(GBP_USD_new, Commodity_data_New1, by.x = "Date", by.y = "Date", all.x = TRUE)

### Remove null record if any
Commodity_data_New3=Commodity_data_New3[complete.cases(Commodity_data_New3),]

### converting commodity data into time series
Commodity_data_New2 = subset(Commodity_data_New3,select = -c(Date))
Commodity_data_ts <- ts(Commodity_data_New2,frequency = 12,
                        start = c(1974, 1))


### plotting time series data
plot(Commodity_data_ts)

#Time series decomposition
dcmp = stl(Commodity_data_ts[,'COFFEE_FUTURE'], s.window='periodic')
dcmp %>% autoplot() + ggtitle("Coffee future STL Decomposition")


### correlation matrix of commodity data with coffee future
Commodity_data_New2 %>%
  as.data.frame() %>%
  GGally::ggpairs()

#TS Linear Regression model*************************************************************

### adf TEST commodity time series to check stationarity
adf_test0<- apply(Commodity_data_ts, 2, function(column) {
  adf.test(column, k = 20)
})

results_adf0_diff <- data.frame(Variable = character(), Test_Statistic = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through the list and extract the relevant information
for (i in seq_along(adf_test0)) {
  results_adf0_diff <- rbind(results_adf0_diff, c(names(adf_test0)[i], adf_test0[[i]]$statistic, adf_test0[[i]]$p.value))
}

# Rename the columns for clarity
colnames(results_adf0_diff) <- c("Variable", "Test_Statistic", "P_Value")

# Print the table
print(results_adf0_diff)

### KPSS test of commodity time series for stationarity
z=apply(Commodity_data_ts, 2, kpss.test)
results_df <- data.frame(Variable = character(), Test_Statistic = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through the list and extract the relevant information
for (i in seq_along(z)) {
  results_df <- rbind(results_df, c(names(z)[i], z[[i]]$statistic, z[[i]]$p.value))
}

# Rename the columns for clarity
colnames(results_df) <- c("Variable", "Test_Statistic", "P_Value")

# Print the table
print(results_df)

kpss.test(Commodity_data_ts[,'COFFEE_FUTURE'],null='Level',lshort = FALSE)
summary(z)

### Differencing commodity time series to make it stationary
commodity_stnry = diffM(Commodity_data_ts)#difference operation on a vector of time series. Default order of differencing is 1.
commodity_data_diff=ts(commodity_stnry,frequency = 12,
                       start = c(1974, 2))


### Plotting differenced Time series data
plot(commodity_data_diff)

### ADF test of differenced commodity time series for stationarity
adf_result <- apply(commodity_data_diff, 2, adf.test)

results_adf_diff <- data.frame(Variable = character(), Test_Statistic = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through the list and extract the relevant information
for (i in seq_along(adf_result)) {
  results_adf_diff <- rbind(results_adf_diff, c(names(adf_result)[i], adf_result[[i]]$statistic, adf_result[[i]]$p.value))
}

# Rename the columns for clarity
colnames(results_adf_diff) <- c("Variable", "Test_Statistic", "P_Value")

# Print the table
print(results_adf_diff)

### KPSS test of differenced commodity time series for stationarity
z2<-apply(commodity_data_diff, 2, kpss.test)
results_df_diff <- data.frame(Variable = character(), Test_Statistic = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through the list and extract the relevant information
for (i in seq_along(z2)) {
  results_df_diff <- rbind(results_df_diff, c(names(z2)[i], z2[[i]]$statistic, z2[[i]]$p.value))
}

# Rename the columns for clarity
colnames(results_df_diff) <- c("Variable", "Test_Statistic", "P_Value")

# Print the table
print(results_df_diff)

### converting time series into data frame
CF_df_ts <- as.data.frame(commodity_stnry, as.numeric(time(Commodity_data_ts)))

CF_ts <- ts(CF_df_ts$COFFEE_FUTURE,frequency = 12,
            start = c(1974, 2))

commodity_data_diff_df <- as.data.frame(commodity_data_diff)

### relocating coffee future column for correlation matrix
dependent_var_index <- 'COFFEE_FUTURE'
dependent_var_column <- which(names(commodity_data_diff_df) == dependent_var_index)

commodity_data_diff2 <- commodity_data_diff_df %>%
  relocate(dependent_var_index, .before = everything())

commodity_data_diff2 <- as.ts(commodity_data_diff2)


# Create a correlation plot matrix using GGally::ggpairs()
commodity_data_diff2 %>%
  as.data.frame() %>%
  GGally::ggpairs()

summary(commodity_data_diff2)

skewness(commodity_data_diff2[,'COFFEE_FUTURE'])

### Cross correlation of commodity market indicator agaisnt coffee future on different lag
ccf_plot(commodity_data_diff2[,'USDI'],commodity_data_diff2[,'COFFEE_FUTURE'],
         title='USDI vs Coffee Future on different lag',lags=-10:10)
ccf_plot(commodity_data_diff2[,'GBP_USD'],commodity_data_diff2[,'COFFEE_FUTURE'],
         title='GBP_USD vs Coffee Future on different lag',lags=-10:10)
ccf_plot(commodity_data_diff2[,'COCOA'],commodity_data_diff2[,'COFFEE_FUTURE'],
         title='COCOA vs Coffee Future on different lag',lags=-10:10)
ccf_plot(commodity_data_diff2[,'COFFEE_SPOT'],commodity_data_diff2[,'COFFEE_FUTURE'],
         title='COFFEE_SPOT vs Coffee Future on different lag',lags=-10:10)
ccf_plot(commodity_data_diff2[,'TEA_AVG'],commodity_data_diff2[,'COFFEE_FUTURE'],
         title='TEA_AVG vs Coffee Future on different lag',lags=-10:10)
ccf_plot(commodity_data_diff2[,'SUGAR_WLD'],commodity_data_diff2[,'COFFEE_FUTURE'],
         title='SUGAR vs Coffee Future on different lag',lags=-10:10)
ccf_plot(commodity_data_diff2[,'UREA_EE_BULK'],commodity_data_diff2[,'COFFEE_FUTURE'],
         title='UREA vs Coffee Future on different lag',lags=-10:10)
ccf_plot(commodity_data_diff2[,'DAP'],commodity_data_diff2[,'COFFEE_FUTURE'],
         title='DAP vs Coffee Future on different lag',lags=-10:10)
ccf_result <- ccf(commodity_data_diff2[,'CRUDE_OIL_AVG'],commodity_data_diff2[,'COFFEE_FUTURE'])

# Print the cross-correlation result
print(ccf_result)
cross_correlation_values <- ccf_result$acf

# Calculate the number of observations
n <- length(commodity_data_diff2[,'CRUDE_OIL_AVG'])

# Calculate the standard error of the cross-correlation
standard_error <- 1 / sqrt(n)

# Calculate the t-statistic
t_statistic <- cross_correlation_values / standard_error

# Calculate the degrees of freedom
degrees_of_freedom <- n - 2  # Assuming you're calculating Pearson's correlation

# Calculate the two-sided p-values
p_values <- 2 * pt(-abs(t_statistic), df = degrees_of_freedom)

# Print the cross-correlation values and associated p-values
print(data.frame(CrossCorrelation = cross_correlation_values, PValue = p_values))

#Splitting train and test data for timeseries linear regression model
#Data splitting
h2 <- 17L
traintslm <- head(CF_df_ts, round(nrow(CF_df_ts) - h2))
CF_tslm<-ts(traintslm$COFFEE_FUTURE,frequency=12,start=c(1974,2))
tail(CF_tslm,1)
testtslm <- tail(CF_df_ts, h2)
test_tslm<-ts(testtslm$COFFEE_FUTURE,frequency=12,start=c(2022,1))
tail(test_tslm)
head(traintslm)
# **************************************************************************
# Create a new data frame with the time series train data and additional predictor variables
CF_dataTSLM <- data.frame(
  CF = CF_tslm,
  GBP_USD = traintslm$GBP_USD,
  USDI = traintslm$USDI,
  CRUDE_OIL_AVG = traintslm$CRUDE_OIL_AVG,
  COCOA = traintslm$COCOA,
  COFFEE_SPOT = traintslm$COFFEE_SPOT,
  TEA_AVG = traintslm$TEA_AVG,
  SUGAR_WLD = traintslm$SUGAR_WLD,
  UREA_EE_BULK = traintslm$UREA_EE_BULK,
  DAP = traintslm$DAP
)

### time series linear regression model started with all feature reduced to only two feature

## Best subset regression to calculate best predictor based on AIC
CF_fittrn2 <- tslm(
  CF ~ .,
  data=CF_dataTSLM,biasadj = T)

summary(CF_fittrn2)

tslm_model <- tslm(CF ~ GBP_USD + USDI + CRUDE_OIL_AVG + COCOA + COFFEE_SPOT + TEA_AVG + 
                     SUGAR_WLD + UREA_EE_BULK + DAP + trend + season, data = CF_dataTSLM)

# Initialize variables to keep track of best model and its AIC
best_model <- NULL
best_aic <- Inf

# Create a list of predictor variables to consider in the model
predictor_list <- c("GBP_USD", "USDI", "CRUDE_OIL_AVG", "COCOA", "COFFEE_SPOT", 
                    "TEA_AVG", "SUGAR_WLD", "UREA_EE_BULK", "DAP", "trend", "season")

# Iterate through different combinations of predictors
for (i in 1:length(predictor_list)) {
  predictors <- combn(predictor_list, i, simplify = FALSE)
  
  for (j in 1:length(predictors)) {
    formula <- as.formula(paste("CF ~", paste(predictors[[j]], collapse = " + ")))
    current_model <- tslm(formula, data = CF_dataTSLM)
    current_aic <- AIC(current_model)
    
    if (current_aic < best_aic) {
      best_model <- current_model
      best_aic <- current_aic
    }
  }
}

# Print the summary of the best selected model
summary(best_model)


### fiting multivariable time series linear regression model based on best model
CF_fittrn <- tslm(
  CF ~ USDI + COFFEE_SPOT,
  data=CF_dataTSLM,biasadj = T)

CF_fittrn
summary(CF_fittrn)

## residual plots
checkresiduals(CF_fittrn)
skewness(CF_fittrn$residuals)
kurtosis(CF_fittrn$residuals)
mean(CF_fittrn$residuals)
sd(CF_fittrn$residuals)
## qq plot to check normality and tail of residuals
qqnorm(CF_fittrn$residuals);qqline(CF_fittrn$residuals,col=2)
shapiro.test(residuals(CF_fittrn))
summary(CF_fittrn2)
jarque.bera.test(residuals(CF_fittrn))

## Box test to check autocorrelation among residuals
Box.test(CF_fittrn$residuals,lag =20,type="Ljung-Box")

## calculating stats of fitted model
Stats_reg<-CF_fittrn %>% glance()
# Extract variable names from the model summary
variable_names <- summary(CF_fittrn)$call$formula[[3]]

### preparing test data for prediction
test_data2 <- data.frame(
  CF = test_tslm,
  GBP_USD = testtslm$GBP_USD,
  USDI = testtslm$USDI,
  CRUDE_OIL_AVG = testtslm$CRUDE_OIL_AVG,
  COCOA = testtslm$COCOA,
  COFFEE_SPOT = testtslm$COFFEE_SPOT,
  TEA_AVG = testtslm$TEA_AVG,
  SUGAR_WLD = testtslm$SUGAR_WLD,
  UREA_EE_BULK = testtslm$UREA_EE_BULK,
  DAP = testtslm$DAP
)
#Forecasting the tslm model
fcasttrn_ts2<-ts(predict(CF_fittrn,newdata=test_data2[,c('USDI','COFFEE_SPOT')],level=0.95),frequency=12,start=c(2022,1))
fcasttrn_ts2

#Inverse transformation of prediction and test data
inverted_seriestrn01 <- diffinv((fcasttrn_ts2), xi = tail(head(Commodity_data_ts[,'COFFEE_FUTURE'], round(nrow(Commodity_data_ts) - h2)),1))
inverted_seriestrn01
inverted_test_series01 <- diffinv((test_tslm), xi = tail(head(Commodity_data_ts[,'COFFEE_FUTURE'], round(nrow(Commodity_data_ts) - h2)),1))
inverted_test_series01

#Residual precision RMSE AND MAPE 
rmse_regtslm <- rmse(inverted_test_series01[-1],inverted_seriestrn01[-1])
print(rmse_regtslm)
MAPE_regtslm <- mape(inverted_test_series01[-1],inverted_seriestrn01[-1])
print(MAPE_regtslm)


#Inverse transformation of Actual differenced coffee data
initial_CF<-head(Commodity_data_New2$COFFEE_FUTURE,1)
Actual_CF<-(CF_dataTSLM[,'CF'])
Actual_CF2<-diffinv((Actual_CF),xi=initial_CF)

# Inverse transform of fitted data
Fitted_CF2<-diffinv((CF_fittrn$fit),xi=initial_CF)
Actual_CF2_ts<-ts(Actual_CF2,frequency = 12, start=c(1974,1))

#PLOT Actual vs prediction
autoplot(Actual_CF2_ts, series="Actual Train") +
  autolayer(Fitted_CF2, series="Fitted Train") +
  autolayer(inverted_test_series01, series="Actual Test") +
  autolayer(inverted_seriestrn01, series="Predicted Test") +
  xlab("Year") + ylab("Price (UScent/lb)") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

## PLOT Actual test vs predicted Test
autoplot(inverted_test_series01, series="Actual Test") +
  autolayer(inverted_seriestrn01, series="Predicted Test") +
  xlab("Year") + ylab("Price (UScent/lb)") +
  ggtitle("Coffee future (Test Data only)") +
  guides(colour=guide_legend(title=" "))


## Actual Data vs Fitted plot
cbind(Data = Actual_CF2_ts,
      Fitted = Fitted_CF2) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("COFFEE FUTURE") +
  geom_abline(intercept=0, slope=1)


fitted_values <- fitted(CF_fittrn)
residuals <- residuals(CF_fittrn)

# Create a data frame with fitted values and residuals
plot_data <- data.frame(Fitted = fitted_values, Residuals = residuals)

# Create a scatter plot using ggplot
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals") +
  ggtitle('Residual vs fitted plot')

residuals_df <- as.data.frame(residuals)
commodity_data_diff_df <- as.data.frame(commodity_data_diff)
res_df<-residuals_df %>% mutate(yearmonth = (seq(as.Date("1974-02-01"), by = "1 month", length.out = nrow(.)))) 
commodity_diff_df <- commodity_data_diff_df %>% mutate(yearmonth = (seq(as.Date("1974-02-01"), by = "1 month", length.out = nrow(.))))
# Join data frames based on 'month'
combined_data <- commodity_diff_df %>%
  left_join(res_df, by = "yearmonth")


# Pivot data for plotting
plot_data <- combined_data %>%
  pivot_longer(cols = c('USDI', 'COFFEE_SPOT'),
               names_to = "regressor", values_to = "x2"
  )#     names_repair = "unique"
plot_data
# Create the Residual plot against predictor
ggplot(plot_data, aes(x = x2, y = x)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")


#END****************************************************************************

## Hybrid(Dynamic Regression)
str(traintslm)
acf(CF_tslm)
pacf(CF_tslm)
xregtrn <- as.matrix(traintslm[, -3])
xregtest <- as.matrix(testtslm[, -3])

## Fitting auto arima model with best subset regressor
fit2trn <- auto.arima(CF_tslm, xreg =xregtrn[,c( "USDI", "COFFEE_SPOT" )],stepwise = FALSE)#[,c( "CRUDE_OIL_AVG", "COFFEE_ARABIC", "SUGAR_WLD" )]
fit2trn

#check the estimated coefficient
coeftest(fit2trn)

## residual plot
checkresiduals(fit2trn)

## Box test for autcorrelation check
Box.test(fit2trn$residuals,lag =20,type="Ljung-Box")

### stats of the fitted model
Stats_reg<- glance(arima(CF_tslm, order=c(1, 0, 2),seasonal = list(order = c(0,0,1),period=12),method="ML",xreg =xregtrn[,c('CRUDE_OIL_AVG','COFFEE_SPOT')]))

## forecasting on Test data predictor
fcast_test12auto <- forecast(fit2trn, 
                             xreg = xregtest[,c( "USDI", "COFFEE_SPOT" )],level=c(95))
fcast_test12auto

#Inverse transformation of prediction and test data
inverted_hybridtrn01 <- diffinv((fcast_test12auto$mean), xi = tail(head(Commodity_data_ts[,'COFFEE_FUTURE'], round(nrow(Commodity_data_ts) - h2)),1))
inverted_hybridtrn01
invertedhybrid_test_series01 <- diffinv((test_tslm), xi = tail(head(Commodity_data_ts[,'COFFEE_FUTURE'], round(nrow(Commodity_data_ts) - h2)),1))
invertedhybrid_test_series01


#Residual precision RMSE AND MAPE 
rmse_hybtslm <- rmse(invertedhybrid_test_series01[-1],inverted_hybridtrn01[-1])
print(rmse_hybtslm)
MAPE_hybtsl <- mape(invertedhybrid_test_series01[-1],inverted_hybridtrn01[-1])
print(MAPE_hybtsl)


#Inverse transformation of Actual differenced coffee data
initial_CF<-head(Commodity_data_New2$COFFEE_FUTURE,1)
Actual_CF<-(CF_dataTSLM[,'CF'])
Actual_CF2<-diffinv((Actual_CF),xi=initial_CF)

#Inverse transformation of train fitted data
Fitted_hybrid_trnCF2<-diffinv((fit2trn$fit),xi=initial_CF)
## converting data into timeseries
Actual_CF2_ts<-ts(Actual_CF2,frequency = 12, start=c(1974,1))


#PLOT Actual vs prediction
autoplot(Actual_CF2_ts, series="Actual Train") +
  autolayer(Fitted_hybrid_trnCF2, series="Fitted Train") +
  autolayer(invertedhybrid_test_series01, series="Actual Test") +
  autolayer(inverted_hybridtrn01, series="Predicted Test") +
  xlab("Year") + ylab("Price (UScent/lb)") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

#PLOT Actual Test vs predicted test
autoplot(invertedhybrid_test_series01, series="Actual Test") +
  autolayer(inverted_hybridtrn01, series="Predicted Test") +
  xlab("Year") + ylab("Price (UScent/lb)") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

# *************************************************************************************** 

## Fitted desired Arima model based on RMSE and best subset regressor
fit53trn <- Arima(CF_tslm, order=c(1,0,2),
                  seasonal = list(order = c(1,1,1),period=12),method='ML',
                  xreg =xregtrn[,c('USDI','COFFEE_SPOT')])
summary(fit53trn)
coeftest(fit53trn)

## residual plot
checkresiduals(fit53trn)
skewness(fit53trn$residuals)

## Box test to check autocorrelation among residuals
Box.test(fit53trn$residuals,lag =20,type="Ljung-Box")

## fitted model stats
Stats_regh<- glance(arima(CF_tslm, order=c(2, 0, 3),seasonal = list(order = c(1,1,1),period=12),method="ML",xreg =xregtrn[,c('USDI','COFFEE_SPOT')]))

## foreasting future data based upon the test predictor
fcast_test53 <- forecast(fit53trn, 
                         xreg = xregtest[,c('USDI','COFFEE_SPOT')],level=c(95)) #
print(fcast_test53)
#Inverse transformation of prediction and test data
inverted_hybridtrn53 <- diffinv((fcast_test53$mean), xi = tail(head(Commodity_data_ts[,'COFFEE_FUTURE'], round(nrow(Commodity_data_ts) - h2)),1))
inverted_hybridtrn53
invertedhybrid_test_series01 <- diffinv((test_tslm), xi = tail(head(Commodity_data_ts[,'COFFEE_FUTURE'], round(nrow(Commodity_data_ts) - h2)),1))
invertedhybrid_test_series01


#Residual precision RMSE AND MAPE  
rmse_hybtslm <- rmse(invertedhybrid_test_series01[-1],inverted_hybridtrn53[-1])
rmse_hybtslm
MAPE_hybtsl <- mape(invertedhybrid_test_series01[-1],inverted_hybridtrn53[-1])
MAPE_hybtsl

#Inverse transformation of Actual differenced coffee data
initial_CF<-head(Commodity_data_New2$COFFEE_FUTURE,1)
Actual_CF<-(CF_dataTSLM[,'CF'])
Actual_CF2<-diffinv((Actual_CF),xi=initial_CF)

## Inverse transformation of fitted train data
Fitted_hybrid_trn53<-diffinv((fit53trn$fit),xi=initial_CF)
Actual_CF2_ts<-ts(Actual_CF2,frequency = 12, start=c(1974,1))
tail(Actual_CF2_ts,1)

#PLOT Actual vs prediction
autoplot(Actual_CF2_ts, series="Actual Train") +
  autolayer(Fitted_hybrid_trn53, series="Fitted Train") +
  autolayer(invertedhybrid_test_series01, series="Actual Test") +
  autolayer(inverted_hybridtrn01, series="Predicted Test") +
  xlab("Year") + ylab("Price (UScent/lb)") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

#PLOT Actual Test vs predicted Test
autoplot(invertedhybrid_test_series01, series="Actual Test") +
  autolayer(inverted_hybridtrn01, series="Predicted Test") +
  xlab("Year") + ylab("Price (UScent/lb)") +
  ggtitle("Coffee future Forecasting (Test Data only)") +
  guides(colour=guide_legend(title=" "))

## Arima and Regression residual preparation for ploting
reg_resid <- as_tibble(residuals(fit53trn, type = "regression"))
arima_resid <- as_tibble(residuals(fit53trn, type = "innovation"))
reg_resid$type <- "Regression residuals"
arima_resid$type <- "ARIMA residuals"
residuals_df <- bind_rows(reg_resid, arima_resid) %>%
  mutate(yearmonth = rep(seq(as.Date("1974-02-01"), by = "1 month", length.out = nrow(.) / 2), 2)) %>%
  arrange(type, yearmonth)

## Plot Arima and regression residuals
ggplot(residuals_df, aes(x = yearmonth, y = x, color = type, group = type)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y",dir='v') +
  labs(x = "Year-Month", y = "Residuals", color = "Residual Type")


#end*************************************************************************  

#ARIMA model ******************************************************************************


commodity_coffee_f <- subset(Commodity_data_New3, select= c('Date','COFFEE_FUTURE'))

Coffee_Future_ts <- ts(commodity_coffee_f$COFFEE_FUTURE,frequency=12,start=c(1974,1))

# splitting train and test data of coffee future timeseries
train = window(Coffee_Future_ts, start=1974, end=c(2021,12))

### test set
### use remaining data from 1957 to 1960 to test accuracy

test = window(Coffee_Future_ts, start=2022, end=c(2023,5))


## ACF and PACF plots
acf(train,lag=40)
pacf(train,lag=40)
acf(Coffee_Future_ts)
pacf(Coffee_Future_ts)
acf(difference(commodity_coffee_f$COFFEE_FUTURE,1), lag.max=36,na.action = na.pass)
pacf(difference(commodity_coffee_f$COFFEE_FUTURE,1), lag.max=36,na.action = na.pass)

ggtsdisplay(diff(train),
            plot.type = "partial",
            main = "ACF & PACF plot for 'Coffee Future Training data' Time-Series",
            lag=40,
            smooth = TRUE) 

ggtsdisplay(diff(Coffee_Future_ts),
            plot.type = "partial",
            main = "ACF & PACF plot for `CF' Time-Series",
            lag=40,
            smooth = TRUE)



#Decomposing coffee future training time series
train_dc <- decompose(train)
autoplot(train_dc)

## Coffee future differenced time series stats
table <- data.frame(
  "Statistic" = c("Mean", "Standard deviation", "Variance", "Skewness", "Kurtosis"),
  "Value" = c(mean(diff(Coffee_Future_ts),na.rm = TRUE), sd(diff(Coffee_Future_ts),na.rm = TRUE), var(diff(Coffee_Future_ts),na.rm = TRUE), skewness(diff(Coffee_Future_ts),na.rm = TRUE), kurtosis(diff(Coffee_Future_ts),na.rm = TRUE))
)
print(table)

## Coffee future log transformed time series stats
table_log <- data.frame(
  "Statistic" = c("Mean", "Standard deviation", "Variance", "Skewness", "Kurtosis"),
  "Value" = c(mean(diff(log(Coffee_Future_ts))), sd(diff(log(Coffee_Future_ts))), var(diff(log(Coffee_Future_ts))), skewness(diff(log(Coffee_Future_ts))), kurtosis(diff(log(Coffee_Future_ts))))
)
print(table_log)

# Normality test
jb_test<-jarque.bera.test((Coffee_Future_ts))
print(jb_test)

## Density plot of coffee future differenced time series
qplot(diff(Coffee_Future_ts) , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) +
  geom_vline(xintercept = mean(diff(Coffee_Future_ts),na.rm = TRUE) , color = 'red' , size = 1) +
  labs(x = 'value', y='Density',title='Density plot CF')

qplot(Coffee_Future_ts_log , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) +
  geom_vline(xintercept = mean(Coffee_Future_ts_log) , color = 'red' , size = 1) +
  labs(x = 'value', y='Density',title='Density plot CF return')



#ARIMA (5,1,3)(1,0,0)12*****************************************
CF_ARMA_fit_5_3s <- Arima(train, order=c(5, 1, 3),seasonal = list(order = c(1,0,0),period=12),method="ML")
CF_ARMA_fit_5_3s
#Residual graph
Box.test(CF_ARMA_fit_5_3s$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals
CF_ARMA_fit_5_3s %>% checkresiduals()
coeftest(CF_ARMA_fit_5_3s)
jarque.bera.test(CF_ARMA_fit_5_3s$residuals)

fitnew53s<-fitted(CF_ARMA_fit_5_3s)
stage5_3s<-glance(arima(train, order=c(5, 1, 3),seasonal = list(order = c(1,0,0),period=12),method="ML"))

# forecasted future data
futurVal53s <- forecast(CF_ARMA_fit_5_3s,h=17, level=c(95)) #confidence level 95%
futurVal53s

# rmse and mape calculation
rmse53s<-rmse(futurVal53s$mean,test)
rmse53s
mape53s <- mape(futurVal53s$mean,test)
mape53s

# Actual vs Prediction
autoplot(train, series="train") +
  autolayer(fitnew53s, series="fitted") +
  autolayer(test, series="Test") +
  autolayer(futurVal53s$mean, series="predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))


#end**************************************************************************

## ARIMA(5,1,3)(0,0,1)12
CF_ARMA_fit_5_3train2 <- Arima(train, order=c(5, 1, 3),seasonal = list(order = c(0,0,1),period=12),method="ML")
CF_ARMA_fit_5_3train2
 
Box.test(CF_ARMA_fit_5_3train2$residuals,lag =20,type="Ljung-Box",fitdf=8) #BoX-test for residuals
#Residual graph
CF_ARMA_fit_5_3train2 %>% checkresiduals()
coeftest(CF_ARMA_fit_5_3train2)
skewness(CF_ARMA_fit_5_3train2$residuals)

fitnew53train<-fitted(CF_ARMA_fit_5_3train2)
## calculate r2 score
ord_r2<-cor(fitnew53train,train)^2
## fitted stats
glance_53s<-glance(arima(train,order=c(5, 1, 3),seasonal = list(order = c(0,0,1),period=12),method="ML"))

## forecasted future data
futurVal53trns <- forecast(CF_ARMA_fit_5_3train2,h=12, level=c(95)) #confidence level 99%
futurVal53trns

## RMSE AND MAPE calculation
rmse23<-rmse(futurVal53trns$mean,test)
rmse23
mape23 <- mape(futurVal53trn$mean,test)
mape23

## PLOT ACTUAL vs PREDICTED
autoplot(train, series="Actual train") +
  autolayer(fitnew53train, series="fitted train") +
  autolayer(test, series="Actual Test") +
  autolayer(futurVal53trns$mean, series="Predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

## PLOT ACTUAL TEST vs PREDICTED TEST
autoplot(test, series="Actual Test") +
  autolayer(futurVal53trns$mean, series="Predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future (Test data only)") +
  guides(colour=guide_legend(title=" "))

# *****************************************************************************

#ARIMA(2,1,3)(0,1,1)12****************************************************
CF_ARMA_fit_2_3s <- Arima(train, order=c(2, 1, 3),seasonal = list(order = c(0,1,1),period=12),method="ML")
CF_ARMA_fit_2_3s

Box.test(CF_ARMA_fit_2_3s$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals

#Residual graph
CF_ARMA_fit_2_3s %>% checkresiduals()
coeftest(CF_ARMA_fit_2_3s)

#Normality test
jarque.bera.test(CF_ARMA_fit_2_3s$residuals)

fitnew_23s<-CF_ARMA_fit_2_3s$fitted

#Forecasted future data
futurVal23s <- forecast(CF_ARMA_fit_2_3s,h=17, level=c(95)) #confidence level 99%
futurVal23s

#Fitted stats
glance_23s<-glance(arima(train, order=c(2, 1, 3),seasonal = list(order = c(0,1,1),period=12),method="ML"))

## RMSE AND MAPE calculation
rmse23s<-rmse(futurVal23s$mean,test)
rmse23s
mape23s <- mape(futurVal23s$mean,test)
mape23s

# Actual vs prediction
autoplot(train, series="Actual train") +
  autolayer(fitnew_23s, series="fitted train") +
  autolayer(test, series="Actual Test") +
  autolayer(futurVal23s$mean, series="Predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

# Actual Test vs predicted Test
autoplot(test, series="Actual Test") +
  autolayer(futurVal23s$mean, series="Predicted test") +
  xlab("Year") + ylab("Price (UScent/lb") +
  ggtitle("Coffee future (Test data only)") +
  guides(colour=guide_legend(title=" "))
# *****************************************************************************
#ARIMA(5,1,3)(0,1,1)12*********************************************
CF_ARMA_fit_5_1_3_ssma <- Arima(train, order=c(5, 1, 3),seasonal = list(order = c(0,1,1),period=12),method="ML")
CF_ARMA_fit_5_1_3_ssma

Box.test(CF_ARMA_fit_5_1_3_ssma$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals

#Residual graph
CF_ARMA_fit_5_1_3_ssma %>% checkresiduals()

coeftest(CF_ARMA_fit_5_1_3_ssma)

fit_513_ssma<-CF_ARMA_fit_5_1_3_ssma$fitted

#Forecasted future value
futurVal513ssma <- forecast(CF_ARMA_fit_5_1_3_ssma,h=17, level=c(95)) #confidence level 99%
futurVal513ssma

glance_53ssma<-glance(arima(train, order=c(5, 1, 3),seasonal = list(order = c(0,1,1),period=12),method="ML"))


# RMSE AND MAPE calculation
rmse53ssma<-rmse(futurVal513ssma$mean,test)
rmse53ssma
mape53ssma <- mape(futurVal513ssma$mean,test)
mape53ssma

# Actual vs Prediction
autoplot(train, series="Actual train") +
  autolayer(fit_513_ssma, series="fitted train") +
  autolayer(test, series="Actual Test") +
  autolayer(futurVal513ssma$mean, series="Predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

# Actual Test vs Predicted Test
autoplot(test, series="Actual Test") +
  autolayer(futurVal513ssma$mean, series="Predicted test") +
  xlab("Year") + ylab("Price (UScent/lb") +
  ggtitle("Coffee future (Test data only)") +
  guides(colour=guide_legend(title=" "))
# end*******************************************************************

#ARMA(2,3) non zero mean*************************************************
CF_ARMA_fit_2_3ws <- Arima(train, order=c(2, 0, 3),method="ML")
CF_ARMA_fit_2_3ws

Box.test(CF_ARMA_fit_2_3ws$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals

#Residual graph
CF_ARMA_fit_2_3ws %>% checkresiduals()
coeftest(CF_ARMA_fit_2_3ws)

fitnew_23ws<-CF_ARMA_fit_2_3ws$fitted

glance_23ws<-glance(arima(train, order=c(2, 0, 3),method="ML"))

# Forecasted Future data
futurVal23wstrn <- forecast(CF_ARMA_fit_2_3ws,h=17, level=c(95)) #confidence level 99%
futurVal23wstrn


# RMSE vs MAPE calculation
rmse23<-rmse(futurVal23wstrn$mean,test)
rmse23
mape23 <- mape(futurVal23wstrn$mean,test)
mape23

#Actual vs Prediction
autoplot(train, series="Actual train") +
  autolayer(fitnew_23ws, series="fitted train") +
  autolayer(test, series="Actual Test") +
  autolayer(futurVal23wstrn$mean, series="Predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

#Actual Test vs Predicted Test
autoplot(test, series="Actual Test") +
  autolayer(futurVal23wstrn$mean, series="Predicted test") +
  xlab("Year") + ylab("Price (UScent/lb") +
  ggtitle("Coffee future (Test data only)") +
  guides(colour=guide_legend(title=" "))
#END*****************************************************************
#ARIMA(5,1,3) TRAIN without seasonal*****************************************
CF_ARMA_fit_5_3ws <- Arima(train, order=c(5, 1, 3),method="ML")
CF_ARMA_fit_5_3ws

Box.test(CF_ARMA_fit_5_3ws$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals

#Residual graph
CF_ARMA_fit_5_3ws %>% checkresiduals()
coeftest(CF_ARMA_fit_5_3ws)

fitnew_53ws<-CF_ARMA_fit_5_3ws$fitted
glance_53ws<-glance(arima(train, order=c(5, 1, 3),method="ML"),method="ML")

# forecasted future data
futurVal53wstrn <- forecast(CF_ARMA_fit_5_3ws,h=17, level=c(95)) #confidence level 99%
futurVal53wstrn

# rmse and mape calculation
rmse53ws<-rmse(futurVal53wstrn$mean,test)
rmse53ws
mape53ws <- mape(futurVal53wstrn$mean,test)
mape53ws

# ACTUAL vs Prediction
autoplot(train, series="Actual train") +
  autolayer(fitnew_53ws, series="fitted train") +
  autolayer(test, series="Actual Test") +
  autolayer(futurVal53wstrn$mean, series="Predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

# ACTUAL test vs Predicted Test
autoplot(test, series="Actual Test") +
  autolayer(futurVal53wstrn$mean, series="Predicted test") +
  xlab("Year") + ylab("Price (UScent/lb") +
  ggtitle("Coffee future (Test data only)") +
  guides(colour=guide_legend(title=" "))
#END*************************************************************

#ARIMA(5,1,3)(1,1,0)12 seasonal ************************************************************
CF_ARMA_fit_5_3sar <- Arima(train, order=c(5, 1, 3),seasonal = list(order = c(1,1,0),period=12),method="ML")
CF_ARMA_fit_5_3sar

Box.test(CF_ARMA_fit_5_3sar$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals

#Residual graph
CF_ARMA_fit_5_3sar %>% checkresiduals()
coeftest(CF_ARMA_fit_5_3sar)

fitnew_53sar<-CF_ARMA_fit_5_3sar$fitted

# fitted stats
glance_53sar<-glance(arima(train,  order=c(5, 1, 3),seasonal = list(order = c(1,1,0),period=12),method="ML"))

# forecasting future data
futurVal53sartrn <- forecast(CF_ARMA_fit_5_3sar,h=17, level=c(95)) #confidence level 99%
futurVal53sartrn

# rmse vs mape calculation future data
rmse53sar<-rmse(futurVal53sartrn$mean,test)
rmse53sar
mape53sar <- mape(futurVal53sartrn$mean,test)
mape53sar


#Actual vs Prediction
autoplot(train, series="Actual train") +
  autolayer(fitnew_53sar, series="fitted train") +
  autolayer(test, series="Actual Test") +
  autolayer(futurVal53sartrn$mean, series="Predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

# Actual Test vs Predicted Test
autoplot(test, series="Actual Test") +
  autolayer(futurVal53sartrn$mean, series="Predicted test") +
  xlab("Year") + ylab("Price (UScent/lb") +
  ggtitle("Coffee future(Test data only)") +
  guides(colour=guide_legend(title=" "))

#ARIMA(5,1,3)(1,1,1)12*********************************

CF_ARMA_fit_5_3sstrain <- Arima(train, order=c(5, 1, 3),seasonal = list(order = c(1,1,1),period=12),method="ML")
CF_ARMA_fit_5_3sstrain
CF_ARMA_fit_5_3sstrain %>% checkresiduals()

# Autocorrelation check
Box.test(CF_ARMA_fit_5_3sstrain$residuals,lag =20,type="Ljung-Box") 

coeftest(CF_ARMA_fit_5_3sstrain)
# Normality check
jarque.bera.test(residuals(CF_ARMA_fit_5_3sstrain))
qqnorm(CF_ARMA_fit_5_3sstrain$residuals);qqline(CF_ARMA_fit_5_3sstrain$residuals,col=2)


fitnew_53ss<-CF_ARMA_fit_5_3sstrain$fitted
glance_53ss<-glance(arima(train,  order=c(5, 1, 3),seasonal = list(order = c(1,1,1),period=12),method="ML"))

# forecasted  future data
futurVal53sstrn <- forecast(CF_ARMA_fit_5_3sstrain,h=17, level=c(95)) #confidence level 99%
futurVal53sstrn

# rmse vs mape calculation
rmse53ss<-rmse(futurVal53sstrn$mean,test)
rmse53ss
mape53ss <- mape(futurVal53sstrn$mean,test)
mape53ss

# Actual vs Prediction
autoplot(train, series="Actual train") +
  autolayer(fitnew_53ss, series="fitted train") +
  autolayer(test, series="Actual Test") +
  autolayer(futurVal53sstrn$mean, series="Predicted test") +
  xlab("Year") + ylab("Price (UScent/lb)") +
  ggtitle("Coffee Future forecasting") +
  guides(colour=guide_legend(title=" "))

# Actual test vs Predicted test
autoplot(test, series="Actual Test") +
  autolayer(futurVal53sstrn$mean, series="Predicted Test") +
  xlab("Year") + ylab("Price (UScent/lb)") +
  ggtitle("Coffee Future forecasting (Test Data only)") +
  guides(colour=guide_legend(title=" "))
#end*****************************************************************************

#end*****************************************************************
#Auto_Arima on coffee future entire time series
CF_arima_auto <- auto.arima(Coffee_Future_ts,stepwise = FALSE, trace = T,method="ML")
CF_arima_auto

Box.test(CF_arima_auto$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals

#Residual graph
CF_arima_auto %>% checkresiduals()
coeftest(CF_arima_auto)

fitnew_autoarma<-CF_arima_auto$fitted

#Normality test
jarque.bera.test(residuals(CF_arima_auto))

# Future prediction
futurVal_autoarma <- forecast(CF_arima_auto,h=36, level=c(95)) #confidence level 99%
#futurVal_autoarma2 <-InvBoxCox(futurVal_autoarma$mean, lambda)
futurVal_autoarma

#Coffee future prediction plot
autoplot(Coffee_Future_ts, series="Data") +
  # autolayer(fitnew_autoarma, series="fitted") +
  autolayer(futurVal_autoarma, series="predicted") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

#end ***********************************************************************

#Auto Arima Train data:**************************************
CF_arima_autotrain <- auto.arima(train,stepwise = FALSE, trace = T,method="ML")
CF_arima_autotrain

Box.test(CF_arima_autotrain$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals

#Residual graph
CF_arima_autotrain %>% checkresiduals()
coeftest(CF_arima_autotrain)
fitnew_autoarmatrain<-CF_arima_autotrain$fitted

# Normality check
jarque.bera.test(residuals(CF_arima_autotrain))
glance_autotrn<-glance(arima(train,  order=c(1, 0, 3),method="ML"))

# Forecasting future data
futurVal_auto_trn <- forecast(CF_arima_autotrain,h=17, level=c(95)) #confidence level 99%
futurVal_auto_trn

# rmse vs mape calculation
rmse_autotrn<-rmse(futurVal_auto_trn$mean,test)
rmse_autotrn
mape_autotrn <- mape(futurVal_auto_trn$mean,test)
mape_autotrn

#Actual vs prediction
autoplot(train, series="Actual train") +
  autolayer(fitnew_autoarmatrain, series="fitted train") +
  autolayer(test, series="Actual Test") +
  autolayer(futurVal_auto_trn$mean, series="Predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

# Actual Test vs Predicted Test
autoplot(test, series="Actual Test") +
  autolayer(futurVal_auto_trn$mean, series="Predicted test") +
  xlab("Year") + ylab("Price (UScent/lb") +
  ggtitle("Coffee future(Test data only)") +
  guides(colour=guide_legend(title=" "))
#end*********************************************************************************

#Difference model using Arma(5,1)*****************
CF_ARMA_fit_5_3diffws <- Arima(train, order=c(5, 1, 0),method="ML")
CF_ARMA_fit_5_3diffws
#Residual graph
Box.test(CF_ARMA_fit_5_3diffws$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals

# #Residual graph
CF_ARMA_fit_5_3diffws %>% checkresiduals()
coeftest(CF_ARMA_fit_5_3diffws)

fitnew_diff53diffws<-CF_ARMA_fit_5_3diffws$fitted
glance_53ws<-glance(arima(train,  order=c(5, 1, 0),method="ML"))

# forecasted future data
futurVal_51 <- forecast(CF_ARMA_fit_5_3diffws,h=17, level=c(95)) #confidence level 99%
futurVal_51

# rmse and mape calculation
rmse51<-rmse(futurVal_51$mean,test)
rmse51
mape51 <- mape(futurVal_51$mean,test)
mape51

# Actual vs prediction plot
autoplot(train, series="Actual train") +
  autolayer(fitnew_diff53diffws, series="fitted train") +
  autolayer(test, series="Actual Test") +
  autolayer(futurVal_51$mean, series="Predicted test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

# Actual test vs Predicted Test
autoplot(test, series="Actual Test") +
  autolayer(futurVal_51$mean, series="Predicted test") +
  xlab("Year") + ylab("Price (UScent/lb") +
  ggtitle("Coffee future(Test Data only)") +
  guides(colour=guide_legend(title=" "))

# **************************************************************************************
acf(diff(BoxCox(train,lambda = -0.3403054),difference=1),lag.max=40)
pacf(diff(BoxCox(train,lambda = -0.3403054),difference=1),lag.max=40)
#ARIMA(5,1,3)(0,1,1)12 BOX COX TRANSFORMED model on train*************************************************
CF_ARMA_fit_5_3tmas <- Arima(train, order=c(5, 1, 3),seasonal = list(order = c(0,1,1),period=12),method="ML",lambda='auto')
CF_ARMA_fit_5_3tmas

Box.test(CF_ARMA_fit_5_3tmas$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals

#Residual graph
CF_ARMA_fit_5_3tmas %>% checkresiduals()
coeftest(CF_ARMA_fit_5_3tmas)
jarque.bera.test(CF_ARMA_fit_5_3tmas$residuals)

fitnew53mas<-fitted(CF_ARMA_fit_5_3tmas)

# forecasted future data
futurenew53tmas <- forecast(CF_ARMA_fit_5_3tmas,h=12, level=c(95)) #confidence level 99%
futurenew53tmas

# rmse and mape calculation
rmse53tmas<-rmse(futurenew53tmas$mean,test)
rmse53tmas
mape53tmas <- mape(futurenew53tmas$mean,test)
mape53tmas

# Actual Test vs Predicted Test
autoplot(test, series="Actual Test") +
  autolayer(futurenew53tmas$mean, series="Predicted Test") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))
# end*****************************************************************

# Coffee future prediction without seasonality***********************************************
CF_ARMA_fit_1_3ws <- Arima(Coffee_Future_ts, order=c(1, 0, 3),method="ML",include.mean = TRUE)
CF_ARMA_fit_1_3ws

Box.test(CF_ARMA_fit_1_3ws$residuals,lag =20,type="Ljung-Box") #BoX-test for residuals
#Residual graph
CF_ARMA_fit_1_3ws %>% checkresiduals()
coeftest(CF_ARMA_fit_1_3ws)

#Normality test
jarque.bera.test(CF_ARMA_fit_1_3ws$residuals)

fitnew13ws<-fitted(CF_ARMA_fit_1_3ws)


#forecasted future dta
futurenew13ws <- forecast(CF_ARMA_fit_1_3ws,h=60, level=c(95)) #confidence level 99%
futurenew13ws


# Coffee future prediction
autoplot(Coffee_Future_ts, series="Actual Data") +
  autolayer(futurenew13ws, series="predicted") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))


# **********************************************************************************
#Boxcox transformed ARIMA(5,1,3)(0,1,1)12model*********************************

CF_ARMA_fit_5_3tsCF <- Arima(Coffee_Future_ts, order=c(5, 1, 3),seasonal = list(order = c(0,1,1),period=12),method="ML",lambda='auto')
CF_ARMA_fit_5_3tsCF

#Residual Graph
CF_ARMA_fit_5_3tsCF %>% checkresiduals()

#check Autocorrelation among residuals
Box.test(CF_ARMA_fit_5_3tsCF$residuals,lag =20,type="Ljung-Box")

#Normality check
skewness(CF_ARMA_fit_5_3tsCF$residuals)
kurtosis(CF_ARMA_fit_5_3tsCF$residuals)
mean(CF_ARMA_fit_5_3tsCF$residuals)
kurtosis(CF_ARMA_fit_5_3tsCF$residuals)
jarque.bera.test(residuals(CF_ARMA_fit_5_3tsCF))
qqnorm(CF_ARMA_fit_5_3tsCF$residuals);qqline(CF_ARMA_fit_5_3tsCF$residuals,col=2)

#Coefficient checked
coeftest(CF_ARMA_fit_5_3tsCF)
fitnew_53tsCF<-CF_ARMA_fit_5_3tsCF$fitted

# forecasted future data
futurVal53tssCF <- forecast(CF_ARMA_fit_5_3tsCF,h=36, level=c(95)) #confidence level 99%
futurVal53tssCF

# coffee future Prediciton
autoplot(Coffee_Future_ts, series="Actual Data") +
  autolayer(futurVal53tssCF, series="Predicted") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future Transformed model Prediction") +
  guides(colour=guide_legend(title=" "))
# end**********************************************************************************

# Coffee future Prediction using best model ARIMA(5,1,3)(1,1,1)12 *************************************************
CF_ARMA_fit_5_3sCF <- Arima(Coffee_Future_ts, order=c(5, 1, 3),seasonal = list(order = c(1,1,1),period=12),method="ML")
CF_ARMA_fit_5_3sCF

#Residual graph
CF_ARMA_fit_5_3sCF %>% checkresiduals()

#Autocorrelation check among residuals
Box.test(CF_ARMA_fit_5_3sCF$residuals,lag =20,type="Ljung-Box") 

# normality test
skewness(CF_ARMA_fit_5_3sCF$residuals)
kurtosis(CF_ARMA_fit_5_3sCF$residuals)
mean(CF_ARMA_fit_5_3sCF$residuals)
kurtosis(CF_ARMA_fit_5_3sCF$residuals)
coeftest(CF_ARMA_fit_5_3sCF)
jarque.bera.test(residuals(CF_ARMA_fit_5_3sCF))
qqnorm(CF_ARMA_fit_5_3sCF$residuals);qqline(CF_ARMA_fit_5_3sCF$residuals,col=2)

#Forecasting Coffee future data
futurVal53ssCF <- forecast(CF_ARMA_fit_5_3sCF,h=36, level=c(95)) #confidence level 99%
futurVal53ssCF

# Coffee future prediction for next 3 year
autoplot(Coffee_Future_ts, series="Actual Data") +
  autolayer(futurVal53ssCF, series="predicted") +
  xlab("Year") + ylab("Price") +
  ggtitle("Coffee future") +
  guides(colour=guide_legend(title=" "))

# *******************************end************************************************************

