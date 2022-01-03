library(data.table) # Fast and clean manipulation of data.tables
library(countrycode) # Country names and ISO codes
library(dplyr) # Get access to pipe %>%
library(zoo) # Allows for easy interpolation/filling of NA's in data 
library(betareg) # Beta regression modeling for proportions/percentages btw. (0,1)
library(ggplot2)
library(plotly)


CLASP_palette <- c("#474C55", "#318589", "#F39922", "#084259", "#C7563E", 
                   "#55382F", "#3D826B", "#CC804A", "#8F8533", "#8F96A8", 
                   "#8C6154", "#7D4D80", "#2E4D42")

## IMPORT DATA
Sys.getenv(c("systemdrive","homepath")) %>%  #get main drive letter (e.g., C:\ and home directory)
  paste(collapse ="") %>%  #returns the path variables as a vector, so collapse into a string
  paste("/Box/Programs/Climate/Impacts & Modelling/Mepsy/Data Files/Updated Data files/Data file/Extending Mepsy Past 2030/Motor", sep ="") %>%
  # add the rest of the path
  setwd #change working directory

PAMS_countries <- fread("PAMS_countries.csv")
PAMS_countries[, ISO := countrycode(Country, origin="country.name", destination="iso3c")]
PAMS_countries[, Region := countrycode(ISO, origin="iso3c", destination="region23")]

###Import Shipment
#country_sales <- read.csv("country_shipment.csv", sep = ",")
country_sales<- read.csv("12countries_shipment.csv")
setDT(country_sales)  
country_sales <- melt(country_sales, id.vars=c("Country","ISO"))
colnames(country_sales) <- c("Country", "ISO", "Year", "Shipment")
country_sales$Year <- as.numeric(substr(country_sales$Year,2,5))
#country_sales$Shipment <- country_sales$Shipment*1000
setnames(country_sales, "Shipment", "Shipment(thousands)") 
country_sales$Year<- as.integer(country_sales$Year) # Convert year to integer
#str(country_sales)

### Import Industrital value added GDP
gdp <- fread("WB_GDPPPP_Historical.csv", skip = "Country Name") # Skip to first col
colnames(gdp) <- as.character(gdp[1,]) # For some reason, not reading column names from file, so set manually
gdp <- gdp[-1,] # Remove duplicative first row
gdp[, c("Indicator Name", "Indicator Code") := NULL] # Remove unnecessary columns
setnames(gdp, "Country Code", "ISO") # Rename to ISO for consistency with other tables
setnames(gdp, "Country Name", "Country") # Rename to ISO for consistency with other tables
gdp.tidy <- melt(gdp, id.vars = c("Country", "ISO"), variable.name = "Year", 
                    value.name = "GDP(Norminal)") 
#str(ivagdp.tidy)
gdp.tidy$Year<- as.integer(as.character(gdp.tidy$Year)) # Convert year to integer
gdp.tidy[, c("Country"):=NULL]

#Calculate IVAGDP-ppp based on percentage of Industry/gdp, data from World bank
#https://data.worldbank.org/indicator/NV.IND.TOTL.ZS

rate<-fread("WB_IVA_GDP_Ratio.csv",skip = "Country Name") 
colnames(rate) <- as.character(rate[1,]) # For some reason, not reading column names from file, so set manually
rate <- rate[-1,] # Remove duplicative first row
rate[, c("Indicator Name", "Indicator Code") := NULL] # Remove unnecessary columns
setnames(rate, "Country Code", "ISO") # Rename to ISO for consistency with other tables
setnames(rate, "Country Name", "Country") # Rename to ISO for consistency with other tables
rate.tidy <- melt(rate, id.vars = c("Country", "ISO"), variable.name = "Year", 
                  value.name = "IVAGDP_Rate") 
rate.tidy$Year<- as.integer(as.character(rate.tidy$Year)) # Convert year to integer
rate.tidy <- rate.tidy[!is.na(`IVAGDP_Rate`)]
rate.tidy[, c("Country"):=NULL]


###Import warehouse building stock Index
building <- fread("Warehouse_Buildingstock.csv") 
building.tidy<- building[, c("Country Name", "Region") := NULL]
setnames(building.tidy,"ISO3", "ISO")


###Import employment Index
employ <- fread("WB_EMPL.csv", skip = "Country Name") # Skip to first col
colnames(employ) <- as.character(employ[1,]) # For some reason, not reading column names from file, so set manually
employ <- employ[-1,] # Remove duplicative first row
employ[, c("Indicator Name", "Indicator Code") := NULL] # Remove unnecessary columns
setnames(employ, "Country Code", "ISO") # Rename to ISO for consistency with other tables
setnames(employ, "Country Name", "Country") # Rename to ISO for consistency with other tables
employ.tidy <- melt(employ, id.vars = c("Country", "ISO"), variable.name = "Year", 
                    value.name = "Employment") 
employ.tidy$Year<- as.integer(as.character(employ.tidy$Year)) # Convert year to integer
employ.tidy[, c("Country"):=NULL]
#employ.tidy.latestyear <- employ.tidy[which(employ.tidy$Year == 2019),]
#employ.tidy.lastestyear <-na.omit(employ.tidy.latestyear)
#str(employ.tidy)


#Integrate three tables together
sales.latestyear <- country_sales[which(country_sales$Year == 2019),]
sales.latestyear[, c("Country"):=NULL]
sales.latestyear<-merge(sales.latestyear, gdp.tidy, by=c("Year", "ISO"))
sales.latestyear<- merge(sales.latestyear, building.tidy, by=c("Year", "ISO"))
sales.latestyear<- merge(sales.latestyear, rate.tidy, by=c("Year", "ISO"))
sales.latestyear[,`IVAGDP(PPP/Billion)`:= `GDP(Norminal)`*(IVAGDP_Rate/100)/(10^9),by=c("Year", "ISO")]
sales.latestyear<- merge(sales.latestyear, employ.tidy, by=c("Year", "ISO"))
sales.latestyear[,`Employment`:= `Employment`/100,by=c("Year", "ISO")]

sales.latestyear<-na.omit(sales.latestyear)




#library(moments)
#skewness(sales.latestyear$`IVAGDP(PPP/Billion)`, na.rm = TRUE)

##Visualize two variables
#library(ggpubr)
#IVAGDP distribution
#ggdensity(sales.latestyear, x = "IVAGDP(PPP/Billion)", fill = "lightgray", title = "IVAGDP") +
#  scale_x_continuous(limits = c(0, 1000)) +
#  stat_overlay_normal_density(color = "red", linetype = "dashed")
#skewness(sales.latestyear$`IVAGDP(PPP/Billion)`, na.rm = TRUE)
# [1] 2.122586 Highly skewed distribution. Need to be normalized

#Building distribution
#ggdensity(sales.latestyear, x = "Building Stock (Mm2)", fill = "lightgray", title = "Building") +
#  scale_x_continuous(limits = c(0, 100)) +
#  stat_overlay_normal_density(color = "red", linetype = "dashed")
#skewness(sales.latestyear$`Building Stock (Mm2)`, na.rm = TRUE)
# [1] 2.79529 Highly skewed distribution. Need to be normalized

#shipment distribution
#ggdensity(sales.latestyear, x = "Shipment(thousands)", fill = "lightgray", title = "Shipment") +
#  scale_x_continuous(limits = c(0, 3000)) +
#  stat_overlay_normal_density(color = "red", linetype = "dashed")
## All variables are positive skew
#skewness(sales.latestyear$`Shipment(thousands)`, na.rm = TRUE)
## 1.309739 (Bigger than 1, it is a highly skewed distribution)

#summary(sales.latestyear$`Shipment(thousands)`)
#hist (sales.latestyear$`Shipment(thousands)`)


## if variables are linear related

#plot(x=sales.latestyear$`Shipment(thousands)`,y=sales.latestyear$`Building Stock (Mm2)`,
#     xlab ="Sales",
#     ylab="Warehouse",
#     xlim=c(0,3000),
#     ylim = c(0,100),
#     main = "Sales vs Warehouse"
#)+lines(c(0,5),c(0,3))

#plot(x=sales.latestyear$`Shipment(thousands)`,y=sales.latestyear$`IVAGDP(PPP/Billion)`,
#     xlab ="Sales",
#     ylab="IVAGDP",
#     xlim=c(0,2000),
#     ylim = c(0,2000),
#     main = "Sales vs IVAGDP"
#)+lines(c(0,2000),c(0,2000))

# sales.latestyear <- sales.latestyear [ISO != "MAC"]Not move the Macao it will increase the regression error


## Linear regression - Without Log-transformed
set.seed(1) # set seed for repeatability
sample <- sample(c(TRUE, FALSE), nrow(sales.latestyear), replace=TRUE, prob=c(0.7,0.3))
train <- sales.latestyear[sample, ]
test <- sales.latestyear[!sample, ]  

motormodel <- lm(`Shipment(thousands)` ~ 
                            `IVAGDP(PPP/Billion)`+`Building Stock (Mm2)`*`Employment`,
                          data = train)
summary (motormodel) #0.9941

sales.latestyear[, `Predicted_Motor_Shipment` := 
                        predict(motormodel, newdata=sales.latestyear, type="response")
                ]
sales.latestyear[, sqrt(sum((`Shipment(thousands)`-`Predicted_Motor_Shipment`)^2))/.N]
# 45.34248

plot(sales.latestyear[,`Shipment(thousands)`], 
     sales.latestyearr[,`Predicted_Motor_Shipment`]) + lines(c(0,4000),c(0,4000))

p <- ggplot(data = sales.latestyear, aes(x=(`Shipment(thousands)`), y=`Predicted_Motor_Shipment`))
p+geom_point()+geom_abline(intercept=0, slope=1, linetype="solid")+ geom_text(x=4000, y=4000, label="1x line", color="black", angle=45)+ xlim(0,4000)+ylim(0,4000)

##betaregression
maxshipment <-6595
sales.latestyear[, `scaled shipment` := `Shipment(thousands)`/maxshipment]
betamotor <- betareg(`scaled shipment` ~
                       `IVAGDP(PPP/Billion)`+`Building Stock (Mm2)`*`Employment`, data=sales.latestyear)
summary(betamotor) #0.9772
sales.latestyear[, `Predicted_Motorbeta_Shipment` := 
                   maxshipment*predict(betamotor, newdata=sales.latestyear, type="response")
]
sales.latestyear[, sqrt(sum((`Shipment(thousands)`-`Predicted_Motorbeta_Shipment`)^2))/.N]
##63.868

## Creat regression model for 35 countries
motormodel2 <- lm(`Shipment(thousands)` ~ 
                   `IVAGDP(PPP/Billion)`+`Building Stock (Mm2)`,
                 data = sales.latestyear)
summary (motormodel2) #0.8231

sales.latestyear[, `Predicted_Motor2_Shipment` := 
                   predict(motormodel2, newdata=sales.latestyear, type="response")
]
sales.latestyear[, sqrt(sum((`Shipment(thousands)`-`Predicted_Motor2_Shipment`)^2))/.N]
##248.4337

#Log transformed -- r squre result is not better, so do not use log transformed variables for the model
#######Log10-transformed Linear Model 
#sales.latestyear[, `Logsales` := 
#                   log10((`Shipment(thousands)`))]
#sales.latestyear[, `LogIVAGDP` := 
#                   log10((`IVAGDP(PPP/Billion)`))]
#sales.latestyear[, `LogWarehouse` := 
#                   log10((`Building Stock (Mm2)`))]
#sales.latestyear[, `LogEmployment` := 
#                   log10((`Employment`))]



##########Extension to 2050##############

## Forecast for Warehouse building stock
min_building <- 0.010017251 # Parameter for cutting off linear model of household size decrease
building.tidy[, ISO.f := factor(ISO)]
building.tidy[, Region.f := factor(countrycode(ISO, origin = "iso3c", destination = "region23"))]

##liner regression for forecasting
lm.building <- lm(`Building Stock (Mm2)` ~ Year + ISO.f, data=building.tidy)

buildingISOs <- unique(building.tidy[,ISO])
building.forecast <- data.table("ISO" = buildingISOs)
building.forecast.period <- 2005:2050 # or 2018?
building.forecast <- building.forecast[rep(seq(1, length(buildingISOs)), length(building.forecast.period))] #repeat table
building.forecast[, Year := rep(building.forecast.period, each=length(buildingISOs))] # cycle through years
building.forecast[, ISO.f := as.factor(ISO)]
building.forecast[ISO %in% buildingISOs, `Building Stock (Mm2)`:= predict(lm.building, 
                                                           newdata = building.forecast[ISO %in% buildingISOs], type = "response")]
building.forecast[, `Building Stock (Mm2)` := pmax(`Building Stock (Mm2)`, min_building)] # Don't go down below preset household size


## Employment forecasting
employ.tidy <- employ.tidy[!is.na(Employment)]

employ.tidy[, ISO.f := factor(ISO)]
employ.tidy[, Region.f := factor(countrycode(ISO, origin = "iso3c", destination = "region23"))]

##liner regression for forecasting
lm.employment <- lm(`Employment` ~ Year + ISO.f, data=employ.tidy)

employISOs <- unique(employ.tidy[,ISO])
employment.forecast <- data.table("ISO" = employISOs)
employment.forecast.period <- 2005:2050 # or 2018?
employment.forecast <- employment.forecast[rep(seq(1, length(employISOs)), length(employment.forecast.period))] #repeat table
employment.forecast[, Year := rep(employment.forecast.period, each=length(employISOs))] # cycle through years
employment.forecast[, ISO.f := as.factor(ISO)]
employment.forecast[ISO %in% employISOs, `Employment`:= predict(lm.employment, 
                                                                          newdata = employment.forecast[ISO %in% employISOs], type = "response")]
employment.forecast[,c("ISO.f"):=NULL]


## IVAGDP forecasting-Forecast to 2050
rate.tidy[, ISO.f := factor(ISO)]
rate.tidy[, Region.f := factor(countrycode(ISO, origin = "iso3c", destination = "region23"))]

##liner regression for forecasting
lm.rate <- lm(`IVAGDP_Rate` ~ Year + ISO.f, data=rate.tidy)

rateISOs <- unique(rate.tidy[,ISO])
rate.forecast <- data.table("ISO" = rateISOs)
rate.forecast.period <- 2005:2050
rate.forecast <- rate.forecast[rep(seq(1, length(rateISOs)), length(rate.forecast.period))] #repeat table
rate.forecast[, Year := rep(rate.forecast.period, each=length(rateISOs))] # cycle through years
rate.forecast[, ISO.f := as.factor(ISO)]
rate.forecast[ISO %in% rateISOs, `IVAGDP_Rate` := predict(lm.rate, 
                                                           newdata = rate.forecast[ISO %in% rateISOs], type = "response")]
rate.forecast[,c("ISO.f"):=NULL]

## There are 12 countires missing IVAGDP. 10 of them has the IVA/GDP ratio for some years.


## Calculate IVAGDP to 2050 based on forecasted IVA/GDP%
GDP2060<-fread("GDP2020-2060.csv")
GDP2060<- GDP2060[,-1] # Remove first column
setnames(GDP2060,"GDP based on OECD or 5 pct Growth (billion 2017$ PPP-adjusted)","GDP(PPP/Billion)")
GDP2060 <- GDP2060[Year <=2050]
GDP2060 <- GDP2060[Year >2019]

GDP2060[,c("Country"):= NULL]


##Prepare and combine gdp data from 2005-2019
GDP2005 <- gdp.tidy[ Year <=2019] 

GDP2005.tidy <- select(GDP2005,"ISO","Year","GDP(Norminal)")

GDP2005.tidy<- GDP2005.tidy %>%
  mutate(`GDP(PPP/Billion)`= `GDP(Norminal)`/10^9)
GDP2005.tidy[,c("GDP(Norminal)"):= NULL]
GDP2005.tidy<-GDP2005.tidy[Year>2004]
GDP2005.tidy <- GDP2005.tidy[ISO != "DJI"]
GDP2005.tidy <- GDP2005.tidy[ISO != "ERI"]
GDP2005.tidy <- GDP2005.tidy[ISO != "SLB"]
GDP2005.tidy <- GDP2005.tidy[ISO != "SYR"]
GDP2005.tidy <- GDP2005.tidy[ISO != "VEN"]
GDP2005.tidy <- GDP2005.tidy[ISO != "YEM"]


GDP2060Final<- union(GDP2005.tidy,GDP2060)
GDP2060Final<-GDP2060Final[Year >2004]
GDP

##Select the missing value and back forecasting
GDP2060.missing <-GDP2060Final%>%
  filter(ISO %in% c("DJI", "ERI", "SLB","SYR", "VEN", "YEM"))

###6 Country DJI, ERI, SLB,SYR, VEN, YEM  does not have GDP before 2019. so we back forecast from 2020)
cagr.GDP <- GDP2060.missing %>%
  group_by(ISO) %>%
  mutate(lag.GDP = lag(`GDP(PPP/Billion)`),
         CAGR = (`GDP(PPP/Billion)`/ lag.GDP) - 1)

cagr.GDP <- cagr.GDP[cagr.GDP$Year %in% c(2020:2050),] ##########reference years for CAGR averaging
cagr.GDP[is.na(cagr.GDP)]<-0
cagr.GDP <- aggregate(CAGR ~ ISO, data = cagr.GDP, FUN = mean)
cagr.GDP$CAGR[cagr.GDP$CAGR<0] <- -(cagr.GDP$CAGR[cagr.GDP$CAGR<0]/2) ################safe mechanism to avoid reversed trend
cagr.GDP$CAGR[cagr.GDP$CAGR=="Inf"]<-0

GDP2060.missing.P <- GDP2060.missing[GDP2060.missing$Year==2020,]
mat0 <- data.frame(ISO=0, Year=0)
for(Year in c(2005:2020)){ ##############years to be filled out, including last year available
  call1 <- unique(GDP2060.missing$ISO)
  mat <- data.frame(ISO=call1,Year=Year)
  mat0 <- rbind(mat0, mat)
}
mat0<-mat0[-1,]
GDP2060.missing.P <- merge(GDP2060.missing.P, mat0, all = T, by=c("ISO","Year"))
GDP2060.missing.P <- merge(GDP2060.missing.P, cagr.GDP, all = T, by=c("ISO"))
GDP2060.missing.P <- GDP2060.missing.P %>%
  group_by(ISO) %>%
  mutate(base = `GDP(PPP/Billion)`[Year=="2020"]) ########reference year (2020?)
GDP2060.missing.P$Year<-as.numeric(GDP2060.missing.P$Year)
GDP2060.missing.P$`GDP(PPP/Billion)` <- (GDP2060.missing.P$base *
                               ((1 + GDP2060.missing.P$CAGR)^(GDP2060.missing.P$Year-2020))) ########reference year (2020?)
GDP2060.missing.P$base<-NULL
GDP2060.missing.P$CAGR<-NULL

GDP2020_2060 <- merge(GDP2060, GDP2060.missing.P, all = T)
GDP2050 <- merge(GDP2020_2060,GDP2005.tidy,all = T)


GDP2050 <- rate.forecast [GDP2050, on=c("ISO", "Year")]


GDP2050[,`IVAGDP(PPP/Billion)`:=
          `GDP(PPP/Billion)`*`IVAGDP_Rate`/100]

#write.csv(GDP2050,"Master GDP 2005-2050.csv")

## Integrate three variables together
forecast.tidy <- GDP2050 %>%
  dplyr::select("ISO","Year","IVAGDP(PPP/Billion)")


forecast.tidy <- building.forecast[forecast.tidy, on=c("ISO", "Year")]
forecast.tidy <- employment.forecast[forecast.tidy, on=c("ISO", "Year")]
forecast.tidy[,`Employment`:= `Employment`/100,by=c("Year", "ISO")]

forecast.tidy[,c("ISO.f"):= NULL]#Delete extra column

forecast.tidy[, `Predicted_Motor_Shipment` :=
                      (predict(motormodel, newdata=forecast.tidy, type="response"))]

Motor.summary <- forecast.tidy[PAMS_countries, 
                              .("Country"=i.Country, ISO, Year, `Predicted_Motor_Shipment`),
                                           on = "ISO"][Year <=2050]

Motor.summary <- merge(Motor.summary, country_sales[, .(ISO, Year, `Motor_Shipment(Omdia)`=
                                                                      `Shipment(thousands)`)], all=TRUE) 

plot_countries <- c("USA", "IND", "IDN", "CHN", "ZAF", "KEN", "GBR", "DEU", "BRA")
#plot_countries <- PAMS_countries[1:81, ISO]
#plot_countries <- PAMS_countries[82:162, ISO]

cols <- c("Omdia"="blue", "Forecast"="black")
p <- ggplot(Motor.summary[ISO %in% plot_countries], aes(x=Year))+
  geom_point(aes(y=`Motor_Shipment(Omdia)`, col="Omdia"))+
  geom_line(aes(y=`Predicted_Motor_Shipment`, col="Forecast"))+
  facet_wrap(~ISO)+
  scale_color_manual(name="Motor Sales", 
                     values = cols) +
  theme(legend.position="bottom") +
  ylab("Motor Sales") +
  scale_x_continuous(breaks=c(2000,2020,2040), labels=c("2000","'20","'40"), 
                     limits=c(1990,2050))
p

#### Compile dataset for integration
compile.motor <-Motor.summary
compile.motor <-left_join(compile.motor, forecast.tidy, by=c("Year","ISO","Predicted_Motor_Shipment"))

compilecountry<- compile.motor[Predicted_Motor_Shipment<0]
compilecountry[,c("Predicted_Motor_Shipment") :=NULL]

compilecountry[, `Predicted_Motor2_Shipment` :=
                maxshipment*(predict(betamotor, newdata=compilecountry, type="response"))]


write.csv(compilecountry,"Compiled data for missing motor.csv")


forecast.tidy.motor<-compile.motor
forecast.tidy.motor <- building.forecast[forecast.tidy.motor, on=c("ISO", "Year")]
forecast.tidy.motor <- employment.forecast[forecast.tidy.motor, on=c("ISO", "Year")]
forecast.tidy.motor[,`Employment`:= `Employment`/100,by=c("Year", "ISO")]

forecast.tidy.motor <- GDP2060Final[forecast.tidy.motor, on=c("ISO", "Year")] ##gdp2060 only includes data starting from 2020
forecast.tidy.motor <- forecast.tidy.motor[Year>=2005]
forecast.tidy.motor[,c("ISO.f","i.IVAGDP(PPP/Billion)") := NULL]
forecast.tidy.motor<- Motor.summary[forecast.tidy.motor,on=c("ISO", "Year","Country")]
write.csv(forecast.tidy.motor,"Compiled dataset for Motor.csv")