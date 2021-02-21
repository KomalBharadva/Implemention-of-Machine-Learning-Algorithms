## Setting present working directory
setwd("D:/NCI/DMML1/Project/Datasets")
getwd()

## Including all necessary libraries
library(dplyr) # DataFrame Manipulation
library(lubridate)
library(ggplot2)
library(sqldf)
library(tidyverse) # metapackage with lots of helpful functions
library(readr)
library(xts)
library(highcharter)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(ggrepel)
library(cowplot)
library(plotly)
library(gganimate)
library(geosphere)
library(DT)
library(openair)
library(corrplot)
library(viridisLite)
library(viridis)
library(ggdark)
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)
library(caTools)
library(party)
library(partykit)
library(tseries)
library(forecast)

## Reading CSV File
Covid19_df <- read.csv("COVID-19.csv", header=T, na.strings=c(""), stringsAsFactors = T)

dim(Covid19_df) # Checking dimension which is number of rows and columns
names(Covid19_df) # Checking 
class(Covid19_df) # This shows the class or type of object
str(Covid19_df)  # This tells us about the structure of dataframe
summary(Covid19_df) # This provides statistics summary of dataframe
head(Covid19_df) # To see first few rows of dataframe

## Data Cleaning
sapply(Covid19_df, function(x) sum(is.na(x))) # Checking for missing values column wise

# In this dataset we can see some columns are not required so we drop them.
Covid19_df <- subset( Covid19_df, select = -c(geoId, countryterritoryCode, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000))

sapply(Covid19_df, function(x) sum(is.na(x)))

popData2019NAs <- Covid19_df %>%
  filter(countriesAndTerritories == "Cases_on_an_international_conveyance_Japan" 
         | countriesAndTerritories == "Wallis_and_Futuna")

# We have seen the column popData2019 is having 90 empty rows which can be dropped since it has only 2 unique record in country column
print(popData2019NAs)

sapply(Covid19_df, function(x) sum(is.na(x)))

# Here we are deleting rows having NA values.
Covid19_df <- na.omit(Covid19_df)

sapply(Covid19_df, function(x) sum(is.na(x)))

Covid19_df <- Covid19_df %>% 
  rename(
    country = countriesAndTerritories,
    continent = continentExp,
    population = popData2019,
    date = dateRep
  )

summary(Covid19_df)

# In cases and deaths columns we can see the min value is in negative which is not possible, so we will remove the rows having negative values in them.

Covid19_df <- Covid19_df[Covid19_df$cases >= 0, ]
Covid19_df <- Covid19_df[Covid19_df$deaths >= 0, ]

#Converting country and continent columns in character
Covid19_df[["country"]] <- as.character(Covid19_df[["country"]])
Covid19_df[["continent"]] <- as.character(Covid19_df[["continent"]])
Covid19_df <- transform(Covid19_df, deaths = as.numeric(deaths), 
                        cases = as.numeric(cases))
Covid19_df1 <- data.frame(Covid19_df)
Covid19_df1$date <- as.Date(Covid19_df1$date, "%d-%m-%Y")

str(Covid19_df)
summary(Covid19_df)
head(Covid19_df)


## Exploratory Data Analysis
#EDA-1
Covid19_df_eda1<-data.frame(Covid19_df)
Covid19_df_eda1$date <- parse_date_time(Covid19_df$date, "%d/%m/%y")

asia <- Covid19_df_eda1 %>% filter(continent=="Asia")
country <- Covid19_df_eda1 %>% group_by(country)
india <- filter(asia,country=="India") %>% select(-'continent')
india$date <- ymd(india$date)
india <- as.data.frame(india)

india_day <- india %>%
  group_by(date) %>%
  summarize(Confirmed = max(cases),Death = max(deaths))

ggplot(data = india_day, aes(x = date , y = Confirmed))+
  geom_bar(stat="identity",fill = "darkgreen",alpha=0.8)+
  geom_point(color="red",size=2)+
  labs(x = 'Date', y = 'Count', 
       title = 'Confirmed cases in India') +
  theme_bw()


#EDA-2
Covid19_df_eda2 = subset(Covid19_df, !(continent == 'Oceania'))
Covid19_df_eda2$date <- as.Date(Covid19_df_eda2$date, "%d-%m-%Y")

Conf <- Covid19_df_eda2 %>%
  group_by(continent, date) %>%
  summarise(x = sum(cases), .groups = 'drop')

ggplot(Conf, aes(date, x, colour = continent))+
  geom_line(size = 1.9, alpha = 0.85)+
  scale_y_continuous(trans="log10", labels = comma)+
  labs(x = "", y = "Number of confirmed cases (logarithmic scale)", title =  "Confirmation of virus infection (active + past)", 
       subtitle = "by East Asia, Europe, USA and Rest of World")+
  geom_vline(xintercept = as.Date("2020-03-11"), linetype = "longdash", size = 0.25, col = "gray30")+
  geom_label_repel(data = Conf[Conf$date == max(Conf$date),], aes(label = paste0(round(x/1000000,2), " m"), colour = continent), hjust = -0.1, fontface = "bold", size = 4.9, alpha = 0.85, show.legend = F)+
  annotate("text", x = as.Date("2020-05-05"), y = 110, label = "WHO announces \n pandemic", size = 4.8)+
  annotate(geom = "curve", x = as.Date("2020-04-25"), y = 32, xend = as.Date("2020-03-14"), yend = 30, curvature = -0.3, arrow = arrow(length = unit(5, "mm")))+
  scale_x_date(date_labels = "%b %d", date_breaks = "70 days", limits = c(min(Conf$date), max(Conf$date)+0))+
  scale_colour_brewer(palette = "Set1")+
  theme_fivethirtyeight()+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 14), plot.caption = element_text(color = "gray20", face = "italic"),
        legend.text = element_text(size = 15), axis.title = element_text(size = 15), axis.line = element_line(size = 0.4, colour = "grey10"),
        plot.background = element_rect(fill = "#d3dae6"), legend.background = element_rect(fill = "#d3dae6"), legend.key = element_rect(fill = "#d3dae6"))


# EDA-3
Dea <- Covid19_df_eda2 %>%
  group_by(continent, date) %>%
  summarise(x = sum(deaths), .groups = 'drop')

Tog2 <- cbind(Dea, Conf)
Tog2 <- Tog2[,c(1,2,3,6)]
names(Tog2)[3:4] <- c("Deaths", "Total")
Tog2$Dea2All <- Tog2$Deaths/Tog2$Total
Tog2 <- na.omit(Tog2)
Tog2 <- Tog2[Tog2$date > '2020-03-09', ]

ggplot(Tog2, aes(date,Dea2All, colour = continent))+
  geom_line(size = 2.2, alpha = 0.85)+
  labs(x = "", y = "Mortality rate", title =  "Virus mortality", subtitle = "by East Asia, Europe, USA and Rest of World (since Mar 10)", 
       colour = "")+
  geom_label_repel(data = Tog2[Tog2$date == max(Tog2$date),], aes(label = paste0(round(Dea2All*100,1), " %"), colour = continent), hjust = -0.1, fontface = "bold", size = 4.9, alpha = 0.85, show.legend = F)+
  scale_y_continuous(labels = scales::percent, limits = c(0,0.105))+
  scale_x_date(date_labels = "%b %d", date_breaks = "52 days", limits = c(min(Tog2$date), max(Tog2$date)+0))+
  scale_colour_brewer(palette = "Set1")+
  theme_fivethirtyeight()+
  theme(legend.position="bottom", legend.direction="horizontal", axis.text = element_text(size = 14), axis.title = element_text(size = 15), plot.caption = element_text(color = "gray20", face = "italic"),
        legend.text = element_text(size = 14), axis.line = element_line(size = 0.4, colour = "grey10"), legend.key = element_rect(fill = "#d3dae6"),
        plot.background = element_rect(fill = "#d3dae6"), legend.background = element_rect(fill = "#d3dae6"))


#EDA-4
Covid19_df %>%
  select('country','date','cases','deaths') %>%
  arrange(desc(cases)) %>%
  datatable(
    rownames = FALSE,
    fillContainer = TRUE,
    options = list(
      bPaginate = FALSE)
  )

#EDA-5
Covid19_df_eda5 = subset(Covid19_df, (country == 'Ireland'))
head(Covid19_df_eda5)

cases_total_date <- Covid19_df_eda5 %>%
  group_by(date) %>%
  summarise(Confirmed = sum(cases),
            Deaths = sum(deaths)) %>%
  mutate("New_Cases" = Confirmed - lag(Confirmed, 1))
head(cases_total_date)
dim(cases_total_date)
cases_total_date <- na.omit(cases_total_date)

confirmed <- cases_total_date[,"Confirmed"]

date <- seq(from=as.Date('2020-01-01'),
            by=1,
            to = as.Date('2020-11-08'))

calendarPlot(data.frame(confirmed, date), pollutant = 'Confirmed', year = 2020, main = "Confirmed Cases of Ireland")


#EDA-6
deaths <- cases_total_date[,"Deaths"]
calendarPlot(data.frame(deaths, date), pollutant = 'Deaths', year = 2020, main = "Deaths in Ireland", cols = "RdGy")


#EDA-7
Covid19_df_eda7<-data.frame(Covid19_df)
Covid19_df_eda7$date <- as.Date(Covid19_df_eda7$date, "%d-%m-%Y")
cases_latest <- Covid19_df_eda7 %>%
  group_by(country, date) %>%
  summarise(Confirmed  = sum(cases),
            Deaths = sum(deaths)) %>%
  mutate("New Cases" = Confirmed - lag(Confirmed, 1) ) %>%
  filter(date == max(date))

top_10_confirmed <- cases_latest %>%
  select('country', Confirmed) %>%
  arrange(desc(Confirmed))

top_10_confirmed[1:10,] %>%
  ggplot(aes(x = reorder(country,Confirmed), y = Confirmed )) +
  geom_bar(stat = "identity", fill  = "red", width = 0.8) +
  theme_economist() +
  scale_y_continuous(breaks = seq(0, 200000, by = 20000), labels = comma) +
  coord_flip() +
  labs(x = "", y = "", title = "Top 10 (the Most Confirmed Cases)") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 11, face = "bold"))


#EDA-8
top_10_Deaths <- cases_latest %>%
  select(country, Deaths) %>%
  arrange(desc(Deaths))

top_10_Deaths[1:10,] %>%
  ggplot(aes(x = reorder(country,Deaths), y = Deaths )) +
  geom_bar(stat = "identity", fill  = "blue", width = 0.8) +
  theme_economist() +
  scale_y_continuous(breaks = seq(0, 2000, by = 150), labels = comma) +
  coord_flip() +
  labs(x = "", y = "", title = "Top 10 (the Most Deaths)") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.title = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 11, face = "bold"))


#EDA-9
Covid19_df_eda9 <- na.omit(Covid19_df)
filtered <- filter(Covid19_df_eda9, Covid19_df_eda9$date==max(Covid19_df_eda9$date)) %>% group_by(country) %>% summarise(Confirmed =  sum(cases) ,Deaths = sum(deaths))
filtered <- filtered[order(filtered$Confirmed,decreasing = TRUE),]
datatable(filtered)


#EDA-10
Covid19_df_eda10 <- data.frame(Covid19_df)
new_df <- Covid19_df_eda10 %>% filter(date == max(Covid19_df_eda10$date))
M <- cor(new_df[, 5:6])
corrplot(M, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.6, cl.cex = 0.6)  #Correlogram


##Applying ML Algorithms
Covid19_df$cases <- log(Covid19_df$cases + 1)
num_folds <- trainControl(method = "cv", number = 5) # Specify 5-fold cross-validation.
parameter_grid <- expand.grid(.cp = seq(0, 0.01, 0.001)) # Explore values of `cp` b/w 0 and 0.01.

set.seed(123)
sample_data = sample.split(Covid19_df, SplitRatio = 0.75)
train_data <- subset(Covid19_df, sample_data == TRUE)
test_data <- subset(Covid19_df, sample_data == FALSE)

grid_search_1 <- train(
  cases ~ country + continent + day + month + year + population,
  data = train_data, 
  method = "rpart", # CART algorithm
  trControl = num_folds, 
  tuneGrid = parameter_grid
)

print(grid_search_1)

tree_1 <- rpart(
  cases ~ country + continent + day + month + year + population,
  data = train_data, 
  cp = 0
)

tree_1_summary <- data.frame(
  variable = names(tree_1$variable.importance), 
  importance = tree_1$variable.importance, 
  stringsAsFactors = FALSE, 
  row.names = NULL
) %>% 
  arrange(desc(importance))

tree_1_summary

train_data$Pred_1 <- exp(predict(tree_1, newdata = train_data)) - 1

summary(train_data$Pred_1)

RMSLE <- sqrt(mean((log(train_data$Pred_1 + 1) - log(train_data$cases + 1))^2))
RMSLE

library(Metrics)
RMSE_dt = RMSE(train_data$cases, train_data$Pred_1)
round(RMSE_dt, 3)

MAE_dt = mae(train_data$cases, train_data$Pred_1)
round(MAE_dt, 3)

MASE_dt = mase(train_data$cases, train_data$Pred_1)
round(MASE_dt, 3)

test_data$cases <- exp(predict(tree_1, newdata = test_data)) - 1

## Time Series Modeling
# Exponential Forecasting Model Fitting
Covid19_df$date <- as.Date(Covid19_df$date)

options(repr.plot.width = 8, repr.plot.height = 8)
info_cov <- arrange(Covid19_df,date)%>%
  group_by(date)%>% 
  summarize(deaths=sum(deaths),case=sum(cases))

ts_info_cov<-ts(diff(info_cov$case), start = c(1), frequency = 15)

# Smoothing a time series by Simple moving averages
plot(ma(ts_info_cov,2))

### Simple Exponential smoothing in R
autoplot(ts_info_cov)

decompose_info_cov <- decompose(ts_info_cov)
plot(decompose_info_cov)

# Let's investigate the series that we get after removing seasonality
ts_info_cov_sea <- ts_info_cov - decompose_info_cov$seasonal
plot(ts_info_cov_sea)

model<-HoltWinters(ts_info_cov)
plot(model,main="fitting a model to the daily cases")

# Let's also see the residuals and their plots
total_cases<-forecast(model,10)
acf(na.omit(resid(total_cases)), lag.max=20)

# Here we ran Ljung-Box test for p-value. In this case p value is significant therefore we cannot reject null hypothesis which clearly implies there is no association between errors
Box.test(total_cases$residuals, lag=20, type="Ljung-Box")

#Lets check third order exponential forecasting model
autoplot(total_cases,fcol = "red") + geom_forecast(h=10) + theme_classic()+labs(title="Covid-19 Cases Prediction using Exponential Forecasting")+xlab("Period")+ylab("Case Count")

#Check the order of differencing required
ndiffs(ts_info_cov)
# d=0
#ACF/PACF plots. Choosing p and q
Acf(ts_info_cov)
Pacf(ts_info_cov)
adf.test(ts_info_cov)

#Fitting an auto ARIMA model
fit_arima <- auto.arima(ts_info_cov)
fit_arima

#Evaluating Model Fit
qqnorm(fit_arima$residuals)
qqline(fit_arima$residuals)
Box.test(fit_arima$residuals, type="Ljung-Box")
checkresiduals(fit_arima)
round(accuracy(fit_arima), 2)

#Forecasting with the fitted model
forecast(fit_arima, 15)
plot(forecast(fit_arima, 15), xlab="Period", ylab="Case Count")

