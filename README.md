# Chinese-Cities-PM2.5-Analysis
Analysis of PM2.5 levels in 5 Chinese Cities
This is my analysis of particulate matter PM2.5 levels in 5 Chinese Cities namely: Beijing, Chengdu, Guangzhou, Shanghai and Sheyang.
The datasets cover form 2010 to 2016
The original datasets(with a "PM2.5" suffix) had a rather high proportion of missing data or NA especially for the PM2.5 readings. 
The data went through a process of data imputation using the MICE R package. The imputations were done using the predictive mean matching technique.
The imputed data were written to csv files ("Complete" suffix) which were used for the analysis.
Exploratory Data Analysis was done on the 5 new datasets to remove outliers and prepare to fit a linear model.
Also a Principal Component Analysis carried out on the Beijingcomplete data for predictive purposes.
The Variables in the Data sets are listed below:

Context
PM2.5 readings are often included in air quality reports from environmental authorities and companies. PM2.5 refers to atmospheric particulate matter (PM) that have a diameter less than 2.5 micrometers. In other words, it's used as a measure of pollution.

Content
The time period for this data is between Jan 1st, 2010 to Dec 31st, 2015. Missing data are denoted as NA.

No: row number
year: year of data in this row
month: month of data in this row
day: day of data in this row
hour: hour of data in this row
season: season of data in this row
PM: PM2.5 concentration (ug/m^3)
DEWP: Dew Point (Celsius Degree)
TEMP: Temperature (Celsius Degree)
HUMI: Humidity (%)
PRES: Pressure (hPa)
cbwd: Combined wind direction
Iws: Cumulated wind speed (m/s)
precipitation: hourly precipitation (mm)
Iprec: Cumulated precipitation (mm)

source: kaggle.com
