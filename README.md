# Chinese-Cities-PM2.5-Analysis
Analysis of PM2.5 levels in 5 Chinese Cities
This is my analysis of particulate matter PM2.5 levels in 5 Chinese Cities namely: Beijing, Chengdu, Guangzhou, Shanghai and Sheyang.
The datasets cover form 2010 to 2016
The original datasets(with a "PM2.5" suffix) had a rather high proportion of missing data or NA especially for the PM2.5 readings. 
The data went through a process of data imputation using the MICE R package. The imputations were done using the predictive mean matching technique.
The imputed data were written to csv files ("Complete" suffix) which were used for the analysis.
Exploratory Data Analysis was done on the 5 new datasets to remove outliers and prepare to fit a linear model.
Also a Principal Component Analysis carried out on the Beijingcomplete data for predictive purposes.
