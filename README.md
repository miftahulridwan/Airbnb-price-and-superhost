# Airbnb Price Determinant and Superhost Prediction
This project is an assignment for Programming with R course in Master of Data Science and Society program, Tilburg University. In this project, we investigate the determinant of price category as well as superhost prediction using Airbnb dataset for [Hawaii area](http://data.insideairbnb.com/united-states/hi/hawaii/2019-11-01/visualisations/listings.csv "Airbnb data for Hawaii Area as of November 2019") as of November 2019. 

## Table of Directory
1. [Code](https://github.com/miftahulridwan/airbnb-price-and-superhost/tree/master/code)
2. [Figures](https://github.com/miftahulridwan/airbnb-price-and-superhost/tree/master/figs)
3. [Report](https://github.com/miftahulridwan/airbnb-price-and-superhost/tree/master/report)

## Project Summary
The aim of this project is to understand the variables which are contribute to Airbnb rental price and try to predict superhost (who are rated 4.8 out of 5 and response rate is at least 90%) based on all available variables and selected variables based on feature selection process using random forrest technique. Superhost prediction is essential to help property owners as well as Airbnb to focus on increasing key variables that leads to higher rating and thus higher consumer satisfaction. Our result is dependent to the dataset we are using.
<br>

In this project, we find that our feature selection model produce marginal improvement in recall score (81.7%) compare to baseline model (81.59%). We also found that owner responsiveness, strict cancellation policy and property cleanliness play pivotal role in predicting the superhost status.
<br>

Furthermore, we find that our feature selection model also produce slight higher accuracy (61.34%) than the baseline model (59.93%). Variables such as: type of bathrooms, number of bedrooms, number of bed available, and type of cancellation policy are among the most important features that predict the price. The full report are available [here](https://github.com/miftahulridwan/airbnb-price-and-superhost/blob/master/report/Group%2031.pdf "Full Report")


## Environment and Library
We deploy our [code](https://github.com/miftahulridwan/airbnb-price-and-superhost/blob/master/code/group%2031_project.R) in R using several libraries, such as:
1. Dplyr
2. Tidyr
3. Ggplot2
4. Caret
5. mice
6. ranger
