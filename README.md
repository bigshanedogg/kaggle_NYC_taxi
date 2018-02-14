---
title: "Kaggle_taxi_prediction"
author: "Hodong Lee"
date: '2017 12 30 '
---

# Simple visualization for XGB
17.12.30

- <a href="#1">1. Introduction</a>
    + <a href="#1.1">1.1 Description</a><br>
    + <a href="#1.2">1.2 Data & Library import</a><br>
    + <a href="#1.3">1.3 Simple transformation before analysis</a><br>
- <a href="#2">2. Data visualization</a>
    + <a href="#2.1">2.1 Factor features : vendor_id, store_and_fwd_flag</a><br>
    + <a href="#2.2">2.2 Numeric features : passenger_count, haversine distance</a><br>
    + <a href="#2.3">2.3 Datetime features : month, weekday, hour</a><br>
    + <a href="#2.4">2.4 Location features : longitude & latitude </a><br>
- <a href="#3">3. Data preprocessing</a>
- <a href="#4">4. Xgboost model & Result</a>
    + <a href="#4.1">4.1 Parameter setting with cross validation </a><br>
    + <a href="#4.2">4.2 Final model & result </a><br>

<br><hr><br>

Currently, the rmsle of the predicted result for model with eta = 0.03 and nrounds = 300 is 0.402 on the private leader board. We can check if the weather affects with monthly trends you have seen above or use Airport coordinates, landmark coordinates, and subway station coordinates with observation of difference in average trip_duration per centroid in the cluster analysis. The accuracy of the model can be improved by mashing up such data.

<br><hr><br>

(This analysis can be seen more clearly on the following kaggle kernal page.
https://www.kaggle.com/bigshane/simple-visualization-for-xgb)
<br><br><br><br><br>
