# 🏀 NBA MVP Predictor

This project builds a machine learning model in R to predict the NBA Most Valuable Player (MVP) using historical player statistics. By analyzing key performance metrics, this model identifies the most likely MVP winner based on past trends.

## 📌 Overview

The goal of this project is to predict the NBA MVP by leveraging key advanced statistics, including player efficiency, team performance, and individual contributions. The model is trained on past MVP data and tested on recent seasons to evaluate its accuracy.

## 📊 Dataset & Features

- Data Source: [Kaggle](https://www.kaggle.com/datasets/dbtjdals/nba-mvp-candidates-1980-2022)

- Final Features Used in the Model:

  - value_over_replacement_player (VORP) – Measures a player’s impact compared to a replacement-level player.

  - WS_per_48 – Win Shares per 48 minutes, an estimate of a player’s contribution to winning.

  - player_efficiency_rating (PER) – Overall measure of a player's per-minute productivity.

  - PTS – Points per game.
  
  - seed – Team’s playoff seeding, representing overall team success.
 
  - free_throw_attempt_rate (FTA Rate) – Indicator of scoring aggressiveness.

  - offensive_box_plus_minus (OBPM) – Measures a player's offensive impact relative to league average.

  - win_loss_ratio – Team’s win-to-loss ratio, accounting for team success.

  - usage_percentage (USG%) – Percentage of team plays used by the player while on the court.

## 🛠️ Tools & Technologies

- Programming Language: R
- Machine Learning Models: Linear Regression, Ridge Regression, Random Forest, XGBoost
- Data Manipulation: dplyr, tidyr
- Visualization: ggplot2
- Libraries Used: tidyverse, gsheet, ggplot2, ggcorrplot, caret, randomForest, xgboost, Metrics

## 🚀 Results & Insights

- The XGBoost model provided the best performance, showing strong predictive accuracy.
- VORP, WS_per_48, PER, and win-loss ratio were the most influential factors in MVP selection.
- The model correctly identified past MVP winners with high accuracy, reinforcing the importance of team success and advanced player efficiency metrics.

## 🔗 Related Links

- Live Project: [NBA MVP Predictor](https://steven-martinez-colon.github.io/projects/nba-mvp.html)
- Data Source: [Kaggle](https://www.kaggle.com/datasets/dbtjdals/nba-mvp-candidates-1980-2022)

## 📩 Contact

🔗 LinkedIn: [Steven Martinez](https://www.linkedin.com/in/steven-martinez-colon/)

💻 Portfolio: [steven-martinez-colon.github.io](https://steven-martinez-colon.github.io)

