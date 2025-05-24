# SAR_Analysis
# 🚗 SAR Car Rental Cancellation Prediction  This project analyzes car booking behavior and predicts ride cancellations using the SAR dataset, which contains 10,000+ historical booking records from San Francisco Auto Rental. The objective is to improve service reliability and support operational decision-making through predictive analytics.  

---

## 🎯 Objective

- Predict the likelihood of a car booking being cancelled
- Understand key factors driving cancellations
- Deliver actionable insights to optimize operations and customer satisfaction

---

## 🛠️ Tools & Technologies

- **Language:** R
- **Libraries:** dplyr, ggplot2, caret, randomForest, e1071, mice, VIM, neuralnet, glmnet, ROCR
- **Methods:** Data cleaning, feature engineering, outlier handling, imputation, chi-square testing, clustering

---

## 🔍 Key Features Engineered

- **Distance calculation** using Haversine formula
- **Waiting time** (from booking to trip)
- **Channel usage indicators** (online, mobile, others)
- **Temporal breakdown** (day of week, hour of booking)

---

## 🤖 Models Used

- Naive Bayes – 77% accuracy, balanced performance  
- Logistic Regression (LASSO) – 93% accuracy, overfit risk  
- Decision Tree – 91.2% accuracy, interpretable model  
- K-Nearest Neighbors – 92.5% accuracy, strong recall  
- Random Forest – **Best performer** at 93.5% accuracy  

---

## 📈 Insights

- **Mobile bookings** had the lowest cancellation rates (~0.7%)
- **Vehicle models 91 & 89** had the highest cancellation issues
- **Peak cancellation times**: Evening hours (9–10 PM) and weekends
- **Longer trips** were less likely to be cancelled

---

## 🚀 Recommendations

- Proactively monitor high-risk booking periods
- Reevaluate underperforming vehicle models and drivers
- Implement a “reliability rewards” system to reduce cancellations

---

## 👨‍💻 Author

**Phanidhar Venkata Naga Kasuba**  
MS in Data Analytics, Webster University  
📫 Email: pkasubavenkatana@webster.edu  
🔗 [LinkedIn](www.linkedin.com/in/phanidhar-kasuba-venkata-naga)

---
