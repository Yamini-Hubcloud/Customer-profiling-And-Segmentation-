# Customer-profiling-And-Segmentation-
Customer Segmentation Using RFM Analysis and K-Means Clustering
This project focuses on customer segmentation by leveraging a combination of RFM (Recency, Frequency, Monetary) analysis and K-Means clustering, implemented in R. The goal is to identify distinct customer groups based on their purchasing behavior to support targeted marketing strategies and improve customer relationship management.

🔍 Project Overview
RFM Analysis:
The RFM model quantifies customer behavior in terms of:

Recency: How recently a customer made a purchase

Frequency: How often they purchase

Monetary: How much they spend

These metrics help identify high-value customers, at-risk customers, and more. After computing RFM scores for each customer, the data was normalized for clustering.

K-Means Clustering:
K-means clustering was applied to the scaled RFM data to group customers with similar purchasing patterns. The optimal number of clusters was determined using methods like the Elbow Method and Silhouette Analysis.

📈 Outcomes
The resulting clusters were interpreted and profiled to identify customer segments such as:

Loyal High-Spenders

Frequent Low-Spenders

Infrequent High-Spenders

At-Risk or Churned Customers

Visualizations such as cluster plots, boxplots, and heatmaps were used to explore and present the segments.

🎯 Applications
This segmentation can help businesses:

Personalize marketing campaigns

Increase customer retention

Allocate resources efficiently

Predict customer lifetime value
