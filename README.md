# Employee_Selection
A statistical report reviewing applicants for a job using R and several R packages to load, clean, wrangle, and create a regression models to predict best hires based on multiple attributes. The final model produced an R-squared coefficient of 0.35 and met legal standards with an adverse impact ratio of 0.85. The model was created on training split of 75% of the data and cross validated on the remaining 25% of the data. When compared to the No-Information Rate, the model had a 13% higher true positive rate (38%), 3% higher true negative rate (78%) and a far superior selection ratio of 0.25 compared to 1.00. As hypothesized, using the regression model to is stronger in everyway than using a data-less model. It should also be noted that legal hiring issues according to adverse impact issues were still present when both fixed and sliding bands were used. The figure below provides a predicted vs. actual scatterogram using percentiles for all applicants. see the full report `Globex Selection Report.docx` for more information.

![image](https://github.com/Jwychor/Employee_Selection/blob/master/Expectancy%20Scatterogram.png)

*Note that TP = True Positive, TN = True Negative, and SR = Selection Ratio. Numbers inside each dotted line represent the number of people inside the rectangular section.
