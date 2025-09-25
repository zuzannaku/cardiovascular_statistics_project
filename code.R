#Project: Health factors and cardiovascular diseases

# This project explores the relationships between health and lifestyle factors 
# (such as cholesterol level, physical activity, glucose levels or BMI) 
# and the presence of cardiovascular diseases.
# Such findings are crucial in guiding preventive healthcare or shaping public health policies. 
# Additionally, these can help businesses such as insurance providers or fitness and wellness companies.

# Hypotheses to be tested:
# - BMI and cardiovascular disease - H₀: Mean BMI is the same for patients with and without cardiovascular disease.
# - Cholesterol and cardiovascular disease - H₀: Cholesterol level is independent of cardiovascular disease.
# - Age and glucose levels - H₀: Mean age is the same across glucose level groups.

# The project pipeline contains:
# 1. Exploratory data analysis - in order to deeply understand the data
# 2. Data preprocessing - cleaning the data from potential outliers, duplicated values, dealing with incorrect data types and irrelevant features.
# 3. Hypotheses test with Assumptions check.
# 4. Interpretation of the results.

# Data source: The data set was acquired from Kaggle.

# Note: No sampling was performed, as the dataset size (~70k rows) is manageable 
# and provides sufficient statistical power without sub-sampling.

############################################################################

# Part 1: Exploratory data analysis

df <- read.csv("https://raw.githubusercontent.com/zuzannaku/cardiovascular_statistics_project/refs/heads/main/cardio_data.csv")

head(df)
str(df)
summary(df)
dim(df)
names(df)

hist(df$cholesterol, main="Cholesterol", xlab="Cholesterol", col="lightblue")
hist(df$height, main="Height", xlab="Height", col="lightcoral", xlim=c(120, 200), breaks=30)
hist(df$age_years, main="Age", xlab="Age", col="lightgray")

barplot(table(df$gender), main="Gender Distribution: 1 - Female, 2 - Male", col="orange")
barplot(table(df$gluc), main="Glucose Level Distribution", col="red")

boxplot(cholesterol ~ bp_category, data=df,
        main="Cholesterol by Blood pressure category", col="lightblue", las=2, cex.names=0.5)

boxplot(bmi ~ active, data=df,
        main="BMI by activity", col="lightgreen")

barplot(table(df$bp_category), main="Blood pressure category", col="purple", las=2, cex.names=0.5)

sum(duplicated(df))

na_counts <- sapply(df, function(x) sum(is.na(x)))
na_counts[order(na_counts, decreasing = TRUE)]

# counts for some binary features
table(df$alco)
table(df$smoke)
table(df$cardio)

# correlations for numerical features
cor(df[, sapply(df, is.numeric)], use="complete.obs")
# and a correlation matrix for all numerical features 
num<-Filter(is.numeric, df)
cormatrix<-cor(num, use = "complete.obs") 
library(ggcorrplot)
ggcorrplot(cormatrix, lab=TRUE)

# Part 2: Data preprocessing

df_raw <- df

# dropping irrelevant features
drop_cols <- c("id", "bp_category_encoded", "age")
keep_cols <- setdiff(names(df), drop_cols)
df <- df[ , keep_cols]

# decode the binary feature: gender to keep it as a factor data type
df$gender <- factor(df$gender, levels = c(1, 2), labels = c("Female", "Male"))

# changing binary flags to factors
bin_cols <- c("smoke", "alco", "active", "cardio")
for (v in bin_cols) df[[v]] <- factor(df[[v]], levels = c(0,1), labels = c("No","Yes"))

# adding oridinality to cholesterol and gluc features
df$cholesterol <- factor(df$cholesterol, levels = c(1,2,3),
                         labels = c("Normal","Above","WellAbove"), ordered = TRUE)
df$gluc <- factor(df$gluc, levels = c(1,2,3),
                  labels = c("Normal","Above","WellAbove"), ordered = TRUE)

# changing bp_category into a factor
df$bp_category <- factor(df$bp_category)

# handling outliers
# height (120–220 cm), weight (30–250 kg), BMI (10–70)

ok_h <- df$height >= 120 & df$height <= 220
ok_w <- df$weight >= 30  & df$weight <= 250
ok_b <- df$bmi    >= 10  & df$bmi    <= 70
df <- df[ ok_h & ok_w & ok_b, ]

# blood pressure check for irrationality
ok_hi <- df$ap_hi >= 90  & df$ap_hi <= 240
ok_lo <- df$ap_lo >= 60  & df$ap_lo <= 140
ok_order <- df$ap_hi >= df$ap_lo
df <- df[ ok_hi & ok_lo & ok_order, ]

df_clean <- df

# Part 3: Hypotheses tests with assumptions checks


########################################################################

# H₀: Mean BMI is the same for patients with and without cardiovascular disease.
# H₁: Mean BMI is different for patients with and without cardiovascular disease.
# Test: Two-sample t-test

# assumption 1: normality check
hist(df_clean$bmi[df_clean$cardio=="No"],  main="BMI (No CVD)")
hist(df_clean$bmi[df_clean$cardio=="Yes"], main="BMI (CVD)")

# assumption 2: check for equal variances
var.test(bmi ~ cardio, data=df_clean)

# Two-sample t-test
# (variance assumption failed - must use the Welch's t-test)
t.test(bmi ~ cardio, data=df_clean)

########################################################################

# H₀: Cholesterol level is independent of cardiovascular disease.
# H₁: Cholesterol level is associated with cardiovascular disease.
# Test: Chi-square

# contingency table
tab <- table(df_clean$cholesterol, df_clean$cardio)
print(tab)

# Chi-square test
chi <- chisq.test(tab)
print(chi)

# assumption check: all expected frequencies >= 5
chi$expected

########################################################################

# H₀: Mean age is the same across glucose level groups.
# H₁: Mean age differs across at least one glucose level group.
# Test: One-way ANOVA

# assumption 1: equal variances
bartlett.test(age_years ~ gluc, data=df_clean)

# assumption 2: check for normality
aov_fit <- aov(age_years ~ gluc, data=df_clean)
hist(residuals(aov_fit), main="Residuals Histogram")
qqnorm(residuals(aov_fit)); qqline(residuals(aov_fit))

# ANOVA test
summary(aov_fit)

