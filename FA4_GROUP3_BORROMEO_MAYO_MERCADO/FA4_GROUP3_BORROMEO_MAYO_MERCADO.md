---
title: "Formative Assessment 4"
author: "Borromeo_Mayo_Mercado"
date: "2025-09-30"
output:
  pdf_document: default
  html_document: default
---

[*https://github.com/chieelo/STATS/tree/d5fb88cf2743867df820d5468d01029b673d2665/FA4_GROUP3_BORROMEO_MAYO_MERCADO*]

## The normal data are female height measurements, the skewed-right data are age at marriage for females, the skewed-left data are obituary data that give the age at death for females, and the uniform data are the amount of cola put into a 12 ounce container by a soft drinks machine.

```r
library(knitr)

data <- read.csv("Data-1.csv")
kable(data)
```

### Functions to use:

```r
origin_moments <- function(x, k) mean(x^k)

mean_moments <- function(x, k) {
  mu <- mean(x)
  mean((x - mu)^k)
}

moment_about_mean <- function(x, k, c) mean((x - c)^k)
```

## 1. Find the (a) first, (b) second, (c) third, and (d ) fourth moments for each of the sets of data (normal, skewed-right, skewed-left, uniform).

```r

origin_results <- sapply(1:4, function(k) c(
  Normal        = origin_moments(data$Normal, k),
  Skewed_Right  = origin_moments(data$SK_right, k),
  Skewed_Left   = origin_moments(data$SK_left, k),
  Uniform       = origin_moments(data$Uniform, k)
))

origin_df <- as.data.frame(t(origin_results))
rownames(origin_df) <- c("1st", "2nd", "3rd", "4th")

kable(origin_df, digits = 3,
      caption = "Moments for each of the sets of data")

```

## 2. Find the (a) first, (b) second, (c) third, and (d ) fourth moments about the mean for each of the sets of data (normal, skewed-right, skewed-left, uniform).

```r

mean_result <- sapply(1:4, function(k) c(
  Normal        = mean_moments(data$Normal, k),
  Skewed_Right  = mean_moments(data$SK_right, k),
  Skewed_Left   = mean_moments(data$SK_left, k),
  Uniform       = mean_moments(data$Uniform, k)
))

mean_df <- as.data.frame(t(mean_result))
rownames(mean_df) <- c("1st", "2nd", "3rd", "4th")

kable(mean_df, digits = 5,
      caption = "Moments about the mean for each of the sets of data ")

```

## 3. Find the (a) first, (b) second, (c) third, and (d ) fourth moments about the number 75 for the set of female height measurements.

```r

moments75 <- sapply(1:4, function(k) moment_about_mean(data$Normal, k, 75))

moments75_df <- data.frame(
  Moment = c("1st", "2nd", "3rd", "4th"),
  Value  = round(moments75, 2)
)

kable(moments75_df, caption = "Moments about the number 75 for the set of female height measurements")


```

## 4. Using the results of items 2 and 3 for the set of female height measurements, verify the relations between the moments

Using the results of items 2 and 3 for the set of female height measurements, verify the relations between the moments

(a) $$
    m_2 = m'_2 - (m'_1)^2
    $$

(b) $$
    m_3 = m'_3 - 3m'_2 m'_1 + 2(m'_1)^3
    $$

(c) $$
    m_4 = m'_4 - 4m'_3 m'_1 + 6m'_2 (m'_1)^2 - 3(m'_1)^4
    $$

```r
# about origin
m1p <- origin_moments(data$Normal, 1)
m2p <- origin_moments(data$Normal, 2)
m3p <- origin_moments(data$Normal, 3)
m4p <- origin_moments(data$Normal, 4)

# about mean
m2 <- mean_moments(data$Normal, 2)
m3 <- mean_moments(data$Normal, 3)
m4 <- mean_moments(data$Normal, 4)


m2_relation <- m2p - m1p^2
m3_relation <- m3p - 3*m2p*m1p + 2*(m1p^3)
m4_relation <- m4p - 4*m3p*m1p + 6*m2p*(m1p^2) - 3*(m1p^4)


moment_tab <- data.frame(
  Moment   = c("m2", "m3", "m4"),
  Actual   = c(m2, m3, m4),
  Relation = c(m2_relation, m3_relation, m4_relation)
)

kable(moment_tab, digits = 4,
      caption = "Relations between the moments")

```
