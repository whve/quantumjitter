---
title: Criminal Goings-on
author: Carl Goodwin
date: '2018-03-01'
slug: forest
categories:
  - R
tags:
  - machine learning
  - statistics
summary: Criminal goings-on in a random forest and predictions with other models from the Tidymodels framework.
lastmod: '2022-04-24'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



When first posted in 2018 this project used the caret package to model [crime in London](https://data.gov.uk/dataset). Since then, the newer [tidymodels](https://www.tidymodels.org) framework, consistent with tidy data principles, has rapidly evolved.

![](/project/forest/featured.jpeg)


```r
library(tidyverse)
library(tidymodels)
library(wesanderson)
library(kableExtra)
library(janitor)
library(scales)
library(vip)
library(poissonreg)
```

This custom palette was created in [Adobe Colour](https://color.adobe.com/create/color-wheel) as the basis for the feature image above and with the hex codes loaded for use in ggplot. `colorRampPalette` enables interpolation of an extended set of colours to support the number of offence types.


```r
theme_set(theme_bw())

cols <- c("#D9B26A", "#D9CAAD", "#402208", "#8C633F", "#0D0D0D") |>
  fct_inorder()

tibble(x = 1:5, y = 1) |>
  ggplot(aes(x, y, fill = cols)) +
  geom_col() +
  geom_label(aes(label = cols), size = 4, vjust = 2, fill = "white") +
  annotate(
    "label",
    x = 3, y = 0.5,
    label = "Custom Pallette",
    fill = "white",
    alpha = 0.8,
    size = 6
  ) +
  scale_fill_manual(values = as.character(cols)) +
  theme_void() +
  theme(legend.position = "none")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />

```r
cols10 <- colorRampPalette(cols)(10)
```


```r
url <- str_c(
  "https://data.london.gov.uk/",
  "download/recorded_crime_rates/",
  "c051c7ec-c3ad-4534-bbfe-6bdfee2ef6bb/",
  "crime%20rates.csv"
)

raw_df <-
  read_csv(url, col_types = "cfcfdn") |>
  clean_names() |>
  mutate(
    year = str_extract(year, "(?:1999|200[0-9]|201[0-7])"), # 1999-2007
    year = as.numeric(year)
  ) |>
  group_by(year, borough, offences) |>
  summarise(number_of_offences = sum(number_of_offences)) |>
  ungroup()
```

A faceted plot is one way to get a sense of the data.


```r
raw_df |>
  mutate(borough = str_wrap(borough, 11)) |>
  ggplot(aes(year, number_of_offences, colour = offences, group = offences)) +
  geom_line() +
  facet_wrap(~borough, scales = "free_y", ncol = 4) +
  labs(
    x = NULL, y = NULL, title = "London Crime by Borough",
    colour = "Offence", caption = "Source: data.gov.uk"
  ) +
  scale_colour_manual(values = cols10) +
  guides(colour = guide_legend(nrow = 4)) +
  theme(
    strip.background = element_rect(fill = cols10[1]),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="100%" />

Visualising data in small multiples using `facet_wrap` or `facet_grid` can be a useful way to explore data. When there are a larger number of these however, as we're starting to see in the example above, there are alternative techniques one can employ. This is explored in [Seeing the Wood for the Trees](/project/wood).

Nonetheless, one can anyway see there are data aggregated at multiple levels. So to net these data down to purely borough-level, I'll filter out the summarised rows, for example, "England and Wales" and "Inner London".


```r
crime_df <- raw_df |>
  filter(
    offences != "All recorded offences",
    !borough %in% c(
      "England and Wales",
      "Met Police Area", 
      "Inner London", 
      "Outer London"
    )
  )
```

There are 9 types of offence in 33 boroughs. The dataset covers the period 1999 to 2016. 

The faceted plot hints at a potential interaction between borough and type of offence. In more affluent boroughs, and/or those attracting greater visitor numbers, e.g. Westminster and Kensington & Chelsea, "theft and handling" is the more dominant category. In Lewisham, for example, "violence against the person" exhibits higher counts. However, for the purpose of this basic model comparison, I'm going to set aside the potential interaction term.

Before modelling, I'll visualise the dependent variable against each independent variable.


```r
crime_df |>
  group_by(offences, borough) |>
  summarise(number_of_offences = sum(number_of_offences)) |>
  group_by(offences) |>
  mutate(
    median_offences = median(number_of_offences),
    offences = str_wrap(offences, 10),
  ) |>
  ggplot(aes(fct_reorder(offences, median_offences), number_of_offences)) +
  geom_boxplot(fill = cols[1]) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL, y = NULL,
    title = "Number of Offences by Type",
    caption = "Source: data.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="100%" />


```r
crime_df |>
  group_by(offences, borough) |>
  summarise(number_of_offences = sum(number_of_offences)) |>
  group_by(borough) |>
  mutate(
    median_offences = median(number_of_offences),
    offences = str_wrap(offences, 10),
  ) |>
  ggplot(aes(fct_reorder(borough, median_offences), number_of_offences)) +
  geom_boxplot(fill = cols[1]) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
    title = "Number of Offences by Borough",
    caption = "Source: data.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="100%" />

The offences and borough variables show significant variation in crime counts. And there is also evidence of a change over time.


```r
crime_df |>
  group_by(year) |>
  summarise(number_of_offences = sum(number_of_offences)) |>
  ggplot(aes(year, number_of_offences)) +
  geom_line(colour = cols[4], linetype = "dashed") +
  geom_smooth(colour = cols[5], fill = cols[1]) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL, y = NULL,
    title = "Number of Offences by Year",
    caption = "Source: data.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="100%" />

I'll separate out some test data so I can compare the performance of the models on data they have not see during model training.


```r
set.seed(123)

data_split <- 
  crime_df |>
  initial_split(strata = offences)

crime_train <- data_split |>
  training()

crime_test <- data_split |>
  testing()
```

I'm using the recipes package to establish the role of the variables. Alternatively I could have used a formula-based approach, i.e. `number_of_offences ~ borough + offences + year`.

Whilst `borough` and `offences` are nominal, I'm not creating any dummy variables since I intend to use tree-based models which will anyway branch left and right based on groups of values.


```r
crime_recipe <-
  crime_train |>
  recipe() |>
  update_role(number_of_offences, new_role = "outcome") |>
  update_role(-has_role("outcome"), new_role = "predictor")

summary(crime_recipe)
```

```
## # A tibble: 4 × 4
##   variable           type    role      source  
##   <chr>              <chr>   <chr>     <chr>   
## 1 year               numeric predictor original
## 2 borough            nominal predictor original
## 3 offences           nominal predictor original
## 4 number_of_offences numeric outcome   original
```

I'll start with a Recursive Partitioning And Regression Trees (rpart) model. The feature importance plot tells me which variables are having the biggest influence on the model. The type of offence is the most important predictor in the rpart model, followed by the location of the offences. This makes intuitive sense. 

Clearly there is a temporal component too otherwise there would be no trend.


```r
rp_model <- 
  decision_tree() |>
  set_engine("rpart") |>
  set_mode("regression")

rp_wflow <- workflow() |>
  add_recipe(crime_recipe) |>
  add_model(rp_model)

rp_fit <- rp_wflow |> 
  fit(crime_train)

rp_fit |>
  pull_workflow_fit() |> 
  vip(aesthetics = list(fill = cols[1])) +
  labs(title = "Feature Importance -- rpart")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="100%" />

```r
rp_results <- rp_fit |> 
  augment(crime_test) |> 
  mutate(model = "rpart")
```

Ranger is an implementation of random forests or recursive partitioning that, according to the documentation, is particularly suited to high dimensional data. My data is not high-dimensional, but let's throw it into the mix.


```r
ranger_model <- 
  rand_forest() |>
  set_engine("ranger", mtry = 2, importance = "impurity") |>
  set_mode("regression")

ranger_wflow <- workflow() |>
  add_recipe(crime_recipe) |>
  add_model(ranger_model)

ranger_fit <- ranger_wflow |> 
  fit(crime_train)

ranger_fit |>
  pull_workflow_fit() |> 
  vip(aesthetics = list(fill = cols[3])) +
  labs(title = "Feature Importance -- Ranger")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="100%" />

```r
ranger_results <- ranger_fit |> 
  augment(crime_test) |> 
  mutate(model = "ranger")
```

And of course my project title would make little sense without a Random Forest.


```r
rf_model <- 
  rand_forest() |>
  set_engine("randomForest", mtry = 2) |>
  set_mode("regression")

rf_wflow <- workflow() |>
  add_recipe(crime_recipe) |>
  add_model(rf_model)

rf_fit <- rf_wflow |> 
  fit(crime_train)

rf_fit |>
  pull_workflow_fit() |> 
  vip(aesthetics = list(fill = cols[5])) +
  labs(title = "Feature Importance -- Random Forest")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="100%" />

```r
rf_results <- rf_fit |> 
  augment(crime_test) |> 
  mutate(model = "random forest")
```
For good measure, I'll also include a generalized linear model (glm)


```r
poisson_model <- 
  poisson_reg() |>
  set_engine("glm") |>
  set_mode("regression")

poisson_wflow <- workflow() |>
  add_recipe(crime_recipe) |>
  add_model(poisson_model)

poisson_fit <- poisson_wflow |> 
  fit(crime_train)

poisson_fit |>
  pull_workflow_fit() |> 
  vip(aesthetics = list(fill = cols[4])) +
  labs(title = "Feature Importance -- glm")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="100%" />

```r
poisson_results <- poisson_fit |> 
  augment(crime_test) |> 
  mutate(model = "glm")
```

The Random Forest and the glm models performed the best here, with the former edging the Mean Absolute Error and R Squared metrics, and the latter with its nose in front on the Root Mean Squared Error. 


```r
model_results <- 
  rp_results |> 
  bind_rows(ranger_results) |> 
  bind_rows(rf_results) |> 
  bind_rows(poisson_results) |> 
  group_by(model) |> 
  metrics(truth = number_of_offences, estimate = .pred)

model_results |> 
  ggplot(aes(model, .estimate, fill = model)) +
  geom_col() +
  geom_label(aes(label = round(.estimate, 2)), size = 3, fill = "white") +
  facet_wrap(~ .metric, scales = "free_y") +
  scale_fill_manual(values = as.character(cols[c(4, 5, 3, 1)])) +
  labs(x = NULL, y = NULL, title = "Comparison of Model Metrics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="100%" />

Another way of approaching all this would be to use time-series forecasting. This would major on auto-regression, i.e. looking at how the lagged number-of-offences influence future values. And one could further include exogenous data such as, say, the numbers of police. It would be reasonable to expect that increasing police numbers would, in time, lead to decreased crime levels.

I explored time-series in other posts such as [Digging Deep](/project/planning), so I won't go down that path here. 

What I could do though is to strengthen my tree-based models above by engineering some additional temporal features. Let's try that just with the Random Forest to see if it improves the outcome.


```r
temp_df <- 
  crime_df |> 
  mutate(num_lag1 = lag(number_of_offences),
         num_lag2 = lag(number_of_offences, 2),
         num_lag3 = lag(number_of_offences, 3)) |> 
  drop_na()
```

So, when predicting the number of offences, the model will now additionally consider, for each borough, type of offence and year, the number of offences in each of the three prior years.


```r
set.seed(123)

data_split <- 
  temp_df |>
  initial_split(strata = offences)

temp_train <- data_split |>
  training()

temp_test <- data_split |>
  testing()

temp_recipe <-
  temp_train |>
  recipe() |>
  update_role(number_of_offences, new_role = "outcome") |>
  update_role(-has_role("outcome"), new_role = "predictor")

summary(temp_recipe)
```

```
## # A tibble: 7 × 4
##   variable           type    role      source  
##   <chr>              <chr>   <chr>     <chr>   
## 1 year               numeric predictor original
## 2 borough            nominal predictor original
## 3 offences           nominal predictor original
## 4 number_of_offences numeric outcome   original
## 5 num_lag1           numeric predictor original
## 6 num_lag2           numeric predictor original
## 7 num_lag3           numeric predictor original
```

```r
temp_model <- 
  rand_forest() |>
  set_engine("randomForest", mtry = 2) |>
  set_mode("regression")

temp_wflow <- workflow() |>
  add_recipe(temp_recipe) |>
  add_model(temp_model)

temp_fit <- temp_wflow |> 
  fit(temp_train)

temp_fit |>
  pull_workflow_fit() |> 
  vip(aesthetics = list(fill = cols[4])) +
  labs(title = "Feature Importance -- Random Forest with Lags")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="100%" />

```r
temp_results <- temp_fit |> 
  augment(temp_test) |> 
  metrics(truth = number_of_offences, estimate = .pred) |> 
  mutate(model = "rf with lags")
```

The recipe summary includes the three new predictors. And the feature importance plot shows the lags playing a larger role in the model than the `year` variable, so looks like we should anticipate a model improvement.


```r
updated_results <- 
  model_results |> 
  bind_rows(temp_results)

updated_results |> 
  ggplot(aes(model, .estimate, fill = model)) +
  geom_col() +
  geom_label(aes(label = round(.estimate, 2)), size = 3, fill = "white") +
  facet_wrap(~ .metric, scales = "free_y") +
  scale_fill_manual(values = as.character(cols[c(4, 5, 3, 2, 1)])) +
  labs(x = NULL, y = NULL, title = "Comparison of Model Metrics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-18-1.png" width="100%" />

The model metrics bear this out. The `mae` and `rmse` are markedly smaller, and the `rsq` significantly improved. We could have tried further lags. We could have tried tweaking some parameters. We could have tried time-series forecasting with, for example a statistical model like ARIMA, or a Neural Network model such as NNETAR.

The best approach would depend upon a more precise definition of the objective. And some trial and error, comparing approaches after more extensive feature-engineering, validation, testing and tuning. For the purposes of this post though I wanted to merely explore some techniques. So I'll leave it there.

## R Toolbox

Summarising below the packages and functions used in this post enables me to separately create a [toolbox visualisation](/project/box) summarising the usage of packages and functions across all posts.

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Package </th>
   <th style="text-align:left;"> Function </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> base </td>
   <td style="text-align:left;"> as.character[3];  as.numeric[1];  c[2];  conflicts[1];  cumsum[1];  function[1];  list[5];  round[2];  search[1];  set.seed[2];  sum[5];  summary[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> broom </td>
   <td style="text-align:left;"> augment[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[6];  lag[3];  arrange[2];  bind_rows[4];  desc[2];  group_by[8];  if_else[3];  mutate[14];  summarise[5];  ungroup[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forcats </td>
   <td style="text-align:left;"> fct_inorder[1];  fct_reorder[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[10];  annotate[1];  coord_flip[1];  element_rect[1];  element_text[3];  facet_wrap[3];  geom_boxplot[2];  geom_col[3];  geom_label[3];  geom_line[2];  geom_smooth[1];  ggplot[7];  guide_legend[1];  guides[1];  labs[11];  scale_colour_manual[1];  scale_fill_manual[3];  scale_y_continuous[1];  scale_y_log10[2];  theme[4];  theme_bw[1];  theme_set[1];  theme_void[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grDevices </td>
   <td style="text-align:left;"> colorRampPalette[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> janitor </td>
   <td style="text-align:left;"> clean_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> parsnip </td>
   <td style="text-align:left;"> decision_tree[1];  poisson_reg[1];  rand_forest[3];  set_engine[5];  set_mode[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[1];  map2_dfr[1];  possibly[1];  set_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> read_csv[1];  read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> recipes </td>
   <td style="text-align:left;"> has_role[2];  recipe[2];  update_role[4] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rsample </td>
   <td style="text-align:left;"> initial_split[2];  testing[2];  training[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scales </td>
   <td style="text-align:left;"> cut_short_scale[3];  label_number[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stats </td>
   <td style="text-align:left;"> median[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[6];  str_count[1];  str_detect[2];  str_extract[1];  str_remove[2];  str_remove_all[1];  str_starts[1];  str_wrap[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[1];  tibble[3];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> drop_na[1];  unnest[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vip </td>
   <td style="text-align:left;"> vip[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> workflows </td>
   <td style="text-align:left;"> add_model[5];  add_recipe[5];  pull_workflow_fit[5];  workflow[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> yardstick </td>
   <td style="text-align:left;"> metrics[2] </td>
  </tr>
</tbody>
</table>
