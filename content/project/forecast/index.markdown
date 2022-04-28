---
title: Can Ravens Forecast?
author: Carl Goodwin
date: '2018-07-29'
slug: forecast
categories: 
  - R
tags:
  - time series
  - forecast
summary: Humans have the magical ability to plan for future events. But it's not quite a uniquely human trait as ravens can match a four-year-old.
lastmod: '2022-04-28'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



![](/project/forecast/featured.jpeg)

Humans have the magical ability to plan for future events, for future gain. It’s [not quite a uniquely human trait](https://www.newscientist.com/article/2140668-ravens-can-plan-for-future-as-well-as-4-year-old-children-can/). Because apparently ravens can match a four-year-old.

An abundance of data, and some very nice R packages, make our ability to plan all the more powerful.

In the Spring of 2018 I looked at sales from an historical perspective in [Six Months Later.](/project/six). Here I'll use the data to model a time-series forecast for the year ahead. The techniques apply to any time series with characteristics of trend, seasonality or longer-term cycles.

Why forecast sales? Business plans require a budget, e.g. for resources, marketing and office space. A good projection of revenue provides the foundation for the budget. And, for an established business, with historical data, time-series forecasting is one way to deliver a robust projection.

Without exogenous data, the forecast assumes one continues to do what one’s doing. So, it provides a good starting-point. Then one might, for example, add assumptions about new products or services. And, if there is forward-looking data available, for example, market size projections (with past  projections to train the model), then one could feed this into the forecast modelling too.


```r
library(tidyverse)
library(wesanderson)
library(kableExtra)
library(fpp3)
library(scales)
library(clock)
```


```r
theme_set(theme_bw())

(cols <- wes_palette(name = "IsleofDogs2"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />

First I'll check the encoding of the data.


```r
url <- 
  "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/"

gcloud_csv <- str_c(url, "703943/G-Cloud_spend_data_to_end_March_2018.csv")
  
dos_csv <- str_c(url, "703952/DOS_spend_data_to_end_March_2018.csv")

names <- c(gcloud_csv, dos_csv)

map(names, guess_encoding)
```

```
## [[1]]
## # A tibble: 2 × 2
##   encoding   confidence
##   <chr>           <dbl>
## 1 ISO-8859-1       0.4 
## 2 ISO-8859-2       0.22
## 
## [[2]]
## # A tibble: 2 × 2
##   encoding   confidence
##   <chr>           <dbl>
## 1 ISO-8859-1       0.36
## 2 ISO-8859-2       0.24
```

Next I'll set up a vector of column names to apply consistently to both files, and import the data with the suggested encoding.


```r
colnam <- 
  c("sector",
    "lot",
    "date",
    "spend",
    "status",
    "supplier",
    "customer",
    "framework")

read_dm <- function(x){
  read_csv(
    x,
    col_names = colnam,
    skip = 1,
    locale = locale(encoding = "ISO-8859-1"),
    col_types = NULL)
}

raw <- map(names, read_dm) |> 
  set_names(c("gcloud", "dos")) |> 
  bind_rows() |> 
  mutate(framework = if_else(is.na(framework), "DOS", framework))
```

I'd like to create some new features: Month-end dates, something to distinguish between the two frameworks (*G-Cloud* or *DOS*). The spend has a messy format and needs a bit of cleaning too.

The lot structure for *G-Cloud* has evolved over time, but fortunately, there is a simple mapping, i.e. *PaaS* and *IaaS* became *Cloud Hosting*, *SaaS* became *Cloud Software*, and *Specialist Cloud Services* became *Cloud Support*, so I'll standardise on the latter.


```r
both <- raw |>
  mutate(
    month_end = date_parse(str_c(date, "01", sep = "-"), format = "%b-%y-%d") |> 
      add_months(1) |> add_days(-1),
    date = yearmonth(month_end),
    framework = str_extract(framework, ".{3,7}"),
    spend = str_remove(spend, coll("£")),
    spend = str_replace(spend, "^\\(", "-"),
    spend = parse_number(spend) / 1000000,
    lot = recode(
      lot,
      "Software as a Service (SaaS)" = "Cloud Software",
      "Infrastructure as a Service (IaaS)" = "Cloud Hosting",
      "Platform as a Service (PaaS)" = "Cloud Hosting",
      "Specialist Cloud Services" = "Cloud Support"
      )
)
```

The tidied data now needs to be converted to a [tsibble](https://github.com/tidyverts/tsibble), the temporal equivalent of a [tibble](https://tibble.tidyverse.org).

R has evolved since I first wrote this post. At that time, it was necessary to either split the data into the two frameworks (G-Cloud and DOS) and forecast them separately. Or, as I did with the three G-Cloud lots, use the purrr package to iterate through a forecast.

The tsibble package combined with the newer fable and feasts packages, make this easier. One of the defining feature of the tsibble is the `key`. I want a model for each framework, so I'm setting this as the tsibble `key` (and the temporal variable as the tsibble `index`).


```r
both_ts <- both |>
  group_by(date, framework) |> 
  summarise(spend = sum(spend)) |> 
  as_tsibble(key = framework, index = date)

both_ts |> 
  ggplot(aes(date, spend, colour = framework)) +
  geom_line(key_glyph = "timeseries") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  scale_colour_manual(values = cols[c(3, 4)]) +
  labs(x = NULL, y = NULL, title = "Monthly Digital Marketplace Sales")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="100%" />

By decomposing the historical data we can tease out the underlying trend and seasonality:

* __Trend__: The sales for both frameworks have grown over time as more Suppliers have added their services to the Government frameworks, and more Public Sector organizations have found the benefits of purchasing Cloud services through this faster, simpler, more transparent and more competitive contracting vehicle.

* __Seasonality__: Suppliers often manage their sales and financials based on a quarterly cycle, with a particular emphasis on a strong close to the financial year. And Government Buyers may want to make optimal use of their budgets at the close of their financial year (March 31st). Consequently, we see quarterly seasonality with an extra spike at financial year-end.


```r
both_ts |>
  model(stl = STL(spend ~ trend(window = 7) + season(window = "periodic"))) |>
  components() |>
  autoplot() +
  scale_colour_manual(values = cols[c(3, 4)]) +
  labs(x = NULL, title = "Timeseries Decomposition")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="100%" />

I'll use `auto.arima`: [AutoRegressive Integrated Moving Average](https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average) modelling which aims to describe the autocorrelations in the data.

By setting `stepwise` and `approximation` to `FALSE`, `auto.arima` will explore a wider range of potential models. 

I'll forecast with the default 80% and 95% prediction intervals. This means the darker-shaded 80% range should include the future sales value with an 80% probability. Likewise with a 95% probability when adding the wider and lighter-shaded area.

Use of `autoplot` would simplify the code, but personally I like to expose all the data, for example unpacking the prediction intervals, and have finer control over the visualisation.


```r
mod_ts <- both_ts |>
  model(ARIMA = ARIMA(spend, stepwise = TRUE, approximation = FALSE))

mod_ts |> 
  glance() |>
  select(-ar_roots, -ma_roots) |> 
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> framework </th>
   <th style="text-align:left;"> .model </th>
   <th style="text-align:right;"> sigma2 </th>
   <th style="text-align:right;"> log_lik </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> AICc </th>
   <th style="text-align:right;"> BIC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> DOS </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:right;"> 23.60154 </td>
   <td style="text-align:right;"> -59.93305 </td>
   <td style="text-align:right;"> 123.8661 </td>
   <td style="text-align:right;"> 124.5720 </td>
   <td style="text-align:right;"> 125.8576 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G-Cloud </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:right;"> 34.75275 </td>
   <td style="text-align:right;"> -191.61547 </td>
   <td style="text-align:right;"> 393.2309 </td>
   <td style="text-align:right;"> 394.3421 </td>
   <td style="text-align:right;"> 403.7027 </td>
  </tr>
</tbody>
</table>

```r
mod_ts |> 
  tidy() |>
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> framework </th>
   <th style="text-align:left;"> .model </th>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> DOS </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> ma1 </td>
   <td style="text-align:right;"> -0.7725018 </td>
   <td style="text-align:right;"> 0.1718541 </td>
   <td style="text-align:right;"> -4.495102 </td>
   <td style="text-align:right;"> 0.0002213 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G-Cloud </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> ar1 </td>
   <td style="text-align:right;"> 0.9390150 </td>
   <td style="text-align:right;"> 0.0595354 </td>
   <td style="text-align:right;"> 15.772391 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G-Cloud </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> ma1 </td>
   <td style="text-align:right;"> -0.5777210 </td>
   <td style="text-align:right;"> 0.1094066 </td>
   <td style="text-align:right;"> -5.280494 </td>
   <td style="text-align:right;"> 0.0000019 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G-Cloud </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> sar1 </td>
   <td style="text-align:right;"> -0.5124417 </td>
   <td style="text-align:right;"> 0.1142670 </td>
   <td style="text-align:right;"> -4.484597 </td>
   <td style="text-align:right;"> 0.0000336 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G-Cloud </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> constant </td>
   <td style="text-align:right;"> 1.4084703 </td>
   <td style="text-align:right;"> 0.3168155 </td>
   <td style="text-align:right;"> 4.445712 </td>
   <td style="text-align:right;"> 0.0000385 </td>
  </tr>
</tbody>
</table>

```r
fcast_ts <- mod_ts |>
  forecast(h = "2 years") |> 
  mutate(`95%` = hilo(spend, 95), `80%` = hilo(spend, 80)) |> 
  unpack_hilo(c("95%", "80%")) |>
  rename(fc_spend = spend) |> 
  bind_rows(both_ts)

fcast_ts |>
  ggplot(aes(date, fill = framework)) +
  geom_line(aes(y = spend), colour = cols[5]) +
  geom_ribbon(aes(ymin = `95%_lower`, ymax = `95%_upper`),
    fill = cols[1], colour = NA
  ) +
  geom_ribbon(aes(ymin = `80%_lower`, ymax = `80%_upper`),
    fill = cols[2], colour = NA
  ) +
  geom_line(aes(y = .mean), colour = "white") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  facet_wrap(~framework) +
  labs(
    title = "Digital Marketplace Sales Forecast by Framework",
    x = NULL, y = "Spend",
    subtitle = "80 & 95% Prediction Intervals"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="100%" />

The G-Cloud framework compromises three lots: Cloud Hosting, Cloud Software and Cloud Support. 

I previously combined `auto.arima` (from the forecast package) with functions from the sweep package, to create multiple forecasts in one shot. tsibble coupled fabletools handle this with the `key` set to the `lot` variable.

An alternative option is hierarchical time-series forecasting which models bottom-up, top-down or middle-out, and ensures the sum of the forecasts at the lower level sum to the top-level forecast. This approach has pros and cons and is not considered here.


```r
gcloud_ts <- both |>
  filter(framework == "G-Cloud") |> 
  group_by(date, lot) |> 
  summarise(spend = sum(spend)) |> 
  as_tsibble(key = lot, index = date)

gc_ts <- gcloud_ts |>
  model(ARIMA = ARIMA(spend, stepwise = TRUE, approximation = FALSE))

gc_ts |> 
  glance() |>
  select(-ar_roots, -ma_roots) |> 
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> lot </th>
   <th style="text-align:left;"> .model </th>
   <th style="text-align:right;"> sigma2 </th>
   <th style="text-align:right;"> log_lik </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> AICc </th>
   <th style="text-align:right;"> BIC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Cloud Hosting </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:right;"> 1.347179 </td>
   <td style="text-align:right;"> -109.3173 </td>
   <td style="text-align:right;"> 224.6346 </td>
   <td style="text-align:right;"> 224.9982 </td>
   <td style="text-align:right;"> 231.3801 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cloud Software </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:right;"> 2.275664 </td>
   <td style="text-align:right;"> -107.5551 </td>
   <td style="text-align:right;"> 221.1102 </td>
   <td style="text-align:right;"> 221.5465 </td>
   <td style="text-align:right;"> 227.3428 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cloud Support </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:right;"> 18.606529 </td>
   <td style="text-align:right;"> -212.6556 </td>
   <td style="text-align:right;"> 435.3112 </td>
   <td style="text-align:right;"> 436.2342 </td>
   <td style="text-align:right;"> 446.6246 </td>
  </tr>
</tbody>
</table>

```r
gc_ts |> 
  tidy() |>
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> lot </th>
   <th style="text-align:left;"> .model </th>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Cloud Hosting </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> ma1 </td>
   <td style="text-align:right;"> -0.8269314 </td>
   <td style="text-align:right;"> 0.0765747 </td>
   <td style="text-align:right;"> -10.799011 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cloud Hosting </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> constant </td>
   <td style="text-align:right;"> 0.1713346 </td>
   <td style="text-align:right;"> 0.0258894 </td>
   <td style="text-align:right;"> 6.617935 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cloud Software </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> ma1 </td>
   <td style="text-align:right;"> -0.9981867 </td>
   <td style="text-align:right;"> 0.1304426 </td>
   <td style="text-align:right;"> -7.652304 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cloud Software </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> ma2 </td>
   <td style="text-align:right;"> 0.2242994 </td>
   <td style="text-align:right;"> 0.1224834 </td>
   <td style="text-align:right;"> 1.831264 </td>
   <td style="text-align:right;"> 0.0721116 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cloud Support </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> ma1 </td>
   <td style="text-align:right;"> -0.7044948 </td>
   <td style="text-align:right;"> 0.0743797 </td>
   <td style="text-align:right;"> -9.471597 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cloud Support </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> sma1 </td>
   <td style="text-align:right;"> 0.3878660 </td>
   <td style="text-align:right;"> 0.1458579 </td>
   <td style="text-align:right;"> 2.659205 </td>
   <td style="text-align:right;"> 0.0096735 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cloud Support </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> sma2 </td>
   <td style="text-align:right;"> 0.7866476 </td>
   <td style="text-align:right;"> 0.4284425 </td>
   <td style="text-align:right;"> 1.836064 </td>
   <td style="text-align:right;"> 0.0705352 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cloud Support </td>
   <td style="text-align:left;"> ARIMA </td>
   <td style="text-align:left;"> constant </td>
   <td style="text-align:right;"> 0.7601155 </td>
   <td style="text-align:right;"> 0.2889763 </td>
   <td style="text-align:right;"> 2.630373 </td>
   <td style="text-align:right;"> 0.0104520 </td>
  </tr>
</tbody>
</table>

```r
fcgc_ts <- gc_ts |>
  forecast(h = "2 years") |> 
  mutate(`95%` = hilo(spend, 95), `80%` = hilo(spend, 80)) |> 
  unpack_hilo(c("95%", "80%")) |> 
  rename(fc_spend = spend) |> 
  bind_rows(gcloud_ts)

fcgc_ts |>
  ggplot(aes(date, fill = lot)) +
  geom_line(aes(y = spend), colour = cols[5]) +
  geom_ribbon(aes(ymin = `95%_lower`, ymax = `95%_upper`),
    fill = cols[1], colour = NA
  ) +
  geom_ribbon(aes(ymin = `80%_lower`, ymax = `80%_upper`),
    fill = cols[2], colour = NA
  ) +
  geom_line(aes(y = .mean), colour = "white") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  facet_wrap(~lot) +
  labs(
    title = "G-Cloud Sales Forecast by Lot",
    x = NULL, y = "Spend",
    subtitle = "80 & 95% Prediction Intervals"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="100%" />

So ravens are not yet ready for forecasting with R. But then neither are 4-year-olds, are they?

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
   <td style="text-align:left;"> c[5];  sum[3];  function[2];  conflicts[1];  cumsum[1];  is.na[1];  search[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> add_days[1];  add_months[1];  date_parse[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> mutate[8];  filter[6];  if_else[4];  bind_rows[3];  group_by[3];  summarise[3];  rename[2];  select[2];  arrange[1];  desc[1];  recode[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fable </td>
   <td style="text-align:left;"> ARIMA[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fabletools </td>
   <td style="text-align:left;"> hilo[4];  model[3];  forecast[2];  glance[2];  tidy[2];  unpack_hilo[2];  components[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> feasts </td>
   <td style="text-align:left;"> STL[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[11];  geom_line[5];  geom_ribbon[4];  labs[4];  ggplot[3];  scale_y_continuous[3];  element_text[2];  facet_wrap[2];  scale_colour_manual[2];  theme[2];  autoplot[1];  theme_bw[1];  theme_set[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[3];  set_names[2];  map2_dfr[1];  possibly[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> guess_encoding[1];  locale[1];  parse_number[1];  read_csv[1];  read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scales </td>
   <td style="text-align:left;"> label_dollar[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[8];  str_remove[3];  str_detect[2];  coll[1];  str_count[1];  str_extract[1];  str_remove_all[1];  str_replace[1];  str_starts[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> tibble[2];  as_tibble[1];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> unnest[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tsibble </td>
   <td style="text-align:left;"> as_tsibble[2];  yearmonth[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wesanderson </td>
   <td style="text-align:left;"> wes_palette[1] </td>
  </tr>
</tbody>
</table>
