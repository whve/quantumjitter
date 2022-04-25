---
title: Six Months Later
author: Carl Goodwin
date: '2018-04-02'
slug: six
categories:
  - R
tags:
summary: Exploring colour palettes and small multiples with ggplot2.
lastmod: '2022-04-25'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



In September 2017 I wrote a post entitled [Let's Jitter](/project/jitter). It looked at how the SME share of sales evolved over time. I revisited this topic here six months later, along the way adding a splash of colour and some faceted plots.

![](/project/six/featured.JPG)


```r
library(tidyverse)
library(clock)
library(scales)
library(kableExtra)
library(wesanderson)
```

I'm using one of the beautiful range of [Wes Anderson Palettes](https://github.com/karthik/wesanderson). I often use the palettes provided by [ColorBrewer](https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3) too.


```r
theme_set(theme_bw())

(cols <- wes_palette(9, name = "Chevalier1", type = "continuous"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />

In [Let's Jitter](/project/jitter) I assumed the G-Cloud data file adopted the [UK Government standard](https://www.gov.uk/guidance/gds-api-technical-and-data-standards#use-unicode-for-encoding) of UTF-8. I used the stringr package to fix any issues.

This time around, I'm importing the files for two frameworks (G-Cloud and DOS) after first checking the encoding to see if I can get a cleaner import. `guess_encoding` suggests these files use the ISO-8859-1 standard.


```r
url <- 
  "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/"

gcloud_csv <- str_c(url, "678283/G-Cloud-spend-Dec2017.csv")

dos_csv <- str_c(url, "678286/DOS-spend-Dec2017.csv")

names <- c(gcloud_csv, dos_csv)

map(names, guess_encoding)
```

```
## [[1]]
## # A tibble: 2 × 2
##   encoding   confidence
##   <chr>           <dbl>
## 1 ISO-8859-1       0.39
## 2 ISO-8859-2       0.24
## 
## [[2]]
## # A tibble: 2 × 2
##   encoding   confidence
##   <chr>           <dbl>
## 1 ISO-8859-1       0.44
## 2 ISO-8859-2       0.23
```

Next I'll set up a vector of column names to apply consistently to both files, import the data with the suggested encoding, and bind them into one tibble.


```r
colnam <-
  c("sector",
    "lot",
    "month",
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

combined_df <- map(names, read_dm) |> 
  set_names(c("gcloud", "dos")) |> 
  bind_rows() |> 
  mutate(framework = if_else(is.na(framework), "DOS", framework))
```

I'd like to create some new features: Month-end dates, something to distinguish between the two frameworks (*G-Cloud* or *DOS*) and the framework version (i.e. G-Cloud 1 to 9). The spend has a messy format and needs a bit of cleaning too.


```r
clean_df <- combined_df |>
  mutate(
    month_end = date_parse(str_c(month, "01", sep = "-"), format = "%b-%y-%d") |> 
      add_months(1) |> add_days(-1),
    version = str_remove(framework, fixed("-Cloud ")),
    version = str_replace(version, fixed("G-Cloud"), "G1"),
    version = str_replace(version, fixed("GIII"), "G3"),
    version = str_replace(version, fixed("GServices II"), "G2"),
    framework = str_extract(framework, ".{3,7}"),
    spend = str_remove(spend, fixed("£")),
    spend = str_replace(spend, "^\\(", "-"),
    spend = parse_number(spend),
    SME_spend = if_else(status == "SME", spend, 0)
  )
```

Finding the interval between G-Cloud versions will enable me to calculate and print the average in the next paragraph using inline r code.


```r
version_df <- clean_df |>
  filter(version != "DOS") |> 
  group_by(version) |>
  summarise(start = min(month_end)) |> 
  mutate(next_start = lead(start),
         interval = lubridate::interval(start, next_start) %/% months(1))
```

Every 7.8 months, on average, suppliers are asked to resubmit their G-Cloud offerings with their latest pricing and service descriptions. It's a chance for new suppliers, often smaller ones, to join the existing list of suppliers and increase overall competitiveness for Cloud services.

Let's visualise how each of these framework versions grows, matures and fades away as the next one takes over.


```r
vers_summary <- clean_df |>
  filter(version != "DOS") |> 
  group_by(version, month_end) |>
  summarise(sales = sum(spend) / 1000000)

vers_summary |> 
  ggplot(aes(month_end, sales, colour = version)) +
  geom_line() +
  geom_smooth(size = 2) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  scale_colour_manual(values = cols) +
  labs(x = NULL, y = NULL, title = "The Lifecycle of G-Cloud Versions", 
       subtitle = "Monthly Sales by Version") + 
  labs(caption = "\nSource: GOV.UK's Digital Marketplace")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="100%" />

[Let's Jitter](/project/jitter) showed signs of a weakening in the SME share of G-Cloud sales by value. This plot shows this trend to have persisted, and also reflects the Digital Outcomes & Specialists (DOS) framework exhibiting a downward trend.


```r
fw_summary <- clean_df |>
  group_by(framework, month_end) |>
  summarise(pct = sum(SME_spend, na.rm = TRUE) / sum(spend, na.rm = TRUE))

fw_summary |> 
  ggplot(aes(month_end, pct, colour = framework)) +
  geom_line() +
  geom_smooth(size = 2) +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1), labels = label_percent()) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_colour_manual(values = cols[c(1, 9)]) +
  labs(x = NULL, y = NULL, 
       title = "The Waning SME Share of Sales", 
       subtitle = "% Monthly Sales Value via SME (vs Large Enterprise) Suppliers") + 
  labs(caption = "\nSource: GOV.UK's Digital Marketplace")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="100%" />

Overall spending via the combined frameworks however continues to grow across all parts of Public Sector. I'll use a [small multiples](https://en.wikipedia.org/wiki/Small_multiple) visualisation technique to show this using ggplot2's `facet_wrap`.


```r
sect_summary <- 
  clean_df |>
  filter(!sector %in% c("Unregistered or Unknown",
                        "Utility (Historic)",
                        "Wider Public Sector")) |>
  group_by(sector, month_end) |>
  summarise(
  sales = sum(spend) / 1000000,
  pct = sum(SME_spend, na.rm = TRUE) / sum(spend, na.rm = TRUE)
  )

sect_summary |> 
  ggplot(aes(month_end, sales, colour = sector)) +
  geom_line() +
  geom_smooth(size = 2) +
  facet_wrap(~ sector, scales = "free_y") +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  scale_colour_manual(values = cols) +
  labs(x = NULL, y = NULL, 
       title = "All Sectors Increase Digital Marketplace Spend", 
       subtitle = "G-Cloud & DOS Spend by Sector") + 
  labs(caption = "\nSource: GOV.UK's Digital Marketplace")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="100%" />

The decline in the proportion of spend via SMEs is also fairly broad-based.


```r
sect_summary |> 
  ggplot(aes(month_end, pct, colour = sector)) +
  geom_line() +
  geom_smooth(size = 2) +
  facet_wrap(~ sector, scales = "free_y") +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = label_percent()) +
  scale_colour_manual(values = cols) +
  labs(x = NULL, y = NULL, 
       title = "Most Sectors Spend Proportionately Less on SMEs", 
       subtitle = "Pct SME G-Cloud & DOS Spend by Sector") + 
  labs(caption = "\nSource: GOV.UK's Digital Marketplace")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="100%" />

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
   <td style="text-align:left;"> c[5];  conflicts[1];  cumsum[1];  function[2];  is.na[1];  min[1];  months[1];  search[1];  sum[7];  version[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> add_days[1];  add_months[1];  date_parse[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[8];  arrange[2];  bind_rows[1];  desc[2];  group_by[5];  if_else[5];  lead[1];  mutate[7];  summarise[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[4];  facet_wrap[2];  geom_line[4];  geom_smooth[4];  ggplot[4];  labs[8];  scale_colour_manual[4];  scale_x_date[4];  scale_y_continuous[4];  theme[2];  theme_bw[1];  theme_set[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[3];  map2_dfr[1];  possibly[1];  set_names[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> guess_encoding[1];  locale[1];  parse_number[1];  read_csv[1];  read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scales </td>
   <td style="text-align:left;"> label_dollar[2];  label_percent[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stats </td>
   <td style="text-align:left;"> start[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> fixed[5];  str_c[8];  str_count[1];  str_detect[2];  str_extract[1];  str_remove[4];  str_remove_all[1];  str_replace[4];  str_starts[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[1];  tibble[2];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> unnest[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wesanderson </td>
   <td style="text-align:left;"> wes_palette[1] </td>
  </tr>
</tbody>
</table>
