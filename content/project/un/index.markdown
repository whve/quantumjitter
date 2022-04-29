---
title: East-West Drift
author: Carl Goodwin
date: '2019-01-09'
slug: un
categories:
  - R
tags:
  - dimensionality reduction
  - animation
summary: Animated dimensionality reduction and whether East and West are drifting closer or farther apart based on historical UN voting patterns.
lastmod: '2022-04-29'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



![](/project/un/featured.GIF)

In [Finding Happiness in 'The Smoke'](/project/happiness), dimensionality reduction and cluster analysis are used to see how different characteristics group London boroughs.

Dimensionality reduction is used here to visualise the grouping of UN members, for example [five of the founding members](https://research.un.org/en/unmembers/founders), based on their [General Assembly](http://www.un.org/en/ga/) voting patterns. And by using animation, it's possible to more easily see changes over time.


```r
library(tidyverse)
library(tidymodels)
library(tsibble)
library(gganimate)
library(clock)
library(unvotes)
library(kableExtra)
library(patchwork)
library(wesanderson)
library(rvest)
library(glue)
```


```r
theme_set(theme_bw())

(cols <- wes_palette("GrandBudapest2"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />


```r
raw_df <- un_votes |>
  inner_join(un_roll_calls, by = "rcid") |>
  filter(country_code %in% c("GB", "CN", "US", "FR", "RU")) |>
  mutate(
    country = recode(
      country_code,
      GB = "UK",
      CN = "China",
      FR = "France",
      RU = "Russia"
    ),
    date = date_parse(as.character(date), format = "%Y-%m-%d")
  )

from <- raw_df |>
  summarise(min(get_year(date))) |>
  pull()

to <- raw_df |>
  summarise(max(get_year(date))) |>
  pull()
```

Applying a sliding window to the roll-calls from 1946 to 2019 will make it possible to show the temporal changes.


```r
tidy_df <- raw_df |>
  arrange(date, rcid) |>
  nest(-c(date, rcid)) |>
  mutate(vote_id = row_number(), year = get_year(date)) |>
  unnest(data) |>
  complete(country, nesting(vote_id)) |>
  mutate(vote = replace_na(as.character(vote), "na"), value = 1) |>
  group_by(vote_id) |>
  fill(year, .direction = "updown") |>
  mutate(variation = n_distinct(vote)) |>
  ungroup() |>
  filter(variation != 1) |>
  select(country, vote_id, year, vote, value)

wdow_df <- tidy_df |>
  as_tsibble(key = country, index = vote_id) |>
  nest(-vote_id) |>
  slide_tsibble(.size = 1000, .step = 250, .id = "slide_id") |>
  unnest(data) |>
  as_tibble() |>
  arrange(slide_id, vote_id, country)
```

Dimensionality reduction may be performed on each window. And the voting patterns are then visualised as a two-dimensional animation.


```r
wdows <- wdow_df |>
  summarise(max(slide_id)) |>
  pull()

slide_pca <- function(x) {
  wide_df <- wdow_df |>
    filter(slide_id == x) |>
    pivot_wider(
      id_cols = c(country, slide_id),
      names_from = c(vote_id, vote),
      values_from = value,
      values_fill = 0
    )

  pca_fit <- wide_df |>
    select(-c(country, slide_id)) |>
    prcomp(scale = TRUE) |>
    augment(wide_df) |>
    select(slide_id, country, .fittedPC1, .fittedPC2)
}

pca_windows <- map_dfr(1:wdows, slide_pca)

p <- pca_windows |>
  mutate(east_west = if_else(country %in% c("China", "Russia"), 
                             "East", "West")) |>
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_label(aes(label = country, fill = east_west)) +
  scale_fill_manual(values = cols[c(1, 3)]) +
  transition_time(slide_id) +
  labs(
    title = glue("P5 Distance for the Period {from} to {to}"),
    subtitle = "Frame {frame} of {nframes}",
    x = "Principal Component 1",
    y = "Principal Component 2",
    fill = NULL,
    caption = "Source: unvotes"
  ) +
  shadow_wake(wake_length = 0.1, wrap = FALSE)
  
animate(p, fps = 5, end_pause = 10)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.gif" width="100%" />

France and the UK, for example, have remained particularly close given their historical ties and geographical proximity.

The UN's [Security Council Veto List](http://research.un.org/en/docs/sc/quick/veto) provides further insights on the changing profile of P5 voting over the decades.


```r
url <- "https://www.un.org/depts/dhl/resguide/scact_veto_table_en.htm"

meeting_df <- url |>
  read_html() |>
  html_element(".tablefont") |>
  html_table(fill = TRUE) |>
  select(date = 1, draft = 2, meeting = 3, agenda = 4, vetoed_by = 5) |>
  slice(-c(1:2))
```


```r
meeting_df2 <- meeting_df |>
  mutate(
    date = str_remove(date, "-(?:\\d{2}|\\d)"),
    date = date_parse(date, format = "%d %B %Y"),
    date = if_else(get_year(date) == "86", date_build(1986, 01, 01), date),
    vetoed_by = str_replace(vetoed_by, "USSR", "Russia"),
    Russia = if_else(str_detect(vetoed_by, "Russia"), 1, 0),
    China = if_else(str_detect(vetoed_by, "China"), 1, 0),
    France = if_else(str_detect(vetoed_by, "France"), 1, 0),
    US = if_else(str_detect(vetoed_by, "US"), 1, 0),
    UK = if_else(str_detect(vetoed_by, "UK"), 1, 0)
  ) |>
  pivot_longer(c(Russia:UK), names_to = "country", values_to = "veto") |>
  filter(veto == 1)

country_df <- meeting_df2 |>
  count(country) |>
  mutate(country = fct_reorder(country, n))
```


```r
cols2 <- wes_palette(5, name = "GrandBudapest2", type = "continuous")

little_plot <- country_df |>
  ggplot(aes(country, n, fill = country)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = cols2[c(1:5)]) +
  geom_label(aes(label = n), colour = "white", hjust = "inward") +
  labs(
    x = NULL, y = NULL, fill = NULL, title = "Most Vetoes",
    caption = "Source: research.un.org"
  )

year_df <- meeting_df2 |>
  mutate(year = get_year(date)) |>
  count(year, country)

to_date <- format(max(meeting_df2$date), "%b %d, %y")

big_plot <- year_df |>
  ggplot(aes(year, n, fill = country)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = cols2[c(1:5)]) +
  scale_x_continuous(breaks = (seq(1945, 2020, 5))) +
  labs(
    x = NULL, y = "Veto Count", fill = NULL,
    title = glue("Security Council Vetoes to {to_date}")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

layout <- "AAB"
big_plot + little_plot + plot_layout(design = layout)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/vetoes-1.png" width="100%" />

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
   <td style="text-align:left;"> as.character[2];  c[8];  conflicts[1];  cumsum[1];  format[1];  function[2];  max[3];  min[1];  search[1];  seq[1];  sum[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> date_build[1];  date_parse[2];  get_year[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[9];  arrange[4];  count[2];  desc[2];  group_by[2];  if_else[10];  inner_join[1];  mutate[12];  n[2];  n_distinct[1];  pull[3];  recode[1];  row_number[1];  select[4];  slice[1];  summarise[4];  ungroup[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forcats </td>
   <td style="text-align:left;"> fct_reorder[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gganimate </td>
   <td style="text-align:left;"> animate[1];  shadow_wake[1];  transition_time[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[5];  coord_flip[1];  element_text[1];  geom_col[2];  geom_label[2];  ggplot[3];  labs[3];  scale_fill_manual[3];  scale_x_continuous[1];  theme[1];  theme_bw[1];  theme_set[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> glue </td>
   <td style="text-align:left;"> glue[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> patchwork </td>
   <td style="text-align:left;"> plot_layout[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[1];  map_dfr[1];  map2_dfr[1];  possibly[1];  set_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rvest </td>
   <td style="text-align:left;"> html_element[1];  html_table[1];  read_html[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stats </td>
   <td style="text-align:left;"> prcomp[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[5];  str_count[1];  str_detect[7];  str_remove[3];  str_remove_all[1];  str_replace[1];  str_starts[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[2];  tibble[2];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> complete[1];  fill[1];  nest[2];  nesting[1];  pivot_longer[1];  pivot_wider[1];  replace_na[1];  unnest[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tsibble </td>
   <td style="text-align:left;"> as_tsibble[1];  slide_tsibble[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> utils </td>
   <td style="text-align:left;"> data[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wesanderson </td>
   <td style="text-align:left;"> wes_palette[2] </td>
  </tr>
</tbody>
</table>
