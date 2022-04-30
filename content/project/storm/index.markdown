---
title: Weathering the Storm
author: Carl Goodwin
date: '2020-08-02'
slug: storm
categories:
  - R
tags:
  - quant
summary: Covid-19 began battering the financial markets in February. Which sectors are faring best when comparing each in the S&P 500 with the overall market since February 19th, 2020.
lastmod: '2022-04-30'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



![](/project/storm/featured.GIF)

Covid-19 began battering the financial markets in 2020 Which sectors are faring best?

I'll compare each sector in the S&P 500 with the overall market. Baselining each at 100% as of February 19th, we'll see which were the first to recover lost ground.


```r
library(tidyverse)
library(wesanderson)
library(kableExtra)
library(scales, exclude = "date_format")
library(glue)
library(tidyquant)
library(clock)
```


```r
theme_set(theme_bw())

(cols <- wes_palette("Moonrise2"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />


```r
symbols <-
  c(
    "SPY",
    "XLV",
    "XLK",
    "XLE",
    "XLF",
    "XLC",
    "XLI",
    "XLY",
    "XLP",
    "XLRE",
    "XLU",
    "XLB"
  )

from <- "2020-02-19"

from_formatted <- date_parse(from, format = "%Y-%m-%d") |> 
  date_format(format = "%b %d, %Y")
```

[Note this patch if having prob lems with `tq_get`](https://stackoverflow.com/questions/72051854/quantmodgetsymbols-cannot-retrieve-data-from-yahoo-finance)


```r
eod_sectors <-
  tq_get(symbols, from = from) |>
  group_by(symbol) |>
  mutate(
    norm_close = adjusted / first(adjusted),
    type = if_else(symbol == "SPY", "Market", "Sector"),
    sector = case_when(
      symbol == "SPY"  ~ "S&P 500",
      symbol == "XLB"  ~ "Materials",
      symbol == "XLE"  ~ "Energy",
      symbol == "XLU"  ~ "Utilities",
      symbol == "XLI"  ~ "Industrical",
      symbol == "XLRE" ~ "Real Estate",
      symbol == "XLV"  ~ "Health",
      symbol == "XLK"  ~ "Technology",
      symbol == "XLF"  ~ "Financial",
      symbol == "XLC"  ~ "Communication",
      symbol == "XLY"  ~ "Consumer Discretionary",
      symbol == "XLP"  ~ "Consumer Staples",
      TRUE             ~ "Other"
    )
  ) |>
  ungroup() |> 
  drop_na()
```

Perhaps not too surprising to see that Tech led the way back. Energy has proven the most volatile, falling further and then recovering faster. And Comms, with all that home-working, benefited during the lockdown, but has faded since.


```r
eod_sectors |>
  mutate(
    sector = str_wrap(sector, 12),
    sector = fct_reorder(sector, norm_close, last, .desc = TRUE)
  ) |>
  ggplot(aes(date, norm_close, colour = type)) +
  geom_rect(aes(xmin = min(date), xmax = max(date), ymin = -Inf, ymax = Inf),
    fill = if_else(eod_sectors$type == "Market", cols[1], NULL), colour = "white"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey80") +
  geom_line(key_glyph = "timeseries") +
  facet_wrap(~sector) +
  scale_colour_manual(values = cols[c(3, 4)]) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "S&P 500 Sector Impact of Covid-19",
    subtitle = glue("Relative to {from_formatted}"),
    x = NULL, y = NULL, colour = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="100%" />

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
   <td style="text-align:left;"> c[1];  conflicts[1];  cumsum[1];  function[1];  max[1];  min[1];  search[1];  sum[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> date_format[1];  date_parse[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[5];  arrange[2];  case_when[1];  desc[2];  group_by[2];  if_else[5];  mutate[6];  summarise[1];  ungroup[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forcats </td>
   <td style="text-align:left;"> fct_reorder[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[2];  element_text[1];  facet_wrap[1];  geom_hline[1];  geom_line[1];  geom_rect[1];  ggplot[1];  labs[1];  scale_colour_manual[1];  scale_y_continuous[1];  theme[1];  theme_bw[1];  theme_set[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> glue </td>
   <td style="text-align:left;"> glue[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[1];  map2_dfr[1];  possibly[1];  set_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scales </td>
   <td style="text-align:left;"> label_percent[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[5];  str_count[1];  str_detect[2];  str_remove[2];  str_remove_all[1];  str_starts[1];  str_wrap[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[1];  tibble[2];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyquant </td>
   <td style="text-align:left;"> tq_get[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> drop_na[1];  unnest[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wesanderson </td>
   <td style="text-align:left;"> wes_palette[1] </td>
  </tr>
</tbody>
</table>

