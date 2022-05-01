---
title: Bootstraps & Bandings
author: Carl Goodwin
date: '2022-03-08'
slug: bands
categories:
  - R
tags:
  - statistical inference
  - regex
  - special effects
summary: Are the residential property bands of [3 decades ago](https://www.gov.uk/guidance/understand-how-council-tax-bands-are-assessed#council-tax-bands-in-england-based-on-1-april-1991-values) becoming less so? Would a sample of those recently-sold reveal band convergence? And what may be inferred about those not sampled?
lastmod: '2022-05-01'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



![](/project/bands/featured.JPG)

Are the distinct residential property bands of [3 decades ago](https://www.gov.uk/guidance/understand-how-council-tax-bands-are-assessed#council-tax-bands-in-england-based-on-1-april-1991-values) becoming less so?

Over the years, urban properties have been added to and divided up. And two streets of equal attractiveness, and with equivalently-banded properties, may have diverged as neighbourhoods evolved. 

Whilst properties can and do move to higher or lower bands following alteration, would a sample of those recently-sold reveal band convergence after so long? And what may be inferred about the wider housing stock?


```r
library(tidyverse)
library(rvest)
library(scales, exclude = "date_format")
library(SPARQL)
library(clock)
library(kableExtra)
library(RColorBrewer)
library(glue)
library(vctrs)
library(janitor)
library(infer)
library(tsibble)
library(ggfx)
```

Setting the theme and colour palette for all graphics (with a little help from the [ggfx](https://ggfx.data-imaginist.com) package).


```r
theme_set(theme_bw())

col <- "RdYlBu"

scale_fill_continuous <- function(...) scale_fill_distiller(palette = col)

cols <- brewer.pal(7, col)

tibble(x = 1, y = 1, fill = 7:1) |> 
  ggplot(aes(x, y, fill = fill)) +
  as_reference(geom_col(show.legend = FALSE), id = "cols") +
  with_blend(
    geom_text(
      x = 1,
      y = 3.5,
      label = col,
      size = 40,
      fontface = "bold"
    ),
    bg_layer = "cols",
    blend_type = "atop",
    flip_order = TRUE,
    id = "text"
  ) +
  with_outer_glow("text") +
  scale_fill_continuous() +
  coord_flip() +
  theme_void()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/theme-1.png" width="100%" />

[Property band](https://www.gov.uk/council-tax-bands) and [price-paid](https://landregistry.data.gov.uk/app/qonsole) data are separately sourced. The free-form street address is the only way to bring the two together. The structure, content and even spelling of the address sometimes differ, for example: "FLAT C, 22 SOME STREET, SOME AREA, SW10 1AA" in one may be "22C 2ND FLR, HOUSE NAME, SOME STREET SW10 1AA" in the other.

So, a little string manipulation is needed to create a common key. And reusable patterns will enable a consistent application to both.


```r
remove_pattern <-
  str_c(
    ", London, SW10 .+$",
    "FLAT ",
    "APARTMENT ",
    "CHELSEA HARBOUR",
    "(?<=COURT|SANDHILLS| HOUSE|WALK|ESTATE|ROW).*",
    "[,'\\.]",
    "(?<= )AT ",
    "(?<=VINT)N",
    "(?<=FARRIER)S",
    "(1ST|2ND|3RD|4TH|5TH|6TH) FLR ",
    "FLR (1ST|2ND|3RD|4TH|5TH|6TH) ",
    " ?- ?[0-9]{1,3}",
    sep = "|"
  )

swizzle_from <- "^([0-9]{1,3})([A-Z])(?= .*)" 
swizzle_to <- "\\2 \\1"
```

Council Tax band data are available for non-commercial use under the [Open Government Licence v3.0 (OGL)](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).


```r
url1 <-
  str_c(
    "https://www.tax.service.gov.uk/",
    "check-council-tax-band/",
    "search-council-tax-advanced?",
    "postcode=Fkvms5WVQum-uX3L00_pcA&",
    "filters.councilTaxBands="
  )

url2 <- "&filters.propertyUse=N&postcode=Fkvms5WVQum-uX3L00_pcA&page="

url3 <- "&filters.bandStatus=Current"

index <- crossing(band = LETTERS[1:8], page = seq(0, 120, 1))

band_df <- map2_dfr(index$band, index$page, possibly(function(i, j) {
  str_c(url1, i, url2, j, url3) |>
    read_html() |>
    html_element("#search-results-table") |>
    html_table(convert = FALSE)
}, otherwise = NA_character_))
```




```r
band_df2 <- 
  band_df |> 
  clean_names() |> 
  mutate(postcode = str_extract(address, "SW10 .+$"),
         raw_band_address = str_remove(address, ", London, SW10 .+$"),
         address = str_remove_all(address, remove_pattern),
         address = str_replace(address, swizzle_from, swizzle_to),
         address = str_squish(address)
  )
```

[House price-paid](https://www.gov.uk/guidance/about-the-price-paid-data#explanations-of-column-headers-in-the-ppd) data are similarly available for non-commercial use under the OGL.


```r
endpoint <- "https://landregistry.data.gov.uk/landregistry/query"

query <- '
PREFIX  xsd:  <http://www.w3.org/2001/XMLSchema#>
PREFIX  text: <http://jena.apache.org/text#>
PREFIX  ppd:  <http://landregistry.data.gov.uk/def/ppi/>
PREFIX  lrcommon: <http://landregistry.data.gov.uk/def/common/>

SELECT  ?ppd_propertyAddress ?ppd_transactionCategory ?ppd_transactionDate ?ppd_pricePaid ?ppd_estateType ?ppd_propertyAddressCounty ?ppd_propertyAddressDistrict ?ppd_propertyAddressLocality ?ppd_propertyAddressPaon ?ppd_propertyAddressPostcode ?ppd_propertyAddressSaon ?ppd_propertyAddressStreet ?ppd_propertyAddressTown ?ppd_propertyType ?ppd_recordStatus

WHERE
  { { ?ppd_propertyAddress
                text:query               ( lrcommon:postcode "( SW10 )" 3000000 ) .
      ?item     ppd:propertyAddress      ?ppd_propertyAddress ;
                ppd:transactionCategory  ppd:standardPricePaidTransaction ;
                ppd:transactionDate      ?ppd_transactionDate ;
                ppd:pricePaid            ?ppd_pricePaid ;
      FILTER ( ?ppd_transactionDate >= "2020-01-01"^^xsd:date )
    }
    OPTIONAL{ ?item  ppd:estateType  ?ppd_estateType }
    OPTIONAL{ ?ppd_propertyAddress lrcommon:county  ?ppd_propertyAddressCounty}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:district  ?ppd_propertyAddressDistrict}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:locality  ?ppd_propertyAddressLocality}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:paon  ?ppd_propertyAddressPaon}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:postcode  ?ppd_propertyAddressPostcode}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:saon  ?ppd_propertyAddressSaon}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:street  ?ppd_propertyAddressStreet}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:town  ?ppd_propertyAddressTown}
    OPTIONAL{ ?item  ppd:propertyType  ?ppd_propertyType }
    OPTIONAL{ ?item  ppd:recordStatus  ?ppd_recordStatus }
    BIND(ppd:standardPricePaidTransaction AS ?ppd_transactionCategory)
  }'

prices_list <- SPARQL(endpoint, query)
```


```r
prices_df2 <-
  prices_list$results |>
  as_tibble() |> 
  clean_names() |>
  rename_with(~ str_remove_all(., "ppd_|property_address_")) |>
  mutate(
    transaction_date = new_datetime(transaction_date) |> as_date(),
    price_paid = price_paid / 1000000
  ) |>
  filter(transaction_date < "2022-01-01") |>
  mutate(
    raw_price_address = str_c(str_replace_na(saon, ""), 
                              paon, street, sep = " ") |> str_squish(),
    address = str_remove_all(raw_price_address, remove_pattern),
    address = str_replace(address, swizzle_from, swizzle_to)
  ) |>
  select(
    address,
    raw_price_address,
    postcode,
    price_paid,
    transaction_date,
    estate_type,
    property_type,
    transaction_category
  )
```

Now there's a common key to join the data.


```r
joined_df <-
  prices_df2 |>
  inner_join(band_df2, by = c("address", "postcode")) |>
  relocate(raw_band_address, .after = raw_price_address) |>
  arrange(postcode, address) |>
  mutate(council_tax_band = factor(council_tax_band))
```



As with previous posts [Digging Deep](https://www.quantumjitter.com/project/planning/) and [House Sales](https://www.quantumjitter.com/project/sw10/), I'm focusing on postcodes in the SW10 part of London.

It's not possible to assess all SW10 properties by band since only a tiny fraction will have been sold recently. Recent sales could though be used as a sample and [Bootstrap Confidence Intervals](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)) then employed to draw a wider inference.

["Pulling yourself up by your bootstraps"](https://www.huffingtonpost.co.uk/entry/pull-yourself-up-by-your-bootstraps-nonsense_n_5b1ed024e4b0bbb7a0e037d4) originally meant doing something absurd. Later it came to mean succeeding with only what you have at your disposal. Hence only the sample will be used as a surrogate for the true population by making repeated random draws from it (with replacement). 

A key assumption is that the sample is representative of the true population. 

Even though only recent sales transactions have been selected, a small movement in market prices will have occurred. So ensuring the bands are reasonably well distributed over the period is worthwhile.


```r
joined_df |>
  select(transaction_date, price_paid, council_tax_band) |>
  mutate(yearquarter = yearquarter(transaction_date)) |>
  count(yearquarter, council_tax_band) |>
  ggplot(aes(yearquarter, n, fill = council_tax_band)) +
  geom_col(position = position_fill()) +
  scale_x_yearquarter() +
  scale_y_continuous(labels = label_percent(1)) +
  scale_fill_manual(values = cols[c(1:7)]) +
  labs(
    title = "Distribution of Sales Transactions by Band & Quarter",
    x = "Quarter", y = "Proportion", fill = "Band"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/distribution-1.png" width="100%" />

A violin plot of the property values by band shows some bimodal distribution and oddly shows bands E & F with lower mean prices than band D. This is worth closer inspection to ensure the sample is representative.


```r
labels <- joined_df |>
  group_by(council_tax_band) |>
  summarise(n = n(), mean_price = mean(price_paid))

transactions <-
  joined_df |>
  count() |>
  pull()

from <- joined_df |>
  summarise(min(transaction_date) |> yearquarter()) |>
  pull()

to <- joined_df |>
  summarise(max(transaction_date) |> yearquarter()) |>
  pull()

joined_df |>
  ggplot(aes(council_tax_band, price_paid)) +
  geom_violin(fill = cols[1]) +
  geom_label(aes(label = glue(
    "n = {n} \nAvg Price ",
    "{dollar(mean_price, prefix = '£', suffix = 'm', accuracy = 0.01)}"
  ), y = 16),
  data = labels, size = 2.3, alpha = 0.7, fill = "white"
  ) +
  scale_y_log10(labels = label_dollar(
    prefix = "£",
    suffix = "m", accuracy = 0.1
  )) +
  labs(
    title = "Droopy Bandings",
    subtitle = glue(
      "Sample of {transactions} Property ",
      "Transactions in SW10 ({from} to {to})"
    ),
    x = "Council Tax Band", y = "Sale Price (log10 scale)",
    caption = "Sources: tax.service.gov.uk & landregistry.data.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/visualise-1.png" width="100%" />


```r
joined_df2 <- joined_df |>
  mutate(`SW10 0JR` = if_else(postcode == "SW10 0JR", "Yes", "No"))
```

It turns out that the unusual transactions below £0.3m are almost entirely from one postcode as shown below when isolating "SW10 0JR". This appears to be a single large new development with all sub-units sold in 2020.

These specific transactions feel somewhat unusual at these banding levels. And irrespective of their accuracy, a sample of 158 postcodes heavily dominated by the transactions of just one would not be representative of the true population.


```r
joined_df2 |>
  ggplot(aes(council_tax_band, price_paid, fill = `SW10 0JR`)) +
  geom_violin() +
  geom_label(aes(label = glue(
    "n = {n} \nAvg Price\n",
    "{dollar(mean_price, prefix = '£', suffix = 'm', accuracy = 0.01)}"
  ), y = 16),
  data = labels, size = 2.3, alpha = 0.7, fill = "white"
  ) +
  geom_hline(yintercept = 0.3, linetype = "dashed") +
  scale_y_log10(labels = label_dollar(
    prefix = "£",
    suffix = "m", accuracy = 0.1
  )) +
  scale_fill_manual(values = cols[c(1, 5)]) +
  labs(
    title = "Unusual Bandings",
    subtitle = glue(
      "Sample of {transactions} Property ",
      "Transactions in SW10 ({from} to {to})"
    ),
    x = "Council Tax Band", y = "Sale Price (log10 scale)",
    caption = "Sources: tax.service.gov.uk & landregistry.data.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/anomaly-1.png" width="100%" />

```r
joined_df2 |>
  count(postcode, sort = TRUE) |>
  slice_head(n = 10) |>
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> postcode </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> SW10 0JR </td>
   <td style="text-align:right;"> 76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SW10 0HQ </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SW10 0AA </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SW10 0HG </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SW10 0DD </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SW10 0UY </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SW10 9AD </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SW10 9JP </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SW10 9RH </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SW10 0AZ </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>

So, I'll remove this postcode.


```r
joined_df3 <- joined_df |> 
  filter(postcode != "SW10 0JR")
```

This now feels like a representative sample of 284 property transactions. And broadly-speaking the plot shows a progression in average property values as we step through the bands. There is though substantial convergence between some, with the "drippy" band E still looking almost indistinguishable from band D.


```r
labels <- joined_df3 |>
  group_by(council_tax_band) |>
  summarise(n = n(), mean_price = mean(price_paid))

transactions <-
  joined_df3 |>
  count() |>
  pull()

joined_df3 |>
  ggplot(aes(council_tax_band, price_paid)) +
  geom_violin(fill = cols[1]) +
  geom_label(aes(label = glue(
    "n = {n} \nAvg Price ",
    "{dollar(mean_price, prefix = '£', suffix = 'm', accuracy = 0.01)}"
  ), y = 16),
  data = labels, size = 2.3, alpha = 0.7, fill = "white"
  ) +
  scale_y_log10(labels = label_dollar(prefix = "£", 
                                       suffix = "m", accuracy = 0.1)) +
  labs(
    title = "Drippy Bandings",
    subtitle = glue(
      "Sample of {transactions} Property ",
      "Transactions in SW10 ({from} to {to})"
    ),
    x = "Council Tax Band", y = "Sale Price (log10 scale)",
    caption = "Sources: tax.service.gov.uk & landregistry.data.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/representative-1.png" width="100%" />



Can we infer that the true population of band Es no longer exhibits any difference in mean values with respect to band D?


```r
bands_ef <- 
  joined_df3 |>
  filter(council_tax_band %in% c("E", "D"))

obs_stat <- 
  bands_ef |> 
  specify(price_paid ~ council_tax_band) |>
  calculate(stat = "diff in means", order = c("E", "D")) |>
  pull()

set.seed(2)

boot_dist <-
  bands_ef |> 
  specify(price_paid ~ council_tax_band) |>
  generate(reps = 2000, type = "bootstrap") |>
  calculate(stat = "diff in means", order = c("E", "D"))

perc_ci <- get_ci(boot_dist)

lower <- perc_ci |>
  pull(lower_ci) |>
  dollar(prefix = "£", suffix = "m", accuracy = 0.01)
upper <- perc_ci |>
  pull(upper_ci) |>
  dollar(prefix = "£", suffix = "m", accuracy = 0.01)

boot_dist |>
  visualise() +
  shade_confidence_interval(
    endpoints = perc_ci,
    color = cols[6], fill = cols[3]
  ) +
  geom_vline(xintercept = obs_stat, linetype = "dashed", colour = "white") +
  annotate("label",
    x = -0.12, y = 350, size = 3,
    label = glue(
      "Observed Difference\nBetween Bands D & E is ",
      "{dollar(obs_stat, prefix = '£', suffix = 'm', accuracy = 0.01)}"
    )
  ) +
  scale_x_continuous(labels = label_dollar(
    prefix = "£",
    suffix = "m", accuracy = 0.1
  )) +
  labs(
    subtitle = glue(
      "95% Confident the Difference ",
      "in Mean Prices Between Bands D & E is {lower} to {upper}"
    ),
    x = "Difference in Means", y = "Count",
    caption = "Sources: tax.service.gov.uk & landregistry.data.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/infer-1.png" width="100%" />

Bootstrapping with a 95% confidence interval suggests the true difference in mean prices between all band D and E properties in SW10 is somewhere in the range -£0.10m to £0.12m. Considerable convergence compared to [3 decades ago](https://www.gov.uk/guidance/understand-how-council-tax-bands-are-assessed#council-tax-bands-in-england-based-on-1-april-1991-values) when the band E minimum exceeded the band D maximum.

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
   <td style="text-align:left;"> c[4];  conflicts[1];  cumsum[1];  factor[1];  function[3];  max[1];  mean[2];  min[1];  readRDS[1];  saveRDS[1];  scale[3];  search[1];  seq[1];  set.seed[2];  sum[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> as_date[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[9];  across[1];  arrange[3];  count[4];  desc[2];  group_by[3];  if_else[4];  inner_join[1];  mutate[11];  n[2];  pull[7];  relocate[1];  rename_with[1];  select[4];  slice_head[1];  slice_sample[1];  summarise[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggfx </td>
   <td style="text-align:left;"> as_reference[1];  with_blend[1];  with_outer_glow[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[8];  annotate[1];  coord_flip[1];  geom_col[2];  geom_hline[1];  geom_label[3];  geom_text[1];  geom_violin[3];  geom_vline[1];  ggplot[5];  labs[5];  position_fill[1];  scale_fill_distiller[1];  scale_fill_manual[2];  scale_x_continuous[1];  scale_y_continuous[1];  scale_y_log10[3];  theme_bw[1];  theme_set[1];  theme_void[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> glue </td>
   <td style="text-align:left;"> glue[8] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> infer </td>
   <td style="text-align:left;"> calculate[2];  generate[1];  get_ci[1];  shade_confidence_interval[1];  specify[2];  visualise[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> janitor </td>
   <td style="text-align:left;"> clean_names[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[1];  map2_dfr[2];  possibly[2];  set_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RColorBrewer </td>
   <td style="text-align:left;"> brewer.pal[1] </td>
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
   <td style="text-align:left;"> scales </td>
   <td style="text-align:left;"> dollar[6];  label_dollar[4];  label_percent[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPARQL </td>
   <td style="text-align:left;"> SPARQL[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[9];  str_count[1];  str_detect[2];  str_extract[1];  str_remove[3];  str_remove_all[5];  str_replace[2];  str_replace_na[1];  str_squish[2];  str_starts[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[2];  tibble[3];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> crossing[1];  fill[1];  unnest[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tsibble </td>
   <td style="text-align:left;"> scale_x_yearquarter[1];  yearquarter[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vctrs </td>
   <td style="text-align:left;"> new_datetime[1] </td>
  </tr>
</tbody>
</table>

## Attribution

Contains HM Land Registry data © Crown copyright and database right 2021. This data is licensed under the [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

