---
title: Digging Deep
author: Carl Goodwin
date: '2018-01-10'
slug: planning
categories:
  - R
tags:
  - time series
  - statistics
  - apps
summary: "Do we see more planning applications when house sales are depressed?"
lastmod: '2022-04-22'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



In [House Sales](/project/sw10) I looked at how a series of events damped down sales. By combining these sales data with planning applications I'd like to see if home owners "start digging" when they can't sell.

![](/project/planning/featured.JPG)

Planning data is harvested with the kind permission of The Royal Borough of Kensington and Chelsea (RBKC). The code for these code chunks is not rendered out of courtesy to RBKC.


```r
library(tidyverse)
library(rvest)
library(SPARQL)
library(quanteda)
library(quanteda.textstats)
library(kableExtra)
library(wesanderson)
library(tictoc)
library(htmlwidgets)
library(clock)
library(fpp3)
library(DT)
library(vctrs)
```


```r
theme_set(theme_bw())

(cols <- wes_palette(name = "Darjeeling2"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />








```r
case_df <- readRDS("case.rds")
```



```r
# url <- "https://www.freemaptools.com/download/full-postcodes/ukpostcodes.zip"
# 
# file_name <- basename(url)
# 
# url |> basename
# 
# download.file(url, file_name)

geocodes <- read_csv("ukpostcodes.zip")
```

The data need a bit of wrangling.  And there is also the opportunity to try the newest column-wise enhancements to mutate: `mutate_if` and `mutate_at` have been superseded by `mutate` with `across`.


```r
wide_df <- case_df |>
  pivot_wider(names_from = X1, values_from = X2) |>
  select(all_of(plan_colnames)) |>
  mutate(across(c(property_list, property_cons), ~na_if(., "N/A")),
         across(c(app_comp, decision), ~na_if(., "")))

tidy_df <- wide_df |>
  mutate(
    dec_date = date_parse(dec_date, format = "%d %b %Y"),
    dec_year = get_year(dec_date),
    proposal_dev = str_to_lower(proposal_dev),
    property_pcode = str_extract(property_add, "SW10[\\s]?\\d[[:alpha:]]{2}"),
    property_pcode = str_replace(property_pcode, "SW10(?!\\s)", "SW10 "),
    app_comp = str_to_upper(app_comp) |>
      str_remove_all("[:punct:]") |>
      str_remove_all("\\b(?:AND|LTD|CO|LIMITED|UK|GROUP|LLP)\\b") |>
      str_squish(),
    decision = fct_explicit_na(decision, na_level = "Other"),
    decision = str_replace(decision, "/", " / "),
    dec_lump = fct_lump(decision, prop = 0.03),
    basement = if_else(str_detect(proposal_dev, "basement"), "Yes", "No"),
    property_listed = case_when(
      property_list %in% c("II", "II*", "2", "2*") ~ "Yes",
      is.na(property_list) ~ "No",
      TRUE ~ "No"
    ),
    app_comp = replace_na(app_comp, "None"),
    property_cons = if_else(property_cons == "" | is.na(property_cons),
      "None", property_cons
    ),
    proposal_dev = if_else(proposal_dev == "" | is.na(proposal_dev),
      "None", proposal_dev
    ),
    across(where(is.character), str_trim),
    across(c("app_comp", "proposal_type", "property_cons"), factor)
  ) |>
  left_join(geocodes, by = c("property_pcode" = "postcode"))

tidy_df |>
  count(dec_lump) |>
  arrange(desc(n)) |>
  kbl(col.names = c("Decision", "Count"))
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Decision </th>
   <th style="text-align:right;"> Count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Grant Planning Permission / Consent </td>
   <td style="text-align:right;"> 5336 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Withdrawn by Applicant </td>
   <td style="text-align:right;"> 1123 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:right;"> 847 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Refuse Planning Permission / Consent </td>
   <td style="text-align:right;"> 752 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Discharge of Conditions - Grant </td>
   <td style="text-align:right;"> 626 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Raise No Objection </td>
   <td style="text-align:right;"> 418 </td>
  </tr>
</tbody>
</table>

I'll use the quanteda package to look at key words in context.





<iframe seamless src="/project/planning/dt1.html" width="100%" height="500"></iframe>

I'd like to review planning applications by theme. So I'll first need to get a sense of what the themes are by plotting the words which appear most frequently.


```r
plus_words <-
  c("new",
    "pp",
    "two",
    "one",
    "dated",
    "withdrawn",
    "flat",
    "x",
    "permission",
    "rear",
    "first",
    "second",
    "planning",
    "floor",
    "erection"
  )

words <- tidy_df |> 
  corpus(text_field = "proposal_dev", 
         doc_vars = c("dec_date", "proposal_type", 
                      "decision", "dec_year")) |> 
  dfm(
    remove = c(stopwords("english"), plus_words),
    remove_numbers = TRUE,
    remove_punct = TRUE) |> 
  textstat_frequency() |> 
  slice_head(n = 30) |> 
  mutate(feature = fct_reorder(feature, frequency))

words |> 
  ggplot(aes(feature, frequency)) +
  geom_col(fill = cols[4]) +
  coord_flip() +
  labs(x = NULL, y = NULL, 
       title = "Frequent Planning Proposal Words",
       caption = "Source: RBKC Planning Search")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="100%" />

Now I can create a theme feature.


```r
tidy_df <- tidy_df |>
  mutate(
    theme = case_when(
      str_detect(proposal_dev, "basement|excav") ~ "Basement or Excavation",
      str_detect(proposal_dev, "exten|conservatory|storey") ~ "Extension, Conservatory \nor Storey",
      str_detect(proposal_dev, "window|door") ~ "Windows or Doors",
      str_detect(proposal_dev, "roof") ~ "Roof",
      str_detect(proposal_type, "Tree") |
        str_detect(proposal_dev, "terrac|landscap|garden") ~ "Trees, Landscaping, \nGarden or Terrace",
      TRUE ~ "Other"
    ),
    outcome = case_when(
      str_detect(decision, "Grant|No Obj|Accept|Lawful") ~ "Positive",
      str_detect(decision, "Refuse") ~ "Refuse",
      str_detect(decision, "Withdrawn") ~ "Withdrawn",
      TRUE ~ "Other"
    )
  )
```

I also want to compare house sales with planning applications over time. So, I'll re-use the SPARQL query from [House Sales](/project/sw10).


```r
tic()

endpoint <- "https://landregistry.data.gov.uk/landregistry/query"

query <- 'PREFIX  text: <http://jena.apache.org/text#>
PREFIX  ppd:  <http://landregistry.data.gov.uk/def/ppi/>
PREFIX  lrcommon: <http://landregistry.data.gov.uk/def/common/>
  
SELECT  ?item ?ppd_propertyAddress ?ppd_hasTransaction ?ppd_pricePaid ?ppd_transactionCategory ?ppd_transactionDate ?ppd_transactionId ?ppd_estateType ?ppd_newBuild ?ppd_propertyAddressCounty ?ppd_propertyAddressDistrict ?ppd_propertyAddressLocality ?ppd_propertyAddressPaon ?ppd_propertyAddressPostcode ?ppd_propertyAddressSaon ?ppd_propertyAddressStreet ?ppd_propertyAddressTown ?ppd_propertyType ?ppd_recordStatus

WHERE
{ ?ppd_propertyAddress text:query _:b0 .
  _:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> lrcommon:postcode .
  _:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:b1 .
  _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> "( SW10 )" .
  _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:b2 .
  _:b2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> 3000000 .
  _:b2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
  ?item ppd:propertyAddress ?ppd_propertyAddress .
  ?item ppd:hasTransaction ?ppd_hasTransaction .
  ?item ppd:pricePaid ?ppd_pricePaid .
  ?item ppd:transactionCategory ?ppd_transactionCategory .
  ?item ppd:transactionDate ?ppd_transactionDate .
  ?item ppd:transactionId ?ppd_transactionId
  
  OPTIONAL { ?item ppd:estateType ?ppd_estateType }
  OPTIONAL { ?item ppd:newBuild ?ppd_newBuild }
  OPTIONAL { ?ppd_propertyAddress lrcommon:county ?ppd_propertyAddressCounty }
  OPTIONAL { ?ppd_propertyAddress lrcommon:district ?ppd_propertyAddressDistrict }
  OPTIONAL { ?ppd_propertyAddress lrcommon:locality ?ppd_propertyAddressLocality }
  OPTIONAL { ?ppd_propertyAddress lrcommon:paon ?ppd_propertyAddressPaon }
  OPTIONAL { ?ppd_propertyAddress lrcommon:postcode ?ppd_propertyAddressPostcode }
  OPTIONAL { ?ppd_propertyAddress lrcommon:saon ?ppd_propertyAddressSaon }
  OPTIONAL { ?ppd_propertyAddress lrcommon:street ?ppd_propertyAddressStreet }
  OPTIONAL { ?ppd_propertyAddress lrcommon:town ?ppd_propertyAddressTown }
  OPTIONAL { ?item ppd:propertyType ?ppd_propertyType }
  OPTIONAL { ?item ppd:recordStatus ?ppd_recordStatus }
}'

sales <- SPARQL(endpoint, query)

toc()
```

```
## 111.924 sec elapsed
```

Let's now bind the data into one tibble and summarise the transaction volumes over time.


```r
sales_df <- sales$results |> 
  as_tibble() |>
  mutate(
    date = new_datetime(ppd_transactionDate) |> as_date(),
    dataset = "Sales"
  ) |>
  group_by(date, dataset) |>
  summarise(volume = n()) |>
  ungroup()

app_df <- tidy_df |>
  mutate(
    date = dec_date,
    dataset = "Planning"
  ) |>
  group_by(date, dataset) |>
  summarise(volume = n()) |>
  ungroup()

compare_df <- bind_rows(app_df, sales_df)

summary_df <- compare_df |>
  filter(date >= min(sales_df$date)) |> 
  mutate(date = date_build(get_year(date), get_month(date), "last")) |> 
  group_by(date, dataset) |>
  summarise(volume = sum(volume)) |> 
  ungroup()
```

The visualisation below does suggest that home owners "start digging" when they can't sell. At least in this part of London.


```r
monthly_ts <- summary_df |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(key = dataset, index = date)

monthly_ts |> 
  ggplot(aes(date, volume, colour = dataset)) +
  geom_line(key_glyph = "timeseries") +
  scale_colour_manual(values = cols[c(2, 3)]) +
  labs(x = NULL, y = NULL, colour = NULL,
       title = "Monthly Property Transaction Volume in SW10",
       caption = "Sources: Land Registry & RBKC Planning"
       )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="100%" />

Time-series data may have an underlying trend and a seasonality pattern. I'll use the seasonal package to decompose each time-series. Each exhibit annual seasonality which evolves over time.


```r
monthly_ts |>
  model(stl = STL(volume ~ season())) |>
  components() |> 
  autoplot() +
  scale_colour_manual(values = cols[c(2, 3)]) +
  labs(x = NULL, title = "Timeseries Decomposition")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="100%" />

We also see some inverse correlation between the two time-series re-affirming the visual conclusion that planning applications increase when the housing market is depressed.


```r
monthly_ts |> 
  pivot_wider(names_from = dataset, values_from = volume) |>
  CCF(Sales, Planning, lag_max = 6) |> 
  autoplot() +
  labs(title = "Correlation Between Sales & Planning") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="100%" />

The overall volumes of planning applications and house transactions in SW10 are fairly similar.


```r
summary_df |>
  group_by(dataset) |>
  summarise(total = sum(volume)) |> 
  kbl(col.names = c("Dataset", "Count"))
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Dataset </th>
   <th style="text-align:right;"> Count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Planning </td>
   <td style="text-align:right;"> 8926 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sales </td>
   <td style="text-align:right;"> 11555 </td>
  </tr>
</tbody>
</table>

Earlier, I added a "theme" feature to the data. So let's take a look at the volume of applications over time faceted by theme and coloured by the outcome. We see that the rise in planning applications is fuelled by basements or excavations, and work on outside landscaping and terracing. So perhaps we do "dig" when we can't sell.


```r
tidy_df |>
  ggplot(aes(dec_year, fill = outcome)) +
  geom_bar() +
  facet_wrap( ~ theme, nrow = 2) +
  scale_fill_manual(values = cols[c(1:4)]) +
  labs(
    title = "Planning Application Themes",
    x = NULL, y = NULL,
    caption = "Source: RBKC Planning Search"
    )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-19-1.png" width="100%" />

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
   <td style="text-align:left;"> as.numeric[1];  basename[1];  c[13];  conflicts[1];  cumsum[1];  factor[1];  function[3];  is.character[1];  is.na[3];  min[1];  readRDS[1];  saveRDS[1];  search[1];  sum[3];  url[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> as_date[1];  date_build[1];  date_parse[1];  get_month[1];  get_year[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[7];  across[4];  arrange[3];  bind_rows[2];  case_when[3];  count[1];  desc[3];  group_by[5];  if_else[6];  left_join[1];  mutate[13];  n[3];  na_if[2];  select[2];  slice_head[1];  summarise[5];  ungroup[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DT </td>
   <td style="text-align:left;"> datatable[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fabletools </td>
   <td style="text-align:left;"> components[1];  model[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> feasts </td>
   <td style="text-align:left;"> CCF[1];  STL[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forcats </td>
   <td style="text-align:left;"> fct_explicit_na[1];  fct_lump[1];  fct_reorder[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> autoplot[2];  aes[3];  coord_flip[1];  element_text[1];  facet_wrap[1];  geom_bar[1];  geom_col[1];  geom_line[1];  ggplot[3];  labs[5];  scale_colour_manual[2];  scale_fill_manual[1];  theme[1];  theme_bw[1];  theme_set[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> htmlwidgets </td>
   <td style="text-align:left;"> saveWidget[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[1];  map_dfr[2];  map2_dfr[1];  possibly[1];  set_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> quanteda </td>
   <td style="text-align:left;"> corpus[2];  dfm[1];  kwic[1];  phrase[1];  stopwords[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> quanteda.textstats </td>
   <td style="text-align:left;"> textstat_frequency[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> read_csv[1];  read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rvest </td>
   <td style="text-align:left;"> html_attr[1];  html_element[2];  html_elements[2];  html_table[1];  html_text[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPARQL </td>
   <td style="text-align:left;"> SPARQL[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stats </td>
   <td style="text-align:left;"> frequency[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[8];  str_count[1];  str_detect[13];  str_extract[1];  str_remove[2];  str_remove_all[3];  str_replace[2];  str_squish[1];  str_starts[1];  str_to_lower[1];  str_to_upper[1];  str_trim[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[3];  tibble[3];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tictoc </td>
   <td style="text-align:left;"> tic[2];  toc[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> pivot_wider[2];  replace_na[1];  unnest[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tsibble </td>
   <td style="text-align:left;"> as_tsibble[1];  yearmonth[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> utils </td>
   <td style="text-align:left;"> download.file[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vctrs </td>
   <td style="text-align:left;"> new_datetime[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wesanderson </td>
   <td style="text-align:left;"> wes_palette[1] </td>
  </tr>
</tbody>
</table>
