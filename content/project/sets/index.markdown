---
title: Set Operations
author: Carl Goodwin
date: '2017-11-14'
slug: sets
categories:
  - R
tags:
  - sets
  - web scraping
  - regex
summary: When visualising a small number of overlapping sets, [Venn diagrams](https://en.wikipedia.org/wiki/Venn_diagram) work well. But what if there are more. Here's a tidy(verse) approach to the exploration of sets and their intersections.
lastmod: '2022-04-20'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



In [Let's Jitter](/project/jitter) I looked at a relatively simple set of sales data. [G-Cloud data](https://www.digitalmarketplace.service.gov.uk/g-cloud/search) offers a much richer source of data with many thousands of services documented by several thousand suppliers and hosted across myriad web pages. These services straddle many categories. I'll use these data to explore sets and their intersections.

![](/project/sets/featured.jpeg)


```r
library(tidyverse)
library(rvest)
library(kableExtra)
library(furrr)
library(wesanderson)
library(tictoc)
library(ggupset)
library(ggVennDiagram)
library(glue)

plan(multicore)
```


```r
theme_set(theme_bw())

(cols <- wes_palette("Royal2"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />

I'm going to focus on the Cloud Hosting lot. Suppliers document the services they want to offer to Public Sector buyers. Each supplier is free to assign each of their services to one or more service categories. It would be interesting to see how these categories overlap when looking at the aggregated data.

I'll begin by harvesting the URL for each category’s search results. And I'll also capture the number of search pages for each category. This will enable me to later control how R iterates through the web pages to extract the required data.


```r
path <- 
  "https://www.digitalmarketplace.service.gov.uk/g-cloud/search?lot=cloud-"

lot_urls <-
  c(
    str_c(path, "hosting"),
    str_c(path, "software"),
    str_c(path, "support")
  )

cat_urls <- future_map_dfr(lot_urls, function(x) {
  nodes <- x |>
    read_html() |>
    html_elements(".app-lot-filter__last-list li a")

  tibble(
    url = nodes |>
      html_attr("href"),

    pages = nodes |>
      html_text()
  )
}) |>
  mutate(
    pages = parse_number(as.character(pages)),
    pages = if_else(pages %% 30 > 0, pages %/% 30 + 1, pages %/% 30),
    lot = str_extract(url, "(?<=cloud-)[\\w]+"),
    url = str_remove(url, ".*(?=&)")
  )

version <- lot_urls[[1]] |> 
  read_html() |> 
  html_elements(".app-search-result:first-child") |> 
  html_text() |> 
  str_extract("G-Cloud \\d\\d")
```

So now I'm all set to parallel process through the data at two levels. At category level. And within each category, I'll iterate through the multiple pages of search results, harvesting 100 service IDs per page. 

I'll also auto-abbreviate the category names so I’ll have the option of more concise names for less-cluttered plotting later on.


```r
tic()

data_df <-
  future_pmap_dfr(
    list(
      cat_urls$url,
      cat_urls$pages,
      cat_urls$lot
    ),
    function(x, y, z) {
      future_map_dfr(1:y, function(y) {
        refs <- str_c(
          "https://www.digitalmarketplace.service.gov.uk/g-cloud/search?page=",
          y,
          x,
          "&lot=cloud-",
          z
        ) |>
          read_html() |>
          html_elements("#js-dm-live-search-results .govuk-link") |>
          html_attr("href") 
        
       tibble(
            lot = str_c("Cloud ", str_to_title(z)),
            id = str_extract(refs, "[[:digit:]]{15}"),
            cat = str_remove(x, "&serviceCategories=") |>
              str_replace_all("\\Q+\\E", " ") |>
              str_remove("%28[[:print:]]+%29")
          )
      })
    }
  ) |>
  select(lot:cat) |>
  mutate(
    cat = str_trim(cat) |> str_to_title(),
    abbr = str_remove(cat, "and") |> abbreviate(3) |> str_to_upper()
  )

toc()
```

```
## 5007.295 sec elapsed
```

```r
# Uncached, 78 mins with multicore
```

Now that I have a nice tidy [tibble](https://tibble.tidyverse.org), I can start to think about visualisations.

I like Venn diagrams. But to create one I'll first need to do a little prep as `ggVennDiagram` requires separate character vectors for each set. 


```r
host_df <- data_df |>
  filter(lot == "Cloud Hosting") |>
  group_by(abbr)

keys <- host_df |> 
  group_keys() |> 
  pull(abbr)

all_cats <- host_df |> 
  group_split() |>
  map("id") |> 
  set_names(keys)
```

Venn diagrams work best with a small number of sets. So we’ll select four categories.


```r
four_cats <- all_cats[c("CAAH", "PAAS", "OBS", "IND")]

four_cats |> 
  ggVennDiagram(label = "count", label_alpha = 0) +
  scale_fill_gradient(low = cols[3], high = cols[5]) +
  scale_colour_manual(values = cols[c(rep(4, 4))]) +
  labs(
    x = "Category Combinations", y = NULL, fill = "# Services",
    title = "The Most Frequent Category Combinations",
    subtitle = glue("Focusing on Four {version} Service Categories"),
    caption = "Source: digitalmarketplace.service.gov.uk\n"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="100%" />

Let's suppose I want to find out which Service IDs lie in a particular intersection. Perhaps I want to go back to the web site with those IDs to search for, and read up on, those particular services. I could use purrr's `reduce` to achieve this.  For example, let's extract the IDs at the heart of the Venn which intersect all categories.


```r
four_cats |> reduce(intersect)
```

```
##  [1] "616441239909908" "162482129495664" "905776993996934" "376219180313371"
##  [5] "407652487989048" "225809803026529" "351972947261871" "141467821661395"
##  [9] "428474094741326" "407756608170776" "124196562348938" "550690199626866"
## [13] "150223927888075" "860854746891399"
```

And if we wanted the IDs intersecting the "OBS" and "IND" categories?


```r
list(
  four_cats$OBS,
  four_cats$IND
) |>
  reduce(intersect)
```

```
##  [1] "616441239909908" "151022407134695" "852500112300305" "162482129495664"
##  [5] "905776993996934" "376219180313371" "407652487989048" "474814461260648"
##  [9] "225809803026529" "351972947261871" "576030340787556" "141467821661395"
## [13] "758507645451314" "428474094741326" "407756608170776" "774442655261342"
## [17] "853411470123271" "504203548963816" "100473171421762" "456595685823942"
## [21] "513866403672024" "110346802387127" "580227303306629" "911282351993476"
## [25] "948752760228442" "124196562348938" "496076310695431" "550690199626866"
## [29] "150223927888075" "675399761262086" "497814439666264" "615391229994933"
## [33] "860854746891399"
```

Sometimes though we need something a little more scalable than a Venn diagram. The ggupset package provides a good solution. Before we try more than four sets though, I'll first use the same four categories so we may compare the visualisation to the Venn.


```r
set_df <- data_df |>
  filter(abbr %in% c("CAAH", "PAAS", "OBS", "IND")) |>
  group_by(id) |>
  mutate(category = list(cat)) |>
  distinct(id, category) |>
  group_by(category) |>
  mutate(n = n()) |>
  ungroup()

set_df |>
  ggplot(aes(category)) +
  geom_bar(fill = cols[1]) +
  geom_label(aes(y = n, label = n), vjust = -0.1, size = 3, fill = cols[5]) +
  scale_x_upset() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.border = element_blank()) +
  labs(
    x = "Category Combinations", y = NULL,
    title = "The Most Frequent Category Combinations",
    subtitle = glue("Focusing on Four {version} Service Categories"),
    caption = "Source: digitalmarketplace.service.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="100%" />

Now let’s take a look at the intersections across all the categories. And let’s suppose that our particular interest is all services which appear in one, and only one, category.


```r
set_df <- data_df |>
  group_by(id) |>
  filter(n() == 1, lot == "Cloud Hosting") |>
  mutate(category = list(cat)) |>
  distinct(id, category) |>
  group_by(category) |>
  mutate(n = n()) |>
  ungroup()

set_df |>
  ggplot(aes(category)) +
  geom_bar(fill = cols[2]) +
  geom_label(aes(y = n, label = n), vjust = -0.1, size = 3, fill = cols[3]) +
  scale_x_upset(n_sets = 10) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.border = element_blank()) +
  labs(
    x = "Category Combinations", y = NULL,
    title = "10 Most Frequent Single-Category Services",
    subtitle = "Focused on Service Categories in the Cloud Hosting Lot",
    caption = "Source: digitalmarketplace.service.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="100%" />

Suppose we want to extract the intersection data for the top intersections across all sets. I could use functions from the tidyr package to achieve this.


```r
cat_mix <- data_df |>
  filter(lot == "Cloud Hosting") |>
  mutate(x = cat) |>
  pivot_wider(id, names_from = cat, values_from = x, values_fill = "^") |>
  unite(col = intersect, -id, sep = "/") |>
  count(intersect) |>
  mutate(
    intersect = str_replace_all(intersect, "(?:\\Q/^\\E|\\Q^/\\E)", ""),
    intersect = str_replace_all(intersect, "/", " | ")
  ) |>
  arrange(desc(n)) |>
  slice(1:21)

cat_mix |>
  kbl(col.names = c("Intersecting Categories", "Services Count"))
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Intersecting Categories </th>
   <th style="text-align:right;"> Services Count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Platform As A Service </td>
   <td style="text-align:right;"> 719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Compute And Application Hosting </td>
   <td style="text-align:right;"> 209 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Networking </td>
   <td style="text-align:right;"> 147 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Archiving Backup And Disaster Recovery </td>
   <td style="text-align:right;"> 126 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Compute And Application Hosting | Nosql Database | Relational Database | Other Database Services | Networking | Platform As A Service | Search | Block Storage | Object Storage | Other Storage Services </td>
   <td style="text-align:right;"> 115 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Logging And Analysis </td>
   <td style="text-align:right;"> 82 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other Storage Services </td>
   <td style="text-align:right;"> 77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Compute And Application Hosting | Platform As A Service </td>
   <td style="text-align:right;"> 77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Infrastructure And Platform Security </td>
   <td style="text-align:right;"> 71 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other Database Services </td>
   <td style="text-align:right;"> 66 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Container Service </td>
   <td style="text-align:right;"> 52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Archiving Backup And Disaster Recovery | Compute And Application Hosting | Content Delivery Network | Data Warehousing | Distributed Denial Of Service Attack  Protection | Firewall | Infrastructure And Platform Security | Intrusion Detection | Platform As A Service | Protective Monitoring </td>
   <td style="text-align:right;"> 39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Relational Database </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Message Queuing And Processing </td>
   <td style="text-align:right;"> 34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Infrastructure And Platform Security | Intrusion Detection | Logging And Analysis | Protective Monitoring </td>
   <td style="text-align:right;"> 34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Archiving Backup And Disaster Recovery | Compute And Application Hosting | Container Service | Distributed Denial Of Service Attack  Protection | Firewall | Infrastructure And Platform Security | Load Balancing | Networking | Protective Monitoring | Block Storage </td>
   <td style="text-align:right;"> 29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Archiving Backup And Disaster Recovery | Compute And Application Hosting | Platform As A Service </td>
   <td style="text-align:right;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Archiving Backup And Disaster Recovery | Compute And Application Hosting </td>
   <td style="text-align:right;"> 24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Object Storage </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Protective Monitoring </td>
   <td style="text-align:right;"> 19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Block Storage | Object Storage | Other Storage Services </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
</tbody>
</table>

And I can compare this table to the equivalent ggupset visualisation.


```r
set_df <- data_df |>
  group_by(id) |>
  filter(lot == "Cloud Hosting") |>
  mutate(category = list(cat)) |>
  distinct(id, category) |>
  group_by(category) |>
  mutate(n = n()) |>
  ungroup()

set_df |>
  ggplot(aes(category)) +
  geom_bar(fill = cols[5]) +
  geom_label(aes(y = n, label = n), vjust = -0.1, size = 3, fill = cols[4]) +
  scale_x_upset(n_sets = 22, n_intersections = 21) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.border = element_blank()) +
  labs(
    x = "Category Combinations", y = NULL,
    title = "Top Intersections Across all Sets",
    subtitle = "Focused on Service Categories in the Cloud Hosting Lot",
    caption = "Source: digitalmarketplace.service.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="100%" />

And if I want to extract all the service IDs for the top 5 intersections, I could use dplyr and tidyr verbs to achieve this too.

I won't print them all out though!


```r
top5_int <- data_df |>
  filter(lot == "Cloud Hosting") |>
  select(id, abbr) |>
  mutate(x = abbr) |>
  pivot_wider(names_from = abbr, values_from = x, values_fill = "^") |>
  unite(col = intersect, -id, sep = "/") |>
  mutate(
    intersect = str_replace_all(intersect, "(?:\\Q/^\\E|\\Q^/\\E)", ""),
    intersect = str_replace(intersect, "/", " | ")
  ) |>
  group_by(intersect) |>
  mutate(count = n_distinct(id)) |>
  arrange(desc(count), intersect, id) |>
  ungroup() |>
  add_count(intersect, wt = count, name = "temp") |>
  mutate(temp = dense_rank(desc(temp))) |>
  filter(temp %in% 1:5) |>
  distinct(id)

top5_int |>
  summarise(ids = n_distinct(id))
```

```
## # A tibble: 1 × 1
##     ids
##   <int>
## 1  1316
```

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
   <td style="text-align:left;"> abbreviate[1];  as.character[1];  c[6];  cat[5];  conflicts[1];  cumsum[1];  function[4];  list[5];  rep[1];  search[1];  sum[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[12];  intersect[4];  add_count[1];  arrange[4];  count[2];  dense_rank[1];  desc[5];  distinct[4];  group_by[9];  group_keys[1];  group_split[1];  id[7];  if_else[4];  mutate[18];  n[8];  n_distinct[2];  pull[1];  select[2];  slice[1];  summarise[2];  ungroup[4] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> furrr </td>
   <td style="text-align:left;"> future_map_dfr[2];  future_pmap_dfr[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> future </td>
   <td style="text-align:left;"> multicore[1];  plan[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[6];  element_blank[3];  expansion[3];  geom_bar[3];  geom_label[3];  ggplot[3];  labs[4];  scale_colour_manual[1];  scale_fill_gradient[1];  scale_y_continuous[3];  theme[3];  theme_bw[1];  theme_set[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggupset </td>
   <td style="text-align:left;"> scale_x_upset[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggVennDiagram </td>
   <td style="text-align:left;"> ggVennDiagram[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> glue </td>
   <td style="text-align:left;"> glue[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[2];  map2_dfr[1];  possibly[1];  reduce[2];  set_names[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> parse_number[1];  read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rvest </td>
   <td style="text-align:left;"> html_attr[2];  html_elements[3];  html_text[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[10];  str_count[1];  str_detect[2];  str_extract[3];  str_remove[6];  str_remove_all[1];  str_replace[1];  str_replace_all[4];  str_starts[1];  str_to_title[2];  str_to_upper[1];  str_trim[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[1];  tibble[4];  tribble[1];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tictoc </td>
   <td style="text-align:left;"> tic[1];  toc[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> pivot_wider[2];  unite[2];  unnest[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wesanderson </td>
   <td style="text-align:left;"> wes_palette[1] </td>
  </tr>
</tbody>
</table>
