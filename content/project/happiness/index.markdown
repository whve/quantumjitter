---
title: "Finding Happiness in 'The Smoke'"
author: Carl Goodwin
date: '2022-03-19'
slug: happiness
categories:
  - R
tags:
  - dimensionality reduction
  - cluster analysis
  - geospatial
  - machine learning
summary: The Smoke, to use London's nickname, has 32 boroughs plus the central business district known as the City of London. What does Cluster Analysis tell us about the characteristics that bind them?
lastmod: '2022-05-01'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



![](/project/happiness/featured.GIF)

[The Smoke](https://www.theguardian.com/environment/2002/nov/30/uknews.pollution), to use London's nickname, has 32 boroughs plus the central business district known as the [City of London](https://en.wikipedia.org/wiki/City_of_London). What does Cluster Analysis tell us about the characteristics that bind them?


```r
library(tidyverse)
library(tidymodels)
library(tidytext)
library(glue)
library(readxl)
library(janitor)
library(ggrepel)
library(sf)
library(scales)
library(kableExtra)
```

The graphics will use a custom palette created in [Adobe Colour](https://color.adobe.com).


```r
theme_set(theme_bw())

cols <- c("#0AC449", "#CF4E0A", "#0057B7", "#FFD700", "#870AC4") |>
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

[The London Datastore](https://data.london.gov.uk/dataset/london-borough-profiles) provides data profiling each area.


```r
raw_df <-
  read_xlsx("london-borough-profiles.xlsx", sheet = 2) |>
  clean_names() |>
  filter(str_starts(code, "E")) |>
  mutate(across(where(is.character), ~ na_if(., ".")),
    inner_outer_london = str_remove(inner_outer_london, " London")
  )
```

These data include 81 numeric variables quantifying such things as population density, happiness and age. Way too many variables to visualise two-dimensionally. [Principal Components Analysis](https://en.wikipedia.org/wiki/Principal_component_analysis) can reduce the bulk of the information down to two variables. It is then possible to more easily visualise the relationships.

The City of London, aka "The Square Mile", is quite distinct from the other 32 areas and has many NA values.


```r
raw_df |> 
  rowwise() |> 
  mutate(na_count = sum(is.na(cur_data()))) |> 
  select(area_name, na_count) |>
  filter(na_count != 0) |>
  arrange(desc(na_count)) |>
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> area_name </th>
   <th style="text-align:right;"> na_count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> City of London </td>
   <td style="text-align:right;"> 27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kensington and Chelsea </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Barnet </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Camden </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hackney </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Haringey </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Harrow </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Islington </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lewisham </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Merton </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Richmond upon Thames </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Waltham Forest </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wandsworth </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

Not surprisingly, the two-dimensional visualisation sets the City of London apart. And the other 32 are broadly, albeit with some mixing, divided into inner and outer London boroughs.


```r
pca_fit <- raw_df |>
  select(where(is.numeric)) |>
  prcomp(scale = TRUE)

pca_augmented <-
  pca_fit |>
  augment(raw_df)

pca_augmented |>
  ggplot(aes(.fittedPC1, .fittedPC2, fill = inner_outer_london)) +
  geom_label(aes(label = area_name), size = 2, hjust = "inward") +
  scale_fill_manual(values = as.character(cols)) +
  labs(
    title = "33 London Areas", fill = "London",
    x = "Principal Component 1", y = "Principal Component 2",
    caption = "Source: data.london.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="100%" />

After squeezing the many dimensions into two, how much of the original information was it possible to retain?


```r
pca_tidied <- pca_fit |>
  tidy(matrix = "eigenvalues")

pct_explained <-
  pca_tidied |>
  pluck("cumulative", 2)

pca_tidied |>
  ggplot(aes(PC, percent)) +
  geom_col(aes(fill = if_else(PC <= 2, TRUE, FALSE)),
    alpha = 0.8, show.legend = FALSE
  ) +
  scale_y_continuous(labels = label_percent(1)) +
  scale_fill_manual(values = as.character(cols)) +
  coord_flip() +
  labs(
    title = glue(
      "{percent(pct_explained, 0.1)} of the ",
      "Variance Explained by Principal Components 1 & 2"
    ),
    x = "Principal Component", y = NULL
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="100%" />

Whilst we do lose ease of interpretation by distilling the information in this way, it is still possible to understand which of the original variables influenced their two-dimensional positioning.

The axes depicted by the arrows below tell us that **anxiety scores** play a significant role in the placement of the City of London towards the upper-left. **Average age** pushes areas more towards the top. And **happiness** influences the bottom-right.


```r
pattern <- "_\\d{4}|_st.+|_score|_rates|proportion_of_|_\\d{2}_out_of_\\d{2}"

pca_fit |>
  tidy(matrix = "rotation") |>
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") |>
  mutate(column = str_remove_all(column, pattern)) |>
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend = 0, colour = "grey70",
    arrow = arrow(ends = "first", length = unit(8, "pt"))
  ) +
  geom_text_repel(aes(label = column), size = 3) +
  theme_minimal() +
  labs(
    x = "PC 1", y = "PC 2",
    title = "Characteristics Influencing Area Positioning",
    caption = "Source: data.london.gov.uk"
  ) +
  theme(axis.text = element_blank())
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="100%" />

This may be validated by ranking all 33 areas by these three original variables.


```r
pca_long <- 
  pca_augmented |>
  select(area_name, matches("happ|anx|average_age")) |>
  rename_with(~ str_remove(., "_.*")) |>
  rename("avg_age" = "average") |>
  pivot_longer(-area, values_to = "score") |>
  mutate(area = reorder_within(area, score, name)) 

pca_long |>
  ggplot(aes(area, score, colour = name)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~name, scales = "free") +
  scale_x_reordered() +
  scale_colour_manual(values = as.character(cols)) +
  coord_flip() +
  labs(x = NULL, caption = "Source: data.london.gov.uk")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="100%" />

To collect these areas into their natural groupings, a decision is needed on the desired number of clusters. We can visualise dividing the areas into 1, 2, 3 and so forth clusters. And, per below, 3 appears to nicely capture the natural grouping of the coloured points.


```r
set.seed(2022)

kclusts <-
  tibble(k = 1:6) |>
  mutate(
    kclust = map(k, ~ kmeans(pca_augmented |> 
                               select(".fittedPC1", ".fittedPC2"), .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, pca_augmented)
  )

assignments <-
  kclusts |>
  unnest(cols = c(augmented))

clusters <-
  kclusts |>
  unnest(cols = c(tidied))

assignments |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point(aes(color = .cluster)) +
  facet_wrap(~k, nrow = 2) +
  scale_colour_manual(values = as.character(cols[c(1:6)])) +
  geom_point(data = clusters, size = 4, shape = 13) +
  labs(
    title = "How Many Clusters Best Captures the Groupings?",
    subtitle = "X Marks the Cluster Centre",
    caption = "Source: data.london.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="100%" />

The [elbow method](https://en.wikipedia.org/wiki/Elbow_method_(clustering)) provides a more mathematical approach to the choice. The compactness of the clustering (as measured by the total within-cluster sum of squares) is significantly optimised when choosing 3 clusters, with diminishing returns thereafter.


```r
kclusts |>
  unnest(cols = c(glanced)) |>
  ggplot(aes(k, tot.withinss)) +
  geom_line() +
  geom_point() +
  geom_label(aes(label = if_else(k == 3, "Elbow", NA_character_)),
    nudge_y = -25, fill = cols[1]
  ) +
  labs(
    title = "Elbow Method",
    x = "Clusters", y = "Within-Cluster Variance"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="100%" />







And settling on this choice of 3 clusters, we get this split.


```r
assignments |>
  filter(k == 3) |>
  ggplot(aes(.fittedPC1, .fittedPC2, fill = .cluster)) +
  geom_label(aes(label = area_name), size = 2, hjust = "inward", overlap = FALSE) +
  scale_fill_manual(values = as.character(cols[c(1, 2, 4)])) +
  labs(
    title = "Closely-Related London Areas", fill = "Cluster",
    x = "Principal Component 1", y = "Principal Component 2",
    caption = "Source: data.london.gov.uk"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="100%" />

How does this look with [geospatial data](https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london)? And how do the clusters relate to inner and outer London?


```r
shape_df <-
  st_read("statistical-gis-boundaries-london/ESRI",
    "London_Borough_Excluding_MHW",
    as_tibble = TRUE, quiet = TRUE
  ) |>
  left_join(assignments |> 
              filter(k == 3), by = c("GSS_CODE" = "code")) |>
  select(.cluster, inner_outer_london, NAME, geometry) |>
  pivot_longer(c(.cluster, inner_outer_london)) |>
  mutate(value = recode(value, "1" = "Cluster 1", 
                        "2" = "Cluster 2", "3" = "Cluster 3"))

shape_df |>
  mutate(name = recode(name,
    ".cluster" = "By Cluster",
    "inner_outer_london" = "By Inner/Outer"
  )) |>
  ggplot() +
  geom_sf(aes(fill = value)) +
  geom_sf_label(aes(label = NAME), size = 2, alpha = 0.7) +
  scale_fill_manual(values = as.character(cols[c(3, 4, 1, 2, 5)])) +
  facet_wrap(~name) +
  theme_void() +
  theme(legend.position = "none") +
  labs(fill = NULL)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="100%" />

Not too dissimilar, but with some notable differences.

The City of London is a cluster apart in the heart of London. Kensington and Chelsea is an inner-London borough, but exhibits outer-London characteristics. And the reverse is true of the likes of Brent and Greenwich.

Dimensionality reduction is further explored in [East-West Drift](/project/un) coupled with animation.

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
   <td style="text-align:left;"> as.character[7];  c[6];  conflicts[1];  cumsum[1];  function[1];  is.character[1];  is.na[1];  is.numeric[1];  search[1];  set.seed[4];  sum[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[9];  across[1];  arrange[3];  cur_data[1];  desc[3];  group_by[1];  if_else[5];  left_join[1];  mutate[11];  na_if[1];  recode[2];  rename[1];  rename_with[1];  rowwise[1];  select[6];  summarise[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forcats </td>
   <td style="text-align:left;"> fct_inorder[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[17];  annotate[1];  arrow[1];  coord_flip[2];  element_blank[1];  facet_wrap[3];  geom_col[2];  geom_label[4];  geom_line[1];  geom_point[4];  geom_segment[1];  geom_sf[1];  geom_sf_label[1];  ggplot[9];  labs[8];  scale_colour_manual[2];  scale_fill_manual[5];  scale_y_continuous[1];  theme[3];  theme_bw[1];  theme_minimal[1];  theme_set[1];  theme_void[2];  unit[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggrepel </td>
   <td style="text-align:left;"> geom_text_repel[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> glue </td>
   <td style="text-align:left;"> glue[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> janitor </td>
   <td style="text-align:left;"> clean_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[5];  map2_dfr[1];  pluck[1];  possibly[1];  set_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readxl </td>
   <td style="text-align:left;"> read_xlsx[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scales </td>
   <td style="text-align:left;"> label_percent[1];  percent[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> st_read[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stats </td>
   <td style="text-align:left;"> kmeans[2];  prcomp[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[5];  str_count[1];  str_detect[2];  str_remove[4];  str_remove_all[2];  str_starts[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[1];  tibble[4];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> pivot_longer[2];  pivot_wider[1];  unnest[4] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidytext </td>
   <td style="text-align:left;"> reorder_within[1];  scale_x_reordered[1] </td>
  </tr>
</tbody>
</table>
