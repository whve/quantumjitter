---
title: Surprising Stories
author: Carl Goodwin
date: '2017-12-20'
slug: stories
categories:
  - R
tags:
  - geospatial
summary: "A little interactive geospatial mapping and an unexpected find"
lastmod: '2022-04-21'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



Late in 2017 I experimented with geospatial mapping techniques in R. The log file for my blog seemed like a good source of data. I thought it might appeal to a wider audience of one (including me). 

![](/project/stories/featured.jpeg)

Combined with longitude and latitude data from MaxMind’s GeoLite2, this offered a basis for analysis. Although less precise than the GeoIP2 database, this would be more than adequate for my purpose of getting to country and city level.  I settled on the leaflet package for visualisation given the interactivity and pleasing choice of aesthetics.

The results however were a little puzzling.


```r
library(tidyverse)
library(rgeolocate)
library(R.utils)
library(leaflet)
library(rgdal)
library(kableExtra)
library(wesanderson)
library(htmlwidgets)
```


```r
theme_set(theme_bw())

(cols <- wes_palette("Darjeeling2"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-1.png" width="100%" />


```r
zip_file <- "world_shape_file.zip"
shape_file <- "TM_WORLD_BORDERS_SIMPL-0.3"

str_c("http://thematicmapping.org/downloads/", shape_file, ".zip") |>
  download.file(zip_file)

unzip(zip_file)

world_spdf <- readOGR(getwd(), shape_file, verbose = FALSE)
```


```r
url <- "http://geolite.maxmind.com/download/geoip/database/GeoLite2-City.mmdb.gz"

file_name <- basename(url)

download.file(url, file_name)

gunzip(file_name, overwrite = TRUE)
```


```r
stats <- read_csv("stats.csv")
```


```r
ip_df <- map2_df(stats$IP, stats$Pages, function(x, y) {
  maxmind(
    x,
    "GeoLite2-City.mmdb",
    c(
      "country_name",
      "city_name",
      "longitude",
      "latitude",
      "region_name"
    )
  ) |>
    mutate(IP = x) |>
    rename(
      country = country_name,
      region = region_name,
      city = city_name
    ) |>
    mutate(
      Pages = y,
      Views = case_when(
        Pages < 500 ~ 1,
        Pages < 1000 ~ 2,
        Pages < 2000 ~ 3,
        TRUE ~ 4
      )
    )
})

ip_df <- ip_df |>
  filter(!is.na(longitude) | !is.na(latitude)) |>
  arrange(Pages)
```



The concentration of page views in central London was of no immediate surprise as this was likely to be my site maintenance and blogging. What did strike me as odd though was the high concentration of page views in the centre of the US. More curious still, when I zoomed in on Kansas and found myself in the middle of the Cheney Reservoir.


```r
pal <-
  colorFactor(cols[c(2:5)],
    domain = c(1, 2, 3, 4)
  )

map1 <- leaflet(world_spdf) |> # World view
  addProviderTiles(providers$CartoDB.Positron,
    options = providerTileOptions(maxZoom = 21)
  ) |>
  setView(-30, 35, zoom = 2) |> # World view
  addPolygons(
    fillColor = cols[1],
    stroke = TRUE,
    fillOpacity = 1,
    color = cols[5],
    weight = 0.3,
    highlight = highlightOptions(
      weight = 3,
      color = cols[3],
      fillOpacity = 0.3,
      bringToFront = FALSE
    ),
    label = world_spdf@data$NAME,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal"),
      textsize = "12px"
    )
  ) |>
  addCircleMarkers(
    lng = ip_df$longitude,
    lat = ip_df$latitude,
    radius = ~ case_when(
      ip_df$Views == 1 ~ 5,
      ip_df$Views == 2 ~ 10,
      ip_df$Views == 3 ~ 15,
      TRUE ~ 20
    ),
    fillColor = ~ pal(ip_df$Views),
    color = cols[5],
    weight = 1,
    fillOpacity = 0.7,
    popup = str_c(
      "<b>",
      ip_df$city,
      "</b>",
      "<br/>",
      ip_df$region,
      "<br/>",
      as.character(ip_df$Pages),
      " ",
      "page views"
    )
  ) |>
  addLegend(
    colors = cols[c(2:5)],
    labels = c("<500", "500+", "1,000+", "2,000+"),
    opacity = 1,
    title = "Page Views<br/>Oct-23 to Dec-31 2017",
    position = "bottomleft"
  )
```



<iframe seamless src="/project/stories/world.html" width="100%" height="500"></iframe>


I imagined someone drifting in the expanse of water with laptop, flask of coffee and box of sandwiches, whiling away the hours absorbed in my blog.  Perhaps not. How could such a small number of blog pages generate in excess of 2,000 page views in one spot in less than two months?

Then I chanced upon a [BBC news article](https://www.bbc.co.uk/news/technology-37048521) from August 2016. When unable to locate IPs, MaxMind chose the geographical centre of the US as a default. This initially turned out to be a rented house in Kansas, which was rather unfortunate for the occupants, and brought upon them all kinds of unwanted attention.

MaxMind subsequently changed its default centre points to be the middle of bodies of water. And this solved another puzzle. Some of the page views in London appeared to be in the middle of the River Thames.

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
   <td style="text-align:left;"> as.character[1];  basename[1];  c[3];  conflicts[1];  cumsum[1];  function[2];  getwd[1];  is.na[2];  list[1];  search[1];  sum[1];  url[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[6];  arrange[3];  case_when[2];  desc[2];  group_by[1];  if_else[5];  mutate[7];  rename[1];  summarise[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> theme_bw[1];  theme_set[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> htmlwidgets </td>
   <td style="text-align:left;"> saveWidget[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> leaflet </td>
   <td style="text-align:left;"> addCircleMarkers[1];  addLegend[1];  addPolygons[1];  addProviderTiles[1];  colorFactor[1];  highlightOptions[1];  labelOptions[1];  leaflet[1];  providerTileOptions[1];  setView[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[1];  map2_df[1];  map2_dfr[1];  possibly[1];  set_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R.oo </td>
   <td style="text-align:left;"> Package[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R.utils </td>
   <td style="text-align:left;"> gunzip[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> read_csv[1];  read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rgdal </td>
   <td style="text-align:left;"> readOGR[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rgeolocate </td>
   <td style="text-align:left;"> maxmind[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[7];  str_count[1];  str_detect[2];  str_remove[2];  str_remove_all[1];  str_starts[1] </td>
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
   <td style="text-align:left;"> utils </td>
   <td style="text-align:left;"> download.file[2];  unzip[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wesanderson </td>
   <td style="text-align:left;"> wes_palette[1] </td>
  </tr>
</tbody>
</table>
