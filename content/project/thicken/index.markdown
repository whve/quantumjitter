---
title: Plots Thicken
author: Carl Goodwin
date: '2018-02-07'
slug: thicken
categories:
  - R
tags:
  - apps
  - web scraping
summary: Every story needs a good plot. Which plot types generate the most interest on Wikipedia?
lastmod: '2022-04-23'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



One could think of data science as “art grounded in facts”. It tells a story through visualisation. Both story and visualisation rely on a good plot. And an abundance of those has evolved over time. Many have their own dedicated Wikipedia page!

![](/project/thicken/featured.jpeg)

Which generate the most interest? How is the interest in each trending over time? Let's build an interactive app to find out.


```r
library(tidyverse)
library(clock)
library(wesanderson)
library(kableExtra)
library(shiny)
library(rvest)
library(pageviews)
library(shinythemes)
```


```r
theme_set(theme_bw())

(cols <- wes_palette(8, name =  "IsleofDogs1", type = "continuous"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />

I'm going to start by harvesting some data from Wikipedia's [Statistical charts and diagrams category](https://en.wikipedia.org/wiki/Category:Statistical_charts_and_diagrams). I can use this to build a list of all chart types which have a dedicated Wikipedia article page. Using rvest inside the app ensures it will respond to any newly-created articles.


```r
charts <-
  tibble(
    chart = read_html(str_c("https://en.wikipedia.org/wiki/", 
      "Category:Statistical_charts_and_diagrams")) |>
      html_elements(".mw-category-group a") |>
      html_text()
  )
```

The pageviews package provides an API into Wikipedia. I'll create a function wrapped around `article_pageviews` so I can later iterate through a subset of the list established in the prior code chunk.


```r
pv <- function(article) {
  article_pageviews(
    project = "en.wikipedia",
    article,
    user_type = "user",
    start = "2015070100",
    end = date_today("GMT")
  )
}
```

I want an input selector so that a user can choose plot types for comparison. I also want to provide user control of the y-axis scale. A combination of *fixed* and *log10* is better for comparing plots. *Free* scaling reveals more detail in the individual trends.


```r
ui <-
  fluidPage(
    theme = shinytheme("sandstone"),
    titlePanel(NULL, windowTitle = "Plot plotter"),
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          helpText(
            "Choose up to 8 wikipedia article titles to compare. The selection list is from the category: \"statistical charts and diagrams\"."
          ),
          selectizeInput(
            inputId = "article",
            label = "Chart type:",
            choices = charts,
            selected = c(
              "Violin plot",
              "Dendrogram",
              "Histogram",
              "Pie chart"
            ),
            options = list(maxItems = 8),
            multiple = TRUE
          )
        ),
        wellPanel(
          helpText(
            "\"Fixed\" with \"log 10\" scaling (i.e. 10, 100, 1,000) works best for a visual comparison of chart types. \"Free\" is better for examining individual chart trends."
          ),
          selectInput(
            inputId = "scales",
            label = "Fixed or free (y) scale:",
            choices = c("Fixed" = "fixed", "Free" = "free_y"),
            selected = "free_y"
          ),
          selectInput(
            inputId = "log10",
            label = "Log 10 or normal (y) scale:",
            choices = c("Log 10" = "log10", "Normal" = "norm"),
            selected = "norm"
          )
        ),
        img(
          src = "logo.png",
          height = 55
        )
      ),

      mainPanel(plotOutput(outputId = "line"))
    )
  )
```

An earlier version, had `map_dfr` pre-load a dataframe with the pageview data for all chart types (there are more than 100). Profiling with profvis prompted the more efficient approach of loading the data only for the user's selection (maximum of 8).

Profvis also showed that attempting to round the corners of the `plot.background` with additional grid package code was expensive. App efficiency felt more important than minor cosmetic detailing that users would probably barely notice.


```r
server <- function(input, output, session) {
  subsetr <- reactive({
    req(input$article)
    pageviews <- map_dfr(input$article, pv) |>
      mutate(
        date = date_parse(as.character(date), format = "%Y-%m-%d"),
        article = str_replace_all(article, "_", " ")
      )
  })

  output$line <- renderPlot({
    p <- ggplot(
      subsetr(),
      aes(date,
        views,
        colour = article
      )
    ) +
      geom_line() +
      theme_bw() +
      theme(
        rect = element_rect(fill = "#f9f5f1"),
        plot.background = element_rect(fill = "#f9f5f1")
      ) +
      scale_colour_manual(values = cols) +
      geom_smooth(colour = cols[7]) +
      facet_wrap(~article, nrow = 1, scales = input$scales) +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        x = NULL, y = NULL,
        title = "Wikipedia Daily Page Views\n",
        caption = "\nSource: en.wikipedia (excludes bots)"
      )

    p2 <- switch(input$log10,
      norm =
        p,
      log10 =
        p + scale_y_log10(breaks = c(1, 10, 100, 1000, 10000))
    )
    p2
  })
}

shinyApp(ui = ui, server = server, options = list(height = 1000))
```

<iframe src="https://quantumjitter.shinyapps.io/thicken/" width="100%" height="1000" scrolling="no" frameBorder="0"></iframe>

Note the utility of selecting the right scaling. The combination of “fixed” and “normal” reveals what must have been “world histogram day” on July 27th 2015, but little else.

Turning non-interactive code into an app sharpens the mind’s focus on performance. And profvis, integrated into RStudio via the profile menu option, is a wonderful “tool for helping you understand how R spends its time”.

My first version of the app was finger-tappingly slow.

Profvis revealed the main culprit to be the pre-loading of a dataframe with the page-view data for all chart types (there are more than 100). Profiling prompted the more efficient “reactive” approach of loading the data only for the user’s selection (maximum of 8).

Profiling also showed that rounding the corners of the plot.background with additional grid-package code was expensive. App efficiency felt more important than minor cosmetic detailing (to the main panel to match the theme’s side panel). And most users would probably barely notice (had I not drawn attention to it here).

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
   <td style="text-align:left;"> as.character[1];  c[4];  conflicts[1];  cumsum[1];  function[3];  list[2];  search[1];  sum[1];  switch[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> date_parse[1];  date_today[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[5];  arrange[2];  desc[2];  group_by[1];  if_else[3];  mutate[5];  summarise[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[1];  element_rect[2];  element_text[2];  facet_wrap[1];  geom_line[1];  geom_smooth[1];  ggplot[1];  labs[1];  scale_colour_manual[1];  scale_y_log10[1];  theme[2];  theme_bw[2];  theme_set[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pageviews </td>
   <td style="text-align:left;"> article_pageviews[1] </td>
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
   <td style="text-align:left;"> html_elements[1];  html_text[1];  read_html[1];  session[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> shiny </td>
   <td style="text-align:left;"> fluidPage[1];  helpText[2];  img[1];  mainPanel[1];  plotOutput[1];  reactive[1];  renderPlot[1];  req[1];  selectInput[2];  selectizeInput[1];  shinyApp[1];  sidebarLayout[1];  sidebarPanel[1];  titlePanel[1];  wellPanel[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> shinythemes </td>
   <td style="text-align:left;"> shinytheme[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[6];  str_count[1];  str_detect[2];  str_remove[2];  str_remove_all[1];  str_replace_all[1];  str_starts[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[1];  tibble[3];  tribble[1];  enframe[1] </td>
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
