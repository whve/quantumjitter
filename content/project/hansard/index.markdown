---
title: Cluster of Six
author: Carl Goodwin
date: '2018-01-29'
slug: hansard
categories:
  - R
tags:
  - statistics
summary: "Exploring parliamentary voting patterns with hierarchical clustering"
lastmod: '2022-04-22'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



Before each vote, the Speaker of the House yells "Division! Clear the Lobby". I'd like to find which cluster of MPs (Members of Parliament) may be exiting the lobby and going their own way.

![](/project/hansard/featured.jpeg)

[Hansard](https://hansard.parliament.uk) reports what's said in the UK Parliament, sets out details of divisions, and records decisions taken during a sitting. The R package [hansard package](https://cran.r-project.org/web/packages/hansard/) provides access to the data.


```r
library(tidyverse)
library(clock)
library(wesanderson)
library(kableExtra)
library(hansard)
library(dendextend)
library(corrplot)
library(broom)
library(factoextra)
library(glue)
library(ggrepel)
```


```r
theme_set(theme_bw())

(cols <- wes_palette(name = "Moonrise2"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />

I'll start by building a list of all Labour Party MPs.


```r
url_prefix <- "http://data.parliament.uk/members/"

mps <- commons_members() |>
  filter(party_value == "Labour" | about == str_c(url_prefix, "478")) |>
  mutate(ID = str_replace(about, url_prefix, ""))

saveRDS(mps, file = "mps.rds")
```




Creating a function will enable me to iterate through the MP list to extract their voting records.


```r
start_date <- "2017-06-08"
end_date <- "2018-01-28"

pull_votes <- function(x) {
  mp_vote_record(x,
    start_date = start_date,
    end_date = end_date,
    verbose = FALSE
  ) |>
    mutate(mp = x)
}
```

I'll use it to extract the "aye" and "no" votes. Use of `possibly` prevents the code from stopping when it encounters former MPs for whom no data is returned. 


```r
votes <-
  map(mps$ID, possibly(pull_votes, NULL)) |>
  compact() |>
  map_dfr(simplify, "tibbles") |>
  rename("lobby" = "vote")

saveRDS(votes, file = "votes.rds")
```



Voting the opposite way to the majority of the party, as well as non-votes, will both be of interest when assessing which MPs are "most distant" from the wider party.


```r
votes_df <- votes |>
  left_join(mps, by = c("mp" = "ID")) |>
  select(about = about.x, title, date_value, lobby, mp, name = full_name_value) |>
  transmute(
    vote = if_else(lobby == "aye", 1, -1),
    mp = str_c(name, " (", mp, ")"),
    about = str_replace(about, "http://data.parliament.uk/resources/", ""),
    title = str_c(title, " (", about, ")")
  ) |> 
  select(-about) |> 
  pivot_wider(names_from = title, values_from = vote, values_fill = 0)
```

The data are standardised (i.e. scaled) to ensure comparability. This is verified by ensuring the mean and standard deviation are close to zero and one respectively.


```r
scaled_df <-
  votes_df |>
  mutate(across(-mp, scale))

scaled_df |>
  summarise(across(-mp, list(mean = mean, sd = sd))) |>
  summarise(
    sd_min = min(c_across(ends_with("_sd"))),
    sd_max = max(c_across(ends_with("_sd"))),
    mean_min = min(c_across(ends_with("_mean"))),
    mean_max = max(c_across(ends_with("_mean")))
  ) |>
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> sd_min </th>
   <th style="text-align:right;"> sd_max </th>
   <th style="text-align:right;"> mean_min </th>
   <th style="text-align:right;"> mean_max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

I'd like to assess whether the data contain meaningful clusters rather than random noise. This is achieved quantitatively by calculating the Hopkins statistic, and visually by inspection.

If the [Hopkins statistic](https://en.wikipedia.org/wiki/Hopkins_statistic) is closer to 1 than 0, then we have data which may be clustered.



```r
scaled_df |>
  select(-mp) |>
  get_clust_tendency(nrow(votes_df) - 1) |>
  pluck("hopkins_stat")
```

```
## [1] 0.783357
```

A visual assessment of clustering tendency reveals distance data exhibiting a visible structure.


```r
scaled_df |>
  select(-mp) |>
  dist() |>
  fviz_dist(
    show_labels = FALSE,
    gradient = list(
      low = cols[1],
      mid = cols[3],
      high = cols[4]
    )
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="100%" />

There are eight methods I could use for [hierarchical clustering](https://en.wikipedia.org/wiki/Hierarchical_clustering), and I'll need to determine which will yield results that best fit the data. 

The correlation plot below shows that the *median* and *ward* methods have a weaker correlation with the other five methods.


```r
orig_dist <- scaled_df |>
  select(-mp) |>
  dist()

dend_meths <-
  c(
    "complete",
    "average",
    "single",
    "ward.D",
    "ward.D2",
    "mcquitty",
    "median",
    "centroid"
  )

dend_list <-
  map(dend_meths, function(x) {
    orig_dist |>
      hclust(x) |>
      as.dendrogram()
  })

dend_list |>
  reduce(dendlist) |>
  set_names(dend_meths) |>
  cor.dendlist() |>
  corrplot(
    "pie",
    "lower",
    col = cols[1],
    mar = c(1, 0.5, 4, 0.5),
    order = "AOE",
    tl.cex = 0.8,
    tl.col = "black",
    cl.cex = 0.7
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="100%" />

The above plot does not tell us which method is optimal. For that, I'll take each of the cluster agglomeration methods and calculate their cophenetic distances. I can then correlate these with the original distance to see which offers the best fit.


```r
methods <- list(
  "complete",
  "average",
  "single",
  "ward.D",
  "ward.D2",
  "mcquitty",
  "median",
  "centroid"
)

best_method <- map_dfr(methods, function(x) {
  co_comp <-
    orig_dist |>
    hclust(x) |>
    cophenetic()
  tibble(
    correlation = cor(orig_dist, co_comp),
    method = x
  )
})
```

The plot below confirms the *ward* and *median* methods having a weaker fit. *Average* produces the strongest correlation coefficient of 0.98.


```r
best_method |>
  ggplot(aes(reorder(method, correlation), correlation)) +
  geom_col(fill = cols[1], width = 0.8) +
  geom_text(aes(label = str_c(method, "  ", round(correlation, 2))),
    hjust = 1.3, colour = "white"
  ) +
  coord_flip() +
  labs(
    x = "Method", y = "Correlation",
    title = "Cluster Method Correlation Coefficients",
    caption = "Source: Hansard"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="100%" />

I can now plot the full Labour Party dendrogram using the *average* method. This shows a "cluster of six" MPs which is the last to merge with the rest of the party based on their voting pattern.


```r
dend_avg <- orig_dist |>
  hclust("average") |>
  as.dendrogram()

labels(dend_avg) <- scaled_df$mp[order.dendrogram(dend_avg)]

dend <- dend_avg |>
  color_branches(k = 2, col = cols[4]) |>
  set("labels_cex", 0.4)

start_formatted <- date_parse(start_date, format = "%Y-%m-%d") |> 
  date_format(format = "%b %d, %Y")

end_formatted <- date_parse(end_date, format = "%Y-%m-%d") |> 
  date_format(format = "%b %d, %Y")

ggplot(rev(dend), horiz = TRUE, offset_labels = -0.2) +
  labs(
    y = "\nDistance", title = "Hierarchical Clustering of Labour MPs",
    subtitle = "Based on House of Commons Divisions Since the 2017 Election",
    caption = glue(
      "Source: Hansard ({start_formatted} to {end_formatted})")
  ) +
  theme(panel.border = element_blank())
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="100%" />

I'll zoom in on the "cluster of six".


```r
dend_cuts <- dend |>
  assign_values_to_leaves_nodePar(19, "pch") |>
  assign_values_to_leaves_nodePar(5, "cex") |>
  assign_values_to_leaves_nodePar(cols[1], "col") |>
  set("labels_cex", 0.4) |>
  set("branches_lwd", 2.5) |>
  color_branches(k = 2, col = cols[1]) |>
  cut(h = 50)

ggplot(rev(dend_cuts$lower[[1]]),
  horiz = TRUE,
  nodePar = nodePar,
  offset_labels = -0.5
) +
  labs(
    title = "Cluster of Six",
    subtitle = "MPs who Branch off First in the Dendrogram"
  ) +
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="100%" />

Summarising and sorting the total votes by MP tells me that the "cluster of six" MPs are among the eight MPs voting the fewest times. And I can, for example, verify the record for Emma Reynolds directly via [*Hansard*](https://hansard.parliament.uk/search/MemberContributions?memberId=4077&startDate=2017-06-08&endDate=2018-01-27&type=Divisions&outputType=List).


```r
fewest_votes <- votes |>
  left_join(mps, by = c("mp" = "ID")) |>
  group_by(mp = full_name_value, lobby) |>
  summarise(n_lobby = n()) |>
  ungroup() |> 
  pivot_wider(names_from = "lobby", values_from = "n_lobby") |>
  mutate(total = aye + no,
         mp = fct_reorder(mp, total)) |>
  slice_min(n = 10, order_by = total) |>
  pivot_longer(cols = -mp) |>
  filter(name != "total")

fewest_votes |>
  ggplot(aes(mp, value, fill = name)) +
  geom_col() +
  geom_label(aes(label = value), position = position_stack()) +
  scale_fill_manual(values = cols[c(1, 3)]) +
  coord_flip() +
  labs(title = "Labour MPs Voting Fewest Times",
       y = "Votes", x = NULL, fill = NULL)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="100%" />

Non-voting will not be the only influencing factor. The "distant cluster" will be particularly influenced by a small minority of MPs voting in the opposite direction to the overwhelming majority.  

[*Cook's Distance*](https://en.wikipedia.org/wiki/Cook%27s_distance) visualises these influential outliers. This shows the voting of three MPs, all on the European Union Withdrawal Bill readings, to be particular outliers. All three MPs are in the "cluster of six".


```r
tidy_df <- votes_df |>
  pivot_longer(cols = -mp, names_to = "title", values_to = "vote")

mod <- lm(vote ~ ., data = tidy_df)

mod_df <- mod |>
  augment() |>
  as_tibble()

ggplot(mod_df, aes(title, .cooksd, colour = mp)) +
  geom_jitter() +
  geom_label_repel(aes(label = if_else(.cooksd > 0.002, mp, NULL)), size = 4) +
  scale_colour_manual(values = wes_palette(220, name = "Moonrise2", type = "continuous")) +
  labs(title = "Cook's Distance") +
  coord_flip() +
  theme(
    panel.border = element_blank(),
    axis.text = element_text(size = 6),
    legend.position = "none"
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-18-1.png" width="100%" />


```r
mod_df |>
  filter(str_detect(title, "759161|824379|809989")) |>
  mutate(title = str_wrap(title, 30)) |> 
  ggplot(aes(title, .cooksd, colour = mp)) +
  geom_point(size = 4) +
  geom_label_repel(aes(label = if_else(.cooksd > 0.0015, mp, NULL)), size = 4) +
  ggtitle("Cook's Distance") +
  theme(
    axis.line.x = element_line(color = "grey60"),
    axis.text = element_text(size = 8),
    legend.position = "none",
    axis.title = element_blank()
  ) +
  scale_colour_manual(values = wes_palette(210, name = "Moonrise2", type = "continuous")) +
  coord_flip()
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
   <td style="text-align:left;"> c[5];  conflicts[1];  cumsum[1];  cut[1];  function[4];  labels[1];  list[3];  max[2];  min[2];  nrow[1];  readRDS[2];  rev[2];  round[1];  saveRDS[2];  scale[1];  search[1];  sum[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> broom </td>
   <td style="text-align:left;"> augment[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> date_format[2];  date_parse[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> corrplot </td>
   <td style="text-align:left;"> corrplot[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dendextend </td>
   <td style="text-align:left;"> assign_values_to_leaves_nodePar[3];  color_branches[2];  cor.dendlist[1];  dendlist[1];  set[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> ends_with[4];  filter[8];  across[2];  arrange[2];  c_across[4];  desc[2];  group_by[2];  if_else[6];  left_join[2];  mutate[9];  n[1];  rename[1];  select[5];  slice_min[1];  summarise[4];  transmute[1];  ungroup[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> factoextra </td>
   <td style="text-align:left;"> fviz_dist[1];  get_clust_tendency[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forcats </td>
   <td style="text-align:left;"> fct_reorder[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[8];  coord_flip[4];  element_blank[3];  element_line[1];  element_text[2];  geom_col[2];  geom_jitter[1];  geom_label[1];  geom_point[1];  geom_text[1];  ggplot[6];  ggtitle[1];  labs[5];  position_stack[1];  scale_colour_manual[2];  scale_fill_manual[1];  theme[4];  theme_bw[1];  theme_set[1];  theme_void[1];  unit[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggrepel </td>
   <td style="text-align:left;"> geom_label_repel[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> glue </td>
   <td style="text-align:left;"> glue[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hansard </td>
   <td style="text-align:left;"> commons_members[1];  mp_vote_record[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> compact[1];  map[3];  map_dfr[2];  map2_dfr[1];  pluck[1];  possibly[2];  reduce[1];  set_names[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stats </td>
   <td style="text-align:left;"> as.dendrogram[2];  cophenetic[1];  cor[1];  dist[2];  hclust[3];  lm[1];  reorder[1];  sd[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[9];  str_count[1];  str_detect[3];  str_remove[2];  str_remove_all[1];  str_replace[2];  str_starts[1];  str_wrap[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[2];  tibble[3];  tribble[1];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> pivot_longer[2];  pivot_wider[2];  unnest[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wesanderson </td>
   <td style="text-align:left;"> wes_palette[3] </td>
  </tr>
</tbody>
</table>
