---
title: A Frosty Deal?
author: Carl Goodwin
date: '2020-09-18'
slug: deal
categories:
  - R
tags:
  - text mining
  - word embeddings
  - natural language processing
summary: Before the post-Brexit trade negotiations concluded, what did quantitative textual analysis and word embeddings tell us about the shifting trade-talk sentiment?
lastmod: '2022-04-30'
draft: false
featured: false
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



![](/project/deal/featured.GIF)

Reading news articles on the will-they-won't-they post-Brexit trade negotiations with the EU sees days of optimism jarred by days of gloom. Do negative news articles, when one wants a positive outcome, leave a deeper impression?

Is it possible to get a more objective view from [quantitative analysis of textual data](https://quanteda.io)? To do this, I'm going to look at hundreds of articles published in the Guardian newspaper over the course of the year to see how trade-talk sentiment changed week-to-week.


```r
library(tidyverse)
library(wesanderson)
library(kableExtra)
library(guardianapi)
library(quanteda)
library(scales)
library(tictoc)
library(clock)
library(patchwork)
library(text2vec)
library(topicmodels)
library(slider)
library(glue)
```


```r
theme_set(theme_bw())

(cols <- wes_palette(name = "Chevalier1"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />

The Withdrawal Agreement between the UK and the European Union was [signed on the 24th of January 2020](https://en.wikipedia.org/wiki/Brexit_withdrawal_agreement). Brexit-related newspaper articles will be imported from that date.

(Since publishing this article in September 2020, [an agreement was reached on December 24th 2020](https://www.bbc.com/news/uk-politics-55476625).)

The Guardian newspaper asks for requests to span no more than 1 month at a time. Creating a set of monthly date ranges will enable the requests to be chunked.


```r
dates_df <- tibble(start_date = date_build(2020, 1:11, 25)) |> 
  mutate(end_date = add_months(start_date, 1) |> add_days(-1))

dates_df |>
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> start_date </th>
   <th style="text-align:left;"> end_date </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2020-01-25 </td>
   <td style="text-align:left;"> 2020-02-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-02-25 </td>
   <td style="text-align:left;"> 2020-03-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-03-25 </td>
   <td style="text-align:left;"> 2020-04-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-04-25 </td>
   <td style="text-align:left;"> 2020-05-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-05-25 </td>
   <td style="text-align:left;"> 2020-06-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-06-25 </td>
   <td style="text-align:left;"> 2020-07-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-07-25 </td>
   <td style="text-align:left;"> 2020-08-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-08-25 </td>
   <td style="text-align:left;"> 2020-09-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-09-25 </td>
   <td style="text-align:left;"> 2020-10-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-10-25 </td>
   <td style="text-align:left;"> 2020-11-24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020-11-25 </td>
   <td style="text-align:left;"> 2020-12-24 </td>
  </tr>
</tbody>
</table>

Access to the Guardian's API requires a key which may be requested [here](https://open-platform.theguardian.com/access/).


```r
tic()

read_slowly <- slowly(gu_content)

article_df <-
  pmap_dfr(dates_df, function(start_date, end_date) {
    read_slowly(
      "brexit",
      from_date = start_date,
      to_date = end_date
    )
  })

toc()
```

The data need a little cleaning, for example, to remove multi-topic articles, html tags and non-breaking spaces.


```r
trade_df <-
  article_df |>
  filter(!str_detect(id, "/live/"), 
         section_id %in% c("world", "politics", "business")) |>
  mutate(
    body = str_remove_all(body, "<.*?>") |> str_to_lower(),
    body = str_remove_all(body, "[^a-z0-9 .-]"),
    body = str_remove_all(body, "nbsp")
  )
```

A corpus then gives me a collection of texts whereby each document is a newspaper article.


```r
trade_corp <- trade_df |> 
  corpus(docid_field = "short_url", text_field = "body", unique_docnames = FALSE)
```

Although only articles mentioning Brexit have been imported, some of these will not be related to trade negotiations with the EU. For example, there are on-going negotiations with many countries around the world. So, word embeddings will help to narrow the focus to the specific context of the UK-EU trade deal.

The chief negotiator for the EU is Michel Barnier, so I'll quantitatively identify words in close proximity to "Barnier" in the context of these Brexit news articles.


```r
window <- 5

trade_fcm <-
  trade_corp |>
  fcm(context = "window", window = window, 
      count = "weighted", weights = window:1)

glove <- GlobalVectors$new(rank = 60, x_max = 10)

set.seed(42)

wv_main <- glove$fit_transform(trade_fcm, n_iter = 10)
```

```
## INFO  [12:24:27.611] epoch 1, loss 0.3798 
## INFO  [12:24:30.456] epoch 2, loss 0.2566 
## INFO  [12:24:33.402] epoch 3, loss 0.2281 
## INFO  [12:24:36.322] epoch 4, loss 0.2082 
## INFO  [12:24:39.260] epoch 5, loss 0.1917 
## INFO  [12:24:42.150] epoch 6, loss 0.1790 
## INFO  [12:24:45.113] epoch 7, loss 0.1693 
## INFO  [12:24:48.049] epoch 8, loss 0.1617 
## INFO  [12:24:51.014] epoch 9, loss 0.1556 
## INFO  [12:24:53.941] epoch 10, loss 0.1505
```

```r
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)

search_coord <- 
  word_vectors["barnier", , drop = FALSE]

word_vectors |> 
  sim2(search_coord, method = "cosine") |> 
  as_tibble(rownames = NA) |> 
  rownames_to_column("term") |> 
  rename(similarity = 2) |> 
  slice_max(similarity, n = 10) |>
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> similarity </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> barnier </td>
   <td style="text-align:right;"> 1.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> michel </td>
   <td style="text-align:right;"> 0.8412831 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> frost </td>
   <td style="text-align:right;"> 0.8136653 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> negotiator </td>
   <td style="text-align:right;"> 0.8102825 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> brussels </td>
   <td style="text-align:right;"> 0.7100695 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> negotiators </td>
   <td style="text-align:right;"> 0.6613339 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> team </td>
   <td style="text-align:right;"> 0.6446460 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> chief </td>
   <td style="text-align:right;"> 0.6440991 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> eus </td>
   <td style="text-align:right;"> 0.6391720 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> told </td>
   <td style="text-align:right;"> 0.6103876 </td>
  </tr>
</tbody>
</table>

Word embedding is a learned modelling technique placing words into a multi-dimensional vector space such that contextually-similar words may be found close by. Not surprisingly, one of the closest words contextually is "Michel". And as he is the chief negotiator for the EU, we find "negotiator" and "brussels" also in the top most contextually-similar words.

The word embeddings algorithm, through word co-occurrence, has identified the name of Michel Barnier's UK counterpart David Frost. So filtering articles for "Barnier", "Frost" and "UK-EU" should help narrow the focus.


```r
context_df <- 
  trade_df |> 
  filter(str_detect(body, "barnier|frost|uk-eu")) 

context_corp <- 
  context_df |> 
  corpus(docid_field = "short_url", text_field = "body")
```

Quanteda's `kwic` function shows key phrases in context to ensure we're homing in on the required texts. Short URLs are included below so one can click on any to read the actual article as presented by The Guardian.


```r
set.seed(123)

context_corp |>
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) |>
  kwic(pattern = phrase(c("trade negotiation", "trade deal", "trade talks")), 
       valuetype = "regex", window = 7) |>
  as_tibble() |>
  left_join(article_df, by = c("docname" = "short_url")) |> 
  slice_sample(n = 10) |> 
  select(docname, pre, keyword, post, headline) |>
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> docname </th>
   <th style="text-align:left;"> pre </th>
   <th style="text-align:left;"> keyword </th>
   <th style="text-align:left;"> post </th>
   <th style="text-align:left;"> headline </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/ep2yb </td>
   <td style="text-align:left;"> put pressure on brussels to agree a </td>
   <td style="text-align:left;"> trade deal </td>
   <td style="text-align:left;"> and iron out problems with the withdrawal </td>
   <td style="text-align:left;"> Boris Johnson bows to Tory rebels with Brexit bill compromise </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/dag4n </td>
   <td style="text-align:left;"> the uk could not have the same </td>
   <td style="text-align:left;"> trade deal </td>
   <td style="text-align:left;"> with the eu as canada he said </td>
   <td style="text-align:left;"> Brexit deal 'a different ball game' to Canada agreement, warns EU </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/ekz7e </td>
   <td style="text-align:left;"> has gone down badly in brussels in </td>
   <td style="text-align:left;"> trade negotiations </td>
   <td style="text-align:left;"> usually both sides work out a consolidated </td>
   <td style="text-align:left;"> Barnier 'flabbergasted' at UK attempt to reopen Brexit specialty food debate </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/fptbj </td>
   <td style="text-align:left;"> for their companies this is the first </td>
   <td style="text-align:left;"> trade deal </td>
   <td style="text-align:left;"> in history that has been about erecting </td>
   <td style="text-align:left;"> Brexit talks followed common pattern but barrier-raising outcome is unique </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/dq896 </td>
   <td style="text-align:left;"> text contains a cut-and-paste from the eus </td>
   <td style="text-align:left;"> trade deal </td>
   <td style="text-align:left;"> with canada stating merely that it would </td>
   <td style="text-align:left;"> Brexit talks: Britain accuses EU of treating UK as 'unworthy' partner </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/fmmga </td>
   <td style="text-align:left;"> the conservative party for years john harris </td>
   <td style="text-align:left;"> trade deals </td>
   <td style="text-align:left;"> are not meant to assert sovereignty she </td>
   <td style="text-align:left;"> EU leaders stress unity as they welcome Brexit trade talks extension </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/fv2xh </td>
   <td style="text-align:left;"> demanded a last-minute compromise to reach a </td>
   <td style="text-align:left;"> trade deal </td>
   <td style="text-align:left;"> and avert chaos at the border as </td>
   <td style="text-align:left;"> Firms plead for Brexit deal as coronavirus leaves industry reeling </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/f6444 </td>
   <td style="text-align:left;"> canada-style trade deal the eu has a </td>
   <td style="text-align:left;"> trade deal </td>
   <td style="text-align:left;"> with canada called the comprehensive economic and </td>
   <td style="text-align:left;"> What did Boris Johnson mean by an Australia-style system of trade? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/fk5kt </td>
   <td style="text-align:left;"> companies await news of a potential uk-eu </td>
   <td style="text-align:left;"> trade deal </td>
   <td style="text-align:left;"> abf said our businesses have completed all </td>
   <td style="text-align:left;"> Primark reports 'phenomenal' trading since lockdowns ended </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/evgxe </td>
   <td style="text-align:left;"> in talks trying to thrash out a </td>
   <td style="text-align:left;"> trade deal </td>
   <td style="text-align:left;"> before january but after the chief negotiators </td>
   <td style="text-align:left;"> Wednesday briefing: Tory revolt over Cummings piles pressure on PM </td>
  </tr>
</tbody>
</table>

Quanteda provides a sentiment dictionary which, in addition to identifying positive and negative words, also finds negative-negatives and negative-positives such as, for example, "not effective". For each week's worth of articles, we can calculate the proportion of positive sentiments.


```r
tic()

sent_df <- 
  context_corp |> 
  dfm(dictionary = data_dictionary_LSD2015) |> 
  as_tibble() |>
  left_join(context_df, by = c("doc_id" = "short_url")) |> 
  mutate(
    pos = positive + neg_negative,
    neg = negative + neg_positive,
    date = date_ceiling(as.Date(web_publication_date), "week"),
    pct_pos = pos / (pos + neg)
  )

sent_df |> 
  select(doc_id, pos, neg) |> 
  slice(1:10) |> 
  kbl(col.names = c("Article", "Positive Score", "Negative Score"))
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Article </th>
   <th style="text-align:right;"> Positive Score </th>
   <th style="text-align:right;"> Negative Score </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/d6qhb </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/d9e9j </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/d6kzd </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/d9vjq </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/d6t3c </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/d79cn </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 51 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/d7n8b </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/d9xtf </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/dag4n </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> https://www.theguardian.com/p/d7d9t </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
</tbody>
</table>

```r
summary_df <- sent_df |> 
  group_by(date) |> 
  summarise(pct_pos = mean(pct_pos), n = n())

toc()
```

```
## 1.17 sec elapsed
```

Plotting the changing proportion of positive sentiment over time did surprise me a little. The outcome was more balanced than I expected which perhaps confirms the deeper impression left on me by negative articles.

The upper violin plot shows a rolling 7-day mean with a narrowing ribbon representing a narrowing variation in sentiment.

The lower plot shows the volume of articles. As we draw closer to the crunch-point the volume appears to be picking up.


```r
width <- 7

sent_df2 <- sent_df |>
  mutate(web_date = as.Date(web_publication_date)) |> 
  group_by(web_date) |>
  summarise(pct_pos = sum(pos) / sum(neg + pos)) |> 
  mutate(
    roll_mean = slide_dbl(pct_pos, mean, .before = 6),
    roll_lq = slide_dbl(pct_pos, ~ quantile(.x, probs = 0.25), .before = 6),
    roll_uq = slide_dbl(pct_pos, ~ quantile(.x, probs = 0.75), .before = 6)
  )

p1 <- sent_df2 |>
  ggplot(aes(web_date)) +
  geom_line(aes(y = roll_mean), colour = cols[1]) +
  geom_ribbon(aes(ymin = roll_lq, ymax = roll_uq), 
              alpha = 0.33, fill = cols[1]) +
  geom_hline(yintercept = 0.5, linetype = "dashed", 
             colour = cols[4], size = 1) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "Changing Sentiment Towards a UK-EU Trade Deal",
    subtitle = glue("Rolling {width} days Since the Withdrawal Agreement"),
    x = NULL, y = "Positive Sentiment"
  )

p2 <- summary_df |> 
  ggplot(aes(date, n)) +
  geom_line(colour = cols[1]) +
  labs(x = "Weeks", y = "Article Count",
       caption = "Source: Guardian Newspaper")

p1 / p2 + 
  plot_layout(heights = c(2, 1))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="100%" />

Some writers exhibit more sentiment variation than others.


```r
byline_df <- 
  sent_df |> 
  mutate(byline = word(byline, 1, 2) |> str_remove_all("[[:punct:]]")) |> 
  group_by(byline, date) |> 
  summarise(pct_pos = mean(pct_pos), n = n()) |> 
  ungroup()

top_3 <- byline_df |> 
  count(byline, sort = TRUE) |> 
  slice_head(n = 3) |> 
  pull(byline)

byline_df |> 
  filter(byline %in% top_3) |> 
  ggplot(aes(date, pct_pos, colour = byline)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = cols[2]) +
  scale_y_continuous(labels = label_percent(), limits = c(0.2, 0.8)) +
  scale_colour_manual(values = cols[c(1, 2, 4)]) +
  labs(title = "Changing Sentiment Towards a UK-EU Trade Deal",
       subtitle = "Three Selected Bylines",
       x = "Weeks", y = "Positive Sentiment", colour = "Byline", 
       caption = "Source: The Guardian")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="100%" />

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
   <td style="text-align:left;"> c[7];  sum[3];  as.Date[2];  function[2];  mean[2];  set.seed[2];  conflicts[1];  cumsum[1];  search[1];  t[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> add_days[1];  add_months[1];  date_build[1];  date_ceiling[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> mutate[10];  filter[8];  group_by[4];  summarise[4];  if_else[3];  n[3];  left_join[2];  select[2];  arrange[1];  count[1];  desc[1];  pull[1];  rename[1];  slice[1];  slice_head[1];  slice_max[1];  slice_sample[1];  ungroup[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[5];  geom_line[3];  ggplot[3];  labs[3];  geom_hline[2];  scale_y_continuous[2];  geom_ribbon[1];  scale_colour_manual[1];  theme_bw[1];  theme_set[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> glue </td>
   <td style="text-align:left;"> glue[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> guardianapi </td>
   <td style="text-align:left;"> gu_content[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> patchwork </td>
   <td style="text-align:left;"> plot_layout[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[1];  map2_dfr[1];  pmap_dfr[1];  possibly[1];  set_names[1];  slowly[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> quanteda </td>
   <td style="text-align:left;"> corpus[2];  data_dictionary_LSD2015[1];  dfm[1];  fcm[1];  kwic[1];  phrase[1];  tokens[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scales </td>
   <td style="text-align:left;"> label_percent[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> slider </td>
   <td style="text-align:left;"> slide_dbl[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stats </td>
   <td style="text-align:left;"> quantile[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[5];  str_remove_all[5];  str_detect[4];  str_remove[2];  str_count[1];  str_starts[1];  str_to_lower[1];  word[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> text2vec </td>
   <td style="text-align:left;"> sim2[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[4];  tibble[3];  enframe[1];  rownames_to_column[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tictoc </td>
   <td style="text-align:left;"> tic[2];  toc[2] </td>
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
