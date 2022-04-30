---
title: Sea Monsters that Lost their Way
author: Carl Goodwin
date: '2021-12-04'
slug: cetacea
categories:
  - R
tags:
  - geospatial
  - machine learning
  - text mining
summary: The Natural History Museum began [recording cetacean]( https://data.nhm.ac.uk/dataset/historical-uk-cetacean-strandings-dataset) strandings in 1913. For some records the species is uncertain. Could these be predicted using [tidymodels](https://www.tidymodels.org) and [textrecipes](https://textrecipes.tidymodels.org)?
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



![](/project/cetacea/featured.GIF)

The Natural History Museum began recording cetacean (whales, dolphins and porpoises) strandings in 1913. I'd like to explore this [1913-1989 dataset]( https://data.nhm.ac.uk/dataset/historical-uk-cetacean-strandings-dataset). 


```r
library(tidyverse)
library(tidymodels)
library(probably)
library(finetune)
library(textrecipes)
library(stopwords)
library(wesanderson)
library(kableExtra)
library(clock)
library(glue)
library(janitor)
library(vip)
library(tictoc)
library(doParallel)

registerDoParallel(cores = 6)
```


```r
theme_set(theme_bw())

(cols <- wes_palette(name = "Darjeeling2"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="100%" />


```r
strandings_df <- read_csv("strandings.csv") |>
  clean_names() |> 
  mutate(
    date = date_parse(date, format = "%d/%m/%Y"),
    length = parse_number(length_et),
    species_lumped = fct_lump_n(species, 20),
    across(ends_with("_val"), as.integer)
  )

# glimpse(strandings_df)
```

## Exploratory

Some of the `species` labels contain a question mark or forward slash. This indicates uncertainty, so it might be fun to see if a machine learning model (multiclass classification) could learn from the known species and suggest an appropriate `species` where it's uncertain.


```r
strandings_df2 <- 
  strandings_df |> 
  mutate(species_uncertainty =
      if_else(str_detect(species, "[?/]"), "Uncertain", "Known"))

strandings_df2 |> 
  filter(species_uncertainty == "Uncertain") |> 
  count(species, sort = TRUE, name = "Count") |> 
  slice_head(n = 6) |> 
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> species </th>
   <th style="text-align:right;"> Count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> delphis/coeruleoalba </td>
   <td style="text-align:right;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> phocoena? </td>
   <td style="text-align:right;"> 42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> melaena? </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> delphis? </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> truncatus? </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> acutorostrata? </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
</tbody>
</table>


The `date` variable has many NA's. Fortunately, the components to construct many of these are in the `year_val`, `month_val` and `day_val` variables. With a little wrangling and imputation, we can coalesce these variables into a new date.  This will be useful since plots of sample `species` by `year`, `month` and `week` of stranding suggest a de-constructed `date` could be a useful predictor.


```r
strandings_df2 |> 
  select(date, year_val, month_val, day_val) |> 
  summary()
```

```
##       date               year_val      month_val         day_val     
##  Min.   :1913-02-13   Min.   :   0   Min.   : 0.000   Min.   : 0.00  
##  1st Qu.:1933-09-09   1st Qu.:1933   1st Qu.: 4.000   1st Qu.: 9.00  
##  Median :1954-04-13   Median :1955   Median : 7.000   Median :16.00  
##  Mean   :1955-01-08   Mean   :1954   Mean   : 6.766   Mean   :15.66  
##  3rd Qu.:1979-03-21   3rd Qu.:1979   3rd Qu.:10.000   3rd Qu.:22.00  
##  Max.   :1989-12-25   Max.   :1989   Max.   :12.000   Max.   :31.00  
##  NA's   :121
```

```r
strandings_df3 <- strandings_df2 |>
  group_by(species) |> 
  mutate(
    month_val = if_else(month_val == 0, mean(month_val) |> 
                          as.integer(), month_val),
    day_val = if_else(day_val == 0, mean(day_val) |> as.integer(), day_val),
    day_val = if_else(day_val == 0, 1L, day_val),
    date2 = date_build(year_val, month_val, day_val, invalid = "NA"),
  ) |> 
  ungroup() |> 
  mutate(date3 = coalesce(date, date2)) |> 
  arrange(id) |> 
  mutate(date = if_else(is.na(date), lag(date3), date3)) |> 
  select(-date2, -date3, -ends_with("_val"))

example_species <-
  c("musculus", "melas", "crassidens", "borealis", "coeruleoalba")

known_species <- strandings_df3 |> 
  filter(species_uncertainty == "Known")

plot_date_feature <- function(var) {
  known_species |>
    mutate(
      year = get_year(date),
      month = get_month(date),
      week = as_iso_year_week_day(date) |> get_week()
    ) |>
    filter(species %in% example_species) |>
    count(species, {{ var }}) |>
    ggplot(aes(species, {{ var }})) +
    geom_violin(
      alpha = 0.7,
      fill = cols[3],
      show.legend = FALSE
    ) +
    coord_flip() +
    labs(
      title = glue("Variation in {str_to_title(as.character(var))}",
                   " of Stranding for Known Species"),
      x = NULL, y = glue("{str_to_title(as.character(var))}")
    )
}

c("year", "month", "week") |> 
  map(sym) |> 
  map(plot_date_feature)
```

```
## [[1]]
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="100%" />

```
## 
## [[2]]
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-2.png" width="100%" />

```
## 
## [[3]]
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-3.png" width="100%" />

Do `latitude` and `longitude` carry useful predictive information?

A geospatial visualisation of strandings shows some `species` do gravitate towards particular stretches of coastline, e.g. "acutus" and "albirostris" to the east, and "coeruleoalba" to the south-west.

Some `species` may also be more prone to mass stranding, so something that indicates whether a `species` has such a history (in these data) may be worth including in the mix.


```r
uki <- map_data("world", region = c("uk", "ireland"))

labels <- c("Mass", "Single")

uki |> 
  ggplot() +
  geom_map(aes(long, lat, map_id = region), map = uki, 
           colour = "black", fill = "grey90", size = 0.1) +
  geom_jitter(aes(longitude, latitude, colour = mass_single, 
                  size = mass_single), 
              alpha = 0.5, data = known_species) +
  facet_wrap(~ species_lumped, nrow = 3) +
  coord_map("mollweide") +
  scale_size_manual(values = c(1, 0.5), labels = labels) +
  scale_colour_manual(values = cols[c(3, 2)], labels = labels) +
  theme_void() +
  theme(legend.position = "top", 
        strip.text = element_text(colour = "grey50")) +
  labs(title = "Strandings by Species", 
       colour = NULL, size = NULL)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="100%" />

```r
# Add history of mass stranding
strandings_df4 <- strandings_df3 |> 
  group_by(species) |>
  mutate(mass_possible = min(mass_single, na.rm = TRUE)) |>
  ungroup()
```

Some records are missing the `length` measurement of the mammal. Nonetheless, where present, this is likely to be predictive, and may help, for example, separate species labelled as "delphis/coeruleoalba" where the `length` is at the extreme ends of the "delphis" range as we see below. And the range of `length` may differ by mammal `sex`.


```r
known_species |>
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    TRUE       ~ "Unknown"
  )) |> 
  filter(species_lumped != "Other") |> 
  count(species_lumped, length, sex) |> 
  mutate(species_lumped = fct_reorder(species_lumped, 
                                      desc(length), min, na.rm = TRUE)) |> 
  ggplot(aes(species_lumped, length)) + 
  geom_violin(aes(fill = if_else(str_detect(species_lumped, "^de|^co"), 
                                 TRUE, FALSE)), show.legend = FALSE) +
  facet_wrap(~ sex) +
  scale_fill_manual(values = cols[c(1, 5)]) +
  coord_flip() +
  labs(title = "Variation in Species Length by Sex", 
       x = NULL, y = "Length (metres)")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="100%" />

With map coordinates not always available, `county` could be, with a little string cleaning, a useful additional predictor.


```r
strandings_df4 |> 
  count(county) |> 
  filter(str_detect(county, "Shet|Northumberland")) |> 
  kbl(col.names = c("County", "Count"))
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> County </th>
   <th style="text-align:right;"> Count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Fair Isle, Shetland Isles </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Northumberland </td>
   <td style="text-align:right;"> 89 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Northumberland. </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Shetland Islands, Scotland </td>
   <td style="text-align:right;"> 232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Shetland Isles, Scotland </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Shetland, Scotland </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Shetlands, Scotland </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

```r
regex_pattern <-
  c("[,/].*$",
    "(?<!Che|Hamp|Lanca|North York)-?shire",
    " Isl.*$",
    " &.*$",
    "[-.]$")

strandings_df5 <- strandings_df4 |>
  mutate(
    county = str_remove_all(county, str_c(regex_pattern, collapse = "|")),
    county = recode(
      county,
      "Carnarvon" = "Caernarvon",
      "E.Lothian" = "East Lothian",
      "Shetlands" = "Shetland",
      "W.Glamorgan" = "West Glamorgan",
      "S.Glamorgan" = "South Glamorgan"
    )
  ) 

strandings_df4 |>
  summarise(counties_before = n_distinct(county))
```

```
## # A tibble: 1 × 1
##   counties_before
##             <int>
## 1             146
```

```r
strandings_df5 |>
  summarise(counties_after = n_distinct(county))
```

```
## # A tibble: 1 × 1
##   counties_after
##            <int>
## 1            109
```

Whilst `count` also appears to hold, based on the plot pattern below, species-related information, I'm not going to use it as a predictor as we do not know enough about how it was derived, as reflected in these [variable descriptions](https://data.nhm.ac.uk/dataset/historical-uk-cetacean-strandings-dataset/resource/9a306dcd-1667-48b5-b682-ce6f071d85ce).


```r
strandings_df5 |>
  ggplot(aes(species, count, colour = species_uncertainty)) +
  geom_jitter(alpha = 0.5, size = 2) +
  coord_flip() +
  scale_y_log10() +
  scale_colour_manual(values = cols[c(1, 5)]) +
  labs(title = "How 'Count' Relates to Species", 
       x = NULL, y = "Count (log10)", colour = "Species") +
  theme(legend.position = "top")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="100%" />

## Modelling

So, I'll set aside the rows where the species is uncertain (to be used later for new predictions), and I'll train a model on 75% of known species, and test it on the remaining 25%.  I'll use the following predictors:

* `latitude` and `longitude`
* Mammal `length` and `sex`
* `mass_possible` indicating a `species` history of mass strandings
* `date` reported converted into decimal, week, month and year
* `county` may be useful, especially where the longitude and latitude are missing
* `fam_genus` which narrows the range of likely species

I'd like to also make use of the textrecipes package. I can tokenise the textual information in `comment` and `location` to see if these add to the predictive power of the model.

I'll tune the model using `tune_race_anova` which quickly discards hyperparameter combinations showing little early promise.


```r
known_species <- strandings_df5 |>
  filter(species_uncertainty == "Known") |>
  mutate(across(
    c(
      "species",
      "mass_single",
      "mass_possible",
      "county",
      "location",
      "sex",
      "fam_genus"
    ),
    factor
  ))

set.seed(123)

data_split <-
  known_species |>
  mutate(species = fct_drop(species)) |> 
  initial_split(strata = species)

train <- data_split |> training()

test <- data_split |> testing()

predictors <-
  c(
    "latitude",
    "longitude",
    "length",
    "mass_single",
    "mass_possible",
    "county",
    "location",
    "comment",
    "sex",
    "fam_genus"
  )

recipe <-
  train |>
  recipe() |>
  update_role(species, new_role = "outcome") |>
  update_role(all_of(predictors), new_role = "predictor") |>
  update_role(!has_role("outcome") & !has_role("predictor"), 
              new_role = "id") |>
  step_date(date, features = c("decimal", "week", "month", "year"), 
            label = FALSE) |>
  step_tokenize(location, comment) |>
  step_stopwords(location, comment) |>
  step_tokenfilter(location, comment, max_tokens = tune()) |> #100
  step_tf(location, comment) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors())

xgb_model <-
  boost_tree(trees = tune(), # 440
             mtry = tune(), # 0.6
             learn_rate = 0.02) |> 
  set_mode("classification") |>
  set_engine("xgboost", 
             count = FALSE,
             verbosity = 0,
             tree_method = "hist")

xgb_wflow <- workflow() |>
  add_recipe(recipe) |>
  add_model(xgb_model)

set.seed(9)

folds <- vfold_cv(train, strata = species)

set.seed(10)

tic()

tune_result <- xgb_wflow |>
  tune_race_anova(
    resamples = folds,
    grid = crossing(
      trees = seq(200, 520, 40),
      mtry = seq(0.5, 0.7, 0.1),
      max_tokens = seq(80, 120, 20)
      ),
    control = control_race(),
    metrics = metric_set(accuracy)
  )

toc()
```

```
## 476.418 sec elapsed
```

```r
tune_result |> 
  plot_race() + 
  labs(title = "Early Elimination of Parameter Combinations")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="100%" />

```r
set.seed(123)

xgb_fit <- xgb_wflow |>
  finalize_workflow(tune_result |> 
                      select_best(metric = "accuracy")) |> 
  fit(train)
```

Having fitted the model with the 3080 records in the training data, I'll test its accuracy on the 1028 records of *known* species the model has not yet seen.

Without spending time on alternative models, we're getting a reasonable result for the "porpoise" of this post, as reflected in both the accuracy metric and confusion matrix.


```r
xgb_results <- xgb_fit |> 
  augment(new_data = test)

xgb_results |>
  accuracy(species, .pred_class)
```

```
## # A tibble: 1 × 3
##   .metric  .estimator .estimate
##   <chr>    <chr>          <dbl>
## 1 accuracy multiclass     0.993
```

```r
xgb_results |>
  conf_mat(species, .pred_class) |>
  autoplot(type = "heatmap") +
  scale_fill_gradient2(
    mid = "white",
    high = cols[1],
    midpoint = 0
  ) +
  labs(title = "Confusion Matrix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="100%" />

The top variable importance scores include `fam_genus`, many of the `comment` tokens, plus `length`, `mass-possible`, `date_decimal`, `date_year`, and `latitude`.


```r
vi(xgb_fit |> extract_fit_parsnip()) |> 
  arrange(desc(Importance)) |> 
  mutate(ranking = row_number()) |> 
  slice_head(n = 40) |> 
  kbl()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:right;"> Importance </th>
   <th style="text-align:right;"> ranking </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> fam_genus_Phocoena </td>
   <td style="text-align:right;"> 0.1215845 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Globicephala </td>
   <td style="text-align:right;"> 0.0816541 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_unidentified </td>
   <td style="text-align:right;"> 0.0736307 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Delphinus </td>
   <td style="text-align:right;"> 0.0710860 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Tursiops </td>
   <td style="text-align:right;"> 0.0500141 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_false </td>
   <td style="text-align:right;"> 0.0498405 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Lagenorhynchus </td>
   <td style="text-align:right;"> 0.0448997 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_finned </td>
   <td style="text-align:right;"> 0.0341306 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_sided </td>
   <td style="text-align:right;"> 0.0302409 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_long </td>
   <td style="text-align:right;"> 0.0244866 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Hyperoodon </td>
   <td style="text-align:right;"> 0.0240353 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> length </td>
   <td style="text-align:right;"> 0.0230962 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Grampus </td>
   <td style="text-align:right;"> 0.0227513 </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_beaked </td>
   <td style="text-align:right;"> 0.0219021 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_lesser </td>
   <td style="text-align:right;"> 0.0215853 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_rorqual </td>
   <td style="text-align:right;"> 0.0199182 </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_dolphin </td>
   <td style="text-align:right;"> 0.0198597 </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Orcinus </td>
   <td style="text-align:right;"> 0.0194480 </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_porpoise </td>
   <td style="text-align:right;"> 0.0192418 </td>
   <td style="text-align:right;"> 19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_bottle </td>
   <td style="text-align:right;"> 0.0165892 </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Pseudorca </td>
   <td style="text-align:right;"> 0.0159957 </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Physeter </td>
   <td style="text-align:right;"> 0.0159090 </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_risso's </td>
   <td style="text-align:right;"> 0.0131135 </td>
   <td style="text-align:right;"> 23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mass_possible_S </td>
   <td style="text-align:right;"> 0.0127573 </td>
   <td style="text-align:right;"> 24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Mesoplodon </td>
   <td style="text-align:right;"> 0.0123743 </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_fin </td>
   <td style="text-align:right;"> 0.0122737 </td>
   <td style="text-align:right;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Ziphius </td>
   <td style="text-align:right;"> 0.0122521 </td>
   <td style="text-align:right;"> 27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_odontocete </td>
   <td style="text-align:right;"> 0.0109274 </td>
   <td style="text-align:right;"> 28 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_cetacean </td>
   <td style="text-align:right;"> 0.0104412 </td>
   <td style="text-align:right;"> 29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_killer </td>
   <td style="text-align:right;"> 0.0099741 </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fam_genus_Stenella </td>
   <td style="text-align:right;"> 0.0087018 </td>
   <td style="text-align:right;"> 31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> date_decimal </td>
   <td style="text-align:right;"> 0.0081614 </td>
   <td style="text-align:right;"> 32 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_nosed </td>
   <td style="text-align:right;"> 0.0075943 </td>
   <td style="text-align:right;"> 33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> date_year </td>
   <td style="text-align:right;"> 0.0072361 </td>
   <td style="text-align:right;"> 34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_whale </td>
   <td style="text-align:right;"> 0.0071684 </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_white </td>
   <td style="text-align:right;"> 0.0067214 </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mass_single_S </td>
   <td style="text-align:right;"> 0.0041988 </td>
   <td style="text-align:right;"> 37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_common </td>
   <td style="text-align:right;"> 0.0041767 </td>
   <td style="text-align:right;"> 38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_cuvier's </td>
   <td style="text-align:right;"> 0.0036603 </td>
   <td style="text-align:right;"> 39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tf_comment_sowerby's </td>
   <td style="text-align:right;"> 0.0036372 </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
</tbody>
</table>

Do the predictions look reasonable? 

The class probability is spread across 27 species. I'm going to set a high threshold of 0.9, meaning the predicted species needs to be a pretty confident prediction.  


```r
xgb_preds <- xgb_fit |> 
  augment(new_data = strandings_df5 |> 
            filter(species_uncertainty == "Uncertain"))

species_levels <- xgb_preds |> 
  select(starts_with(".pred_"), -.pred_class) |> 
  names() |> 
  as.factor()

subset_df <- xgb_preds |>
  mutate(
    .class_pred = make_class_pred(
      .pred_acutorostrata,
      .pred_acutus,
      .pred_albirostris,
      .pred_ampullatus,
      .pred_bidens,
      .pred_borealis,
      .pred_breviceps,
      .pred_cavirostris,
      .pred_coeruleoalba,
      .pred_crassidens,
      .pred_delphis,
      .pred_electra,
      .pred_europaeus,
      .pred_griseus,
      .pred_leucas,
      .pred_macrocephalus,
      .pred_melaena,
      .pred_melas,
      .pred_mirus,
      .pred_monoceros,
      .pred_musculus,
      .pred_novaeangliae,
      .pred_orca,
      .pred_phocoena,
      .pred_physalus,
      .pred_sp.indet.,
      .pred_truncatus,
      levels = species_levels,
      min_prob = .9
    )
  )

subset_df |>
  group_by(species, .class_pred) |> 
  summarise(n = n()) |> 
  arrange(species, desc(n)) |> 
  kbl(col.names = c("Actual Label", "Predicted Label", "Count"))
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Actual Label </th>
   <th style="text-align:right;"> Predicted Label </th>
   <th style="text-align:right;"> Count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> acutorostrata? </td>
   <td style="text-align:right;"> .pred_acutorostrata </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> acutorostrata/borealis </td>
   <td style="text-align:right;"> .pred_acutorostrata </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> acutus? </td>
   <td style="text-align:right;"> .pred_acutus </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> albirostris? </td>
   <td style="text-align:right;"> .pred_albirostris </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ampullatus? </td>
   <td style="text-align:right;"> .pred_ampullatus </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bidens? </td>
   <td style="text-align:right;"> .pred_bidens </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bidens? </td>
   <td style="text-align:right;"> [EQ] </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cavirostris? </td>
   <td style="text-align:right;"> .pred_cavirostris </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> coeruleoalba? </td>
   <td style="text-align:right;"> .pred_coeruleoalba </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> delphis? </td>
   <td style="text-align:right;"> .pred_delphis </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> delphis/coeruleoalba </td>
   <td style="text-align:right;"> [EQ] </td>
   <td style="text-align:right;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> griseus? </td>
   <td style="text-align:right;"> .pred_griseus </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> macrocephalus? </td>
   <td style="text-align:right;"> .pred_macrocephalus </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> melaena? </td>
   <td style="text-align:right;"> .pred_melaena </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> orca? </td>
   <td style="text-align:right;"> .pred_orca </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> phocoena? </td>
   <td style="text-align:right;"> .pred_phocoena </td>
   <td style="text-align:right;"> 42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> physalus? </td>
   <td style="text-align:right;"> .pred_physalus </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> physalus/acutorostrata </td>
   <td style="text-align:right;"> [EQ] </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> truncatus? </td>
   <td style="text-align:right;"> .pred_truncatus </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> truncatus/albirostris </td>
   <td style="text-align:right;"> [EQ] </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
</tbody>
</table>

The majority of the 203 uncertain records are predicted to be as suspected in the original labelling. The remainder are classed as equivocal as they have not met the high bar of a 0.9-or-above probability for a single species.

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
   <td style="text-align:left;"> as.character[2];  as.integer[3];  c[11];  comment[3];  conflicts[1];  cumsum[1];  function[2];  is.na[1];  length[2];  log10[1];  mean[2];  min[1];  names[1];  search[1];  seq[3];  set.seed[4];  sum[1];  summary[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clock </td>
   <td style="text-align:left;"> as_iso_year_week_day[1];  date_build[1];  date_parse[1];  get_month[1];  get_week[1];  get_year[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> doParallel </td>
   <td style="text-align:left;"> registerDoParallel[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> filter[12];  across[2];  arrange[5];  case_when[1];  coalesce[1];  count[4];  desc[5];  group_by[4];  id[1];  if_else[9];  mutate[18];  n[2];  n_distinct[2];  recode[1];  row_number[1];  select[3];  slice_head[2];  summarise[4];  ungroup[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> finetune </td>
   <td style="text-align:left;"> control_race[1];  plot_race[1];  tune_race_anova[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forcats </td>
   <td style="text-align:left;"> fct_drop[1];  fct_lump_n[1];  fct_reorder[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> aes[6];  coord_flip[3];  coord_map[1];  element_text[2];  facet_wrap[2];  geom_jitter[2];  geom_map[1];  geom_violin[2];  ggplot[4];  labs[6];  map_data[1];  scale_colour_manual[2];  scale_fill_gradient2[1];  scale_fill_manual[1];  scale_size_manual[1];  scale_y_log10[1];  theme[3];  theme_bw[1];  theme_set[1];  theme_void[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> glue </td>
   <td style="text-align:left;"> glue[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> janitor </td>
   <td style="text-align:left;"> clean_names[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> kableExtra </td>
   <td style="text-align:left;"> kbl[5] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> parsnip </td>
   <td style="text-align:left;"> boost_tree[1];  set_engine[1];  set_mode[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> probably </td>
   <td style="text-align:left;"> make_class_pred[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> map[3];  map2_dfr[1];  possibly[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readr </td>
   <td style="text-align:left;"> parse_number[1];  read_csv[1];  read_lines[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> recipes </td>
   <td style="text-align:left;"> all_nominal_predictors[1];  all_predictors[1];  has_role[2];  step_date[1];  step_dummy[1];  step_zv[1];  update_role[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rsample </td>
   <td style="text-align:left;"> initial_split[1];  testing[1];  training[1];  vfold_cv[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stats </td>
   <td style="text-align:left;"> var[3] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> stringr </td>
   <td style="text-align:left;"> str_c[6];  str_count[1];  str_detect[5];  str_remove[2];  str_remove_all[2];  str_starts[1];  str_to_title[2] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> textrecipes </td>
   <td style="text-align:left;"> step_stopwords[1];  step_tf[1];  step_tokenfilter[1];  step_tokenize[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tibble </td>
   <td style="text-align:left;"> as_tibble[1];  tibble[2];  enframe[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tictoc </td>
   <td style="text-align:left;"> tic[1];  toc[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyr </td>
   <td style="text-align:left;"> crossing[1];  unnest[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tune </td>
   <td style="text-align:left;"> finalize_workflow[1];  select_best[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wesanderson </td>
   <td style="text-align:left;"> wes_palette[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> workflows </td>
   <td style="text-align:left;"> add_model[1];  add_recipe[1];  workflow[1] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> yardstick </td>
   <td style="text-align:left;"> accuracy[2];  conf_mat[1];  metric_set[1] </td>
  </tr>
</tbody>
</table>

## Citation

Natural History Museum (2019). Data Portal Query on "UK cetacean strandings 1913-1989" created at 2019-08-10 16:41:12.475340 PID https://doi.org/10.5519/qd.iwg63595. Subset of "Historical UK cetacean strandings dataset (1913-1989)" (dataset) PID https://doi.org/10.5519/0028204.
