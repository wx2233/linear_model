bootstrap
================
Weijia Xiong
11/14/2019

``` r
n_samp = 250

sim_df_const = 
  tibble(
    x = rnorm(n_samp, 1, 1),
    error = rnorm(n_samp, 0, 1),
    y = 2 + 3 * x + error
  )

sim_df_nonconst = sim_df_const %>% 
  mutate(
  error = error * .75 * x,
  y = 2 + 3 * x + error
)
```

``` r
sim_df = 
  bind_rows(const = sim_df_const, nonconst = sim_df_nonconst, .id = "data_source") 

sim_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = .5) +
  stat_smooth(method = "lm") +
  facet_grid(~data_source) 
```

<img src="bootstrap_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
sim_df_const %>% 
  lm(y ~ x, data = . ) %>% 
  broom::tidy() 
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.98    0.0981      20.2 3.65e- 54
    ## 2 x               3.04    0.0699      43.5 3.84e-118

``` r
sim_df_nonconst %>% 
  lm(y ~ x, data = . ) %>% 
  broom::tidy() 
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.93    0.105       18.5 1.88e- 48
    ## 2 x               3.11    0.0747      41.7 5.76e-114

## how can i bootstrap

write a function to draw a bootsrap sample based on a dataframe

``` r
sim_df_nonconst %>% 
  sample_frac(size = 1 ,replace = TRUE) %>%  ## take the sample with replacement(some x appear more than once), size: proportion, 1: 100%
  arrange(x)
```

    ## # A tibble: 250 x 3
    ##         x  error       y
    ##     <dbl>  <dbl>   <dbl>
    ##  1 -1.89   1.62  -2.04  
    ##  2 -1.29   1.40  -0.454 
    ##  3 -0.989 -1.97  -2.93  
    ##  4 -0.989 -1.97  -2.93  
    ##  5 -0.914 -0.908 -1.65  
    ##  6 -0.914 -0.908 -1.65  
    ##  7 -0.914 -0.908 -1.65  
    ##  8 -0.733  0.447  0.248 
    ##  9 -0.733  0.447  0.248 
    ## 10 -0.606 -0.106  0.0774
    ## # … with 240 more rows

``` r
boot_sample = function(df) {
  sample_frac(df, size = 1 ,replace = TRUE)
}
```

``` r
boot_sample(sim_df_nonconst) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = .5) +
  stat_smooth(method = "lm")
```

<img src="bootstrap_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />
some original data points appear more than once, others don’t appear at
all.

Organize a dataframe rerun bootsample

``` r
boot_straps = 
  tibble(
    strap_num = 1:1000,
    strap_sample = rerun(1000,boot_sample(df = sim_df_nonconst))
  )
```

``` r
boot_straps %>% 
  filter(strap_num %in% 1:2) %>% 
  mutate(strap_sample = map(strap_sample, ~arrange(.x, x))) %>% 
  pull(strap_sample)
```

    ## [[1]]
    ## # A tibble: 250 x 3
    ##         x  error       y
    ##     <dbl>  <dbl>   <dbl>
    ##  1 -1.29   1.40  -0.454 
    ##  2 -0.989 -1.97  -2.93  
    ##  3 -0.914 -0.908 -1.65  
    ##  4 -0.914 -0.908 -1.65  
    ##  5 -0.805  0.292 -0.123 
    ##  6 -0.805  0.292 -0.123 
    ##  7 -0.665 -0.544 -0.539 
    ##  8 -0.641 -0.416 -0.338 
    ##  9 -0.606 -0.106  0.0774
    ## 10 -0.606 -0.106  0.0774
    ## # … with 240 more rows
    ## 
    ## [[2]]
    ## # A tibble: 250 x 3
    ##         x  error      y
    ##     <dbl>  <dbl>  <dbl>
    ##  1 -1.89   1.62  -2.04 
    ##  2 -1.89   1.62  -2.04 
    ##  3 -1.29   1.40  -0.454
    ##  4 -1.29   1.40  -0.454
    ##  5 -1.00   0.832 -0.169
    ##  6 -0.914 -0.908 -1.65 
    ##  7 -0.805  0.292 -0.123
    ##  8 -0.665 -0.544 -0.539
    ##  9 -0.665 -0.544 -0.539
    ## 10 -0.665 -0.544 -0.539
    ## # … with 240 more rows

``` r
boot_straps %>% 
  filter(strap_num %in% 1:3) %>% 
  unnest(strap_sample) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = .5) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_grid(~strap_num) 
```

<img src="bootstrap_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

Do some kind of analysis…

``` r
bootstrap_results =
  boot_straps %>% 
  mutate(
    models = map(strap_sample,~ lm(y ~ x, data = .x)),
    results = map(models,broom::tidy)
  ) %>% 
  select(-strap_sample,-models) %>% 
  unnest(results)
```

summarize these results

``` r
bootstrap_results %>% 
  group_by(term) %>% 
  summarise(
    se = sd(estimate)
  )
```

    ## # A tibble: 2 x 2
    ##   term            se
    ##   <chr>        <dbl>
    ## 1 (Intercept) 0.0748
    ## 2 x           0.101

se is too low\!

use package

``` r
sim_df_nonconst %>% 
  modelr::bootstrap(n = 1000) %>% 
  mutate(
    models = map(strap,~ lm(y ~ x, data = .x)),
    results = map(models,broom::tidy)
  ) %>% 
  select(-strap,-models) %>% 
  unnest(results)
```

    ## # A tibble: 2,000 x 6
    ##    .id   term        estimate std.error statistic   p.value
    ##    <chr> <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 0001  (Intercept)     1.90    0.101       18.8 1.05e- 49
    ##  2 0001  x               3.11    0.0774      40.2 1.27e-110
    ##  3 0002  (Intercept)     1.87    0.0944      19.8 5.17e- 53
    ##  4 0002  x               3.24    0.0667      48.6 1.13e-128
    ##  5 0003  (Intercept)     2.01    0.115       17.5 2.87e- 45
    ##  6 0003  x               2.96    0.0776      38.1 1.38e-105
    ##  7 0004  (Intercept)     1.91    0.114       16.7 1.31e- 42
    ##  8 0004  x               3.12    0.0818      38.1 1.03e-105
    ##  9 0005  (Intercept)     1.92    0.109       17.7 8.68e- 46
    ## 10 0005  x               3.05    0.0748      40.8 4.45e-112
    ## # … with 1,990 more rows

``` r
boot_straps$strap[[1]]
```

    ## NULL

``` r
## <resample [250 x 3]> 8, 132, 69, 225, 180, 122, 34, 170, 216, 122, ...
as_data_frame(boot_straps$strap[[1]])
```

    ## # A tibble: 0 x 0

``` r
sim_df_nonconst %>% 
  lm(y ~ x, data = . ) %>% 
  broom::tidy() 
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.93    0.105       18.5 1.88e- 48
    ## 2 x               3.11    0.0747      41.7 5.76e-114

``` r
sim_df_nonconst %>% 
  modelr::bootstrap(n = 1000) %>% 
  mutate(
    models = map(strap,~lm(y ~ x, data = .x)),
    results = map(models,broom::tidy)
  ) %>% 
  select(-strap,-models) %>% 
  unnest(results) %>% 
  group_by(term) %>% 
  summarise(boot_se = sd(estimate))
```

    ## # A tibble: 2 x 2
    ##   term        boot_se
    ##   <chr>         <dbl>
    ## 1 (Intercept)  0.0793
    ## 2 x            0.104

check for const

``` r
sim_df_const %>% 
  lm(y ~ x, data = . ) %>% 
  broom::tidy() 
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.98    0.0981      20.2 3.65e- 54
    ## 2 x               3.04    0.0699      43.5 3.84e-118

``` r
sim_df_const %>% 
  modelr::bootstrap(n = 1000) %>% 
  mutate(models = map(strap, ~lm(y ~ x, data = .x) ),
         results = map(models, broom::tidy)) %>% 
  select(-strap, -models) %>% 
  unnest(results) %>% 
  group_by(term) %>% 
  summarize(boot_se = sd(estimate))
```

    ## # A tibble: 2 x 2
    ##   term        boot_se
    ##   <chr>         <dbl>
    ## 1 (Intercept)  0.101 
    ## 2 x            0.0735

## Airbnb

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    boro = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(boro != "Staten Island") %>% 
  select(price, stars, boro, neighborhood, room_type)
```

``` r
nyc_airbnb %>% 
  ggplot(aes(x = stars, y = price, color = room_type)) + 
  geom_point() 
```

<img src="bootstrap_files/figure-gfm/unnamed-chunk-15-1.png" width="90%" />

``` r
airbnb_results = 
 nyc_airbnb %>% 
  filter(boro == "Manhattan") %>% 
  modelr::bootstrap(n = 1000) %>% 
  mutate(
    models = map(strap, ~ lm(price ~ stars + room_type, data = .x)),
    results = map(models, broom::tidy)) %>% 
  select(results) %>% 
  unnest(results)
```

Make a plot of the `stars`

``` r
airbnb_results %>% 
  filter(term == "stars") %>% 
  ggplot(aes(x = estimate)) + geom_density()
```

<img src="bootstrap_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" />

Skew\!
