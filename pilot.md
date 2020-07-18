LIRS Rapid Response Survey
================
Saurabh Khanna
2020-07-18

  - [Main Survey](#main-survey)
      - [Consent](#consent)
      - [Language Selected](#language-selected)
      - [Country of origin](#country-of-origin)
      - [Home country](#home-country)
  - [SMS data from Callhub](#sms-data-from-callhub)
  - [Phone survey data](#phone-survey-data)
  - [Email survey data](#email-survey-data)
  - [General data from Sierra](#general-data-from-sierra)

``` r
# Libraries
library(tidyverse)

# Parameters
df <- read_csv(here::here("data", "pilot_data.csv"))
```

## Main Survey

### Consent

``` r
df %>% 
  mutate(consent = fct_infreq(consent)) %>% 
  ggplot(aes(consent)) +
  geom_bar()
```

<img src="pilot_files/figure-gfm/unnamed-chunk-2-1.png" width="672" />

### Language Selected

``` r
df %>% 
  mutate(Q_Language = fct_infreq(Q_Language)) %>% 
  ggplot(aes(Q_Language)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 10, 1))
```

<img src="pilot_files/figure-gfm/unnamed-chunk-3-1.png" width="672" />

### Country of origin

``` r
df %>% 
  mutate(bcountry = fct_infreq(bcountry)) %>% 
  ggplot(aes(bcountry)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  coord_flip()
```

<img src="pilot_files/figure-gfm/unnamed-chunk-4-1.png" width="672" />

### Home country

``` r
df %>% 
  mutate(homeCountry = fct_infreq(homeCountry)) %>% 
  ggplot(aes(homeCountry)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  coord_flip()
```

<img src="pilot_files/figure-gfm/unnamed-chunk-5-1.png" width="672" />

## SMS data from Callhub

## Phone survey data

## Email survey data

## General data from Sierra
