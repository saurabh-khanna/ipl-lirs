LIRS Rapid Response Survey
================
Saurabh Khanna
2020-07-18

  - [Consent](#consent)

``` r
# Libraries
library(tidyverse)

# Parameters
df <- read_csv(here::here("data", "pilot_data.csv"))
```

## Consent

``` r
df %>% 
  select(consent) %>% 
  mutate(consent = as_factor(consent)) %>% 
  ggplot(aes(consent)) +
  geom_bar()
```

<img src="pilot_files/figure-gfm/unnamed-chunk-2-1.png" width="672" />
