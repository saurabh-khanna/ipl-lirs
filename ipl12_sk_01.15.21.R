## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = FALSE, fig.retina = 4)
knitr::opts_chunk$set(fig.pos = 'H')


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# Libraries
pacman::p_load(tidyverse, readxl, janitor, lubridate, extrafont, hrbrthemes, sf, ggcorrplot, plotly, ggradar, estimatr, texreg, estimatr)
extrafont::loadfonts()

# Parameters
df_baseline <- 
  read_csv(here::here("data", "Rapid_response_baseline_cleaned_all_consent.csv")) %>%
  inner_join(read_csv(here::here("data", "weights.csv")), by = "study_ID") %>% 
  filter(yearArrive >= 2010) %>% 
  mutate(
    years_in_us = 2020 - yearArrive,
    age_at_arrival = yearArrive - birthYear,
    female = recode(gender, "Male" = "0", "Female" = "1", .default = NA_character_) %>% as.character %>% parse_integer(),
    unemploymentBeneBaseline = if_else(unemploymentBeneBaseline == "Yes", "1", unemploymentBeneBaseline),
    unemploymentBeneBaseline = if_else(unemploymentBeneBaseline == "No", "0", unemploymentBeneBaseline),
    unemployment = unemploymentBeneBaseline %>% as.integer()
  )


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# extracting data for dlaitin on box
# df_baseline %>%
#   select(-contains(c("name", "numb", "phone", "cell", "mobile", "mail", "contact", "address"))) %>%
#   relocate(ResponseID, ResponseSet, weight) %>% 
#   write_csv(here::here("data", "baseline_data_with_weights_de-identified_11.03.2020.csv"))
#df_baseline %>% select(contains("12"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  select(contains("12s")) %>% 
  summarize_all(~ mean(., na.rm = T)) %>% 
  rename_all(~ str_c(., "_mean")) %>% 
  relocate(ipl12s_mean)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  select(contains("12s"), weight) %>% 
  summarize_at(vars(contains("12s")), ~ weighted.mean(., w = weight, na.rm = T)) %>% 
  rename_all(~ str_c(., "_mean")) %>% 
  relocate(ipl12s_mean)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  select(contains("12s")) %>% 
  summarize_all(~ median(., na.rm = T)) %>% 
  rename_all(~ str_c(., "_median")) %>% 
  relocate(ipl12s_median)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  select(contains("12s")) %>% 
  summarize_all(~ sd(., na.rm = T)) %>% 
  rename_all(~ str_c(., "_sd")) %>% 
  relocate(ipl12s_sd)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% count(female)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>%
  mutate(
    read = case_when(
      read == "Very well" ~ 5,
      read == "Well" ~ 4,
      read == "Moderately well" ~ 3,
      read == "Not well" ~ 2,
      read == "Not well at all" ~ 1
    ),
    speak = case_when(
      speak == "Very well" ~ 5,
      speak == "Well" ~ 4,
      speak == "Moderately well" ~ 3,
      speak == "Not well" ~ 2,
      speak == "Not well at all" ~ 1
    ),
    citizen = recode(citizen, "No" = "0", "Yes" = "1", default = NA_character_) %>% as.integer(),
    cosponsorship = recode(cosponsorship, "No" = "0", "Yes" = "1", default = NA_character_) %>% as.integer()
  ) %>% 
  drop_na(female) %>% 
  group_by(female) %>% 
  summarise_at(
    vars(years_in_us, citizen, age, age_at_arrival, houseHoldIncome, unemployment, cosponsorship, householdChildren, householdSize, educationLevelNumeric, contains("12s")), 
    ~ weighted.mean(., weight, na.rm = T)
  ) %>% 
  pivot_longer(-female) %>% 
  pivot_wider(names_from = female, values_from = value) %>% 
  select(variable = name, male = `0`, female = `1`) %>% 
  mutate_at(vars(contains("ale")), ~ round(., 4))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  ggplot(aes(ipl12s, stat(density), weight = weight)) +
  geom_histogram(color = "black", fill = "grey") +
  geom_density() +
  theme_ipsum()


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  ggplot(aes(score, weight = weight)) +
  geom_density(color = "black", fill = "grey", alpha = 0.5) +
  facet_wrap(vars(measure)) +
  theme_ipsum()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
x <- df_baseline %>% select(contains("12s"))
ggcorrplot(cor(x, use = "pairwise.complete.obs"), hc.order = TRUE, type = "lower", p.mat = cor_pmat(x), insig = "blank", lab = TRUE)
rm(x)


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
plot <-
  ussf::boundaries(geography = "state") %>%
  select(stateCurrent = NAME) %>% 
  left_join(
    df_baseline %>% 
      mutate(
        stateCurrent = if_else(stateCurrent == "Refused", NA_character_, stateCurrent),
        stateCurrent = if_else(stateCurrent == "I do not reside in the United States", NA_character_, stateCurrent)
      ) %>% 
      group_by(stateCurrent) %>% 
      summarize(
        ipl12s = mean(ipl12s, na.rm = T) %>% round(2),
        weight = mean(weight, na.rm = T)
      ),
    by = "stateCurrent"
  ) %>% 
  ggplot(aes(fill = ipl12s, weight = weight, text = stateCurrent)) +
  geom_sf(size = 0.3, show.legend = F) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplotly(plot, tooltip = c("stateCurrent", "ipl12s")) %>% 
  layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
plot <-
  ussf::boundaries(geography = "state") %>%
  select(stateCurrent = NAME) %>% 
  left_join(
    df_baseline %>% 
      mutate(
        stateCurrent = if_else(stateCurrent == "Refused", NA_character_, stateCurrent),
        stateCurrent = if_else(stateCurrent == "I do not reside in the United States", NA_character_, stateCurrent)
      ) %>% 
      group_by(stateCurrent) %>% 
      summarize(
        ling12s = mean(ling12s, na.rm = T) %>% round(2),
        weight = mean(weight, na.rm = T)
      ),
    by = "stateCurrent"
  ) %>% 
  ggplot(aes(fill = ling12s, weight = weight, text = stateCurrent)) +
  geom_sf(size = 0.3, show.legend = F) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplotly(plot, tooltip = c("stateCurrent", "ling12s")) %>% 
  layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
plot <-
  ussf::boundaries(geography = "state") %>%
  select(stateCurrent = NAME) %>% 
  left_join(
    df_baseline %>% 
      mutate(
        stateCurrent = if_else(stateCurrent == "Refused", NA_character_, stateCurrent),
        stateCurrent = if_else(stateCurrent == "I do not reside in the United States", NA_character_, stateCurrent)
      ) %>% 
      group_by(stateCurrent) %>% 
      summarize(
        pol12s = mean(pol12s, na.rm = T) %>% round(2),
        weight = mean(weight, na.rm = T)
      ),
    by = "stateCurrent"
  ) %>% 
  ggplot(aes(fill = pol12s, weight = weight, text = stateCurrent)) +
  geom_sf(size = 0.3, show.legend = F) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplotly(plot, tooltip = c("stateCurrent", "pol12s")) %>% 
  layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
plot <-
  ussf::boundaries(geography = "state") %>%
  select(stateCurrent = NAME) %>% 
  left_join(
    df_baseline %>% 
      mutate(
        stateCurrent = if_else(stateCurrent == "Refused", NA_character_, stateCurrent),
        stateCurrent = if_else(stateCurrent == "I do not reside in the United States", NA_character_, stateCurrent)
      ) %>% 
      group_by(stateCurrent) %>% 
      summarize(
        soc12s = mean(soc12s, na.rm = T) %>% round(2),
        weight = mean(weight, na.rm = T)
      ),
    by = "stateCurrent"
  ) %>% 
  ggplot(aes(fill = soc12s, weight = weight, text = stateCurrent)) +
  geom_sf(size = 0.3, show.legend = F) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplotly(plot, tooltip = c("stateCurrent", "soc12s")) %>% 
  layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
plot <-
  ussf::boundaries(geography = "state") %>%
  select(stateCurrent = NAME) %>% 
  left_join(
    df_baseline %>% 
      mutate(
        stateCurrent = if_else(stateCurrent == "Refused", NA_character_, stateCurrent),
        stateCurrent = if_else(stateCurrent == "I do not reside in the United States", NA_character_, stateCurrent)
      ) %>% 
      group_by(stateCurrent) %>% 
      summarize(
        econ12s = mean(econ12s, na.rm = T) %>% round(2),
        weight = mean(weight, na.rm = T)
      ),
    by = "stateCurrent"
  ) %>% 
  ggplot(aes(fill = econ12s, weight = weight, text = stateCurrent)) +
  geom_sf(size = 0.3, show.legend = F) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplotly(plot, tooltip = c("stateCurrent", "econ12s")) %>% 
  layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
plot <-
  ussf::boundaries(geography = "state") %>%
  select(stateCurrent = NAME) %>% 
  left_join(
    df_baseline %>% 
      mutate(
        stateCurrent = if_else(stateCurrent == "Refused", NA_character_, stateCurrent),
        stateCurrent = if_else(stateCurrent == "I do not reside in the United States", NA_character_, stateCurrent)
      ) %>% 
      group_by(stateCurrent) %>% 
      summarize(
        psy12s = mean(psy12s, na.rm = T) %>% round(2),
        weight = mean(weight, na.rm = T)
      ),
    by = "stateCurrent"
  ) %>% 
  ggplot(aes(fill = psy12s, weight = weight, text = stateCurrent)) +
  geom_sf(size = 0.3, show.legend = F) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplotly(plot, tooltip = c("stateCurrent", "psy12s")) %>% 
  layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
plot <-
  ussf::boundaries(geography = "state") %>%
  select(stateCurrent = NAME) %>% 
  left_join(
    df_baseline %>% 
      mutate(
        stateCurrent = if_else(stateCurrent == "Refused", NA_character_, stateCurrent),
        stateCurrent = if_else(stateCurrent == "I do not reside in the United States", NA_character_, stateCurrent)
      ) %>% 
      group_by(stateCurrent) %>% 
      summarize(
        nav12s = mean(nav12s, na.rm = T) %>% round(2),
        weight = mean(weight, na.rm = T)
      ),
    by = "stateCurrent"
  ) %>% 
  ggplot(aes(fill = nav12s, weight = weight, text = stateCurrent)) +
  geom_sf(size = 0.3, show.legend = F) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplotly(plot, tooltip = c("stateCurrent", "nav12s")) %>% 
  layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))


## ---- eval=F----------------------------------------------------------------------------------------------------------------------------------------------
## df_baseline %>% select(contains("age"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(years_in_us) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>% 
  filter(years_in_us <= 10) %>% 
  ggplot(aes(years_in_us, ipl12s, weight = weight)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_ipsum()


## ---- out.width="100%"------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>%
  group_by(years_in_us) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  drop_na(years_in_us) %>% 
  filter(years_in_us <= 10) %>%
  ggplot(aes(years_in_us, score, weight = weight)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_ipsum() +
  facet_wrap(vars(measure))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>%
  group_by(female) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>% 
  ggplot(aes(factor(female), ipl12s, weight = weight)) +
  geom_col() +
  theme_ipsum()


## ---- out.width="100%"------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(female) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(female = fct_inorder(factor(female), ipl12s) %>% fct_rev()) %>%
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>%
  ggplot(aes(factor(female), score, weight = weight)) +
  geom_col() +
  facet_wrap(vars(measure)) +
  theme_ipsum()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  select(female, contains("12s"), -ipl12s) %>% 
  group_by(female) %>% 
  summarize_all(mean, na.rm = T) %>% 
  ggradar(values = c("0", "0.4", "0.8"), grid.mid = 0.4, grid.max = 0.8, legend.title = "Female")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>%
  group_by(citizen) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>% 
  ggplot(aes(factor(citizen), ipl12s, weight = weight)) +
  geom_col() +
  theme_ipsum()


## ---- out.width="100%"------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(citizen) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(citizen = fct_inorder(factor(citizen), ipl12s) %>% fct_rev()) %>%
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>%
  ggplot(aes(factor(citizen), score, weight = weight)) +
  geom_col() +
  facet_wrap(vars(measure)) +
  theme_ipsum()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(educationLevel) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(educationLevel = fct_inorder(educationLevel, ipl12s) %>% fct_rev()) %>% 
  ggplot(aes(educationLevel, ipl12s, weight = weight)) +
  geom_col() +
  theme_ipsum() +
  coord_flip()


## ---- out.width="100%"------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>%
  group_by(educationLevel) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(speak = fct_inorder(educationLevel, ipl12s) %>% fct_rev()) %>%
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  drop_na(educationLevel) %>% 
  ggplot(aes(educationLevel, score, weight = weight)) +
  geom_col() +
  theme_ipsum() +
  facet_wrap(vars(measure)) +
  coord_flip()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(speak) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(speak = fct_inorder(speak, ipl12s) %>% fct_rev()) %>% 
  ggplot(aes(speak, ipl12s, weight = weight)) +
  geom_col() +
  theme_ipsum() +
  coord_flip()


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(speak) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(speak = fct_inorder(speak, ipl12s) %>% fct_rev()) %>%
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  ggplot(aes(speak, score, weight = weight)) +
  geom_col() +
  facet_wrap(vars(measure)) +
  theme_ipsum() + 
  coord_flip()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(read) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(read = fct_inorder(read, ipl12s) %>% fct_rev()) %>% 
  ggplot(aes(read, ipl12s, weight = weight)) +
  geom_col() +
  theme_ipsum()


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(read) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(read = fct_inorder(read, ipl12s) %>% fct_rev()) %>%
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  ggplot(aes(read, score, weight = weight)) +
  geom_col() +
  facet_wrap(vars(measure)) +
  theme_ipsum() + 
  coord_flip()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(income) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>% 
  ggplot(aes(income, ipl12s, weight = weight)) +
  geom_col() +
  theme_ipsum()


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(income) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(
    income = as_factor(income),
    income = fct_inorder(income, ipl12s) %>% fct_rev()
    ) %>%
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  ggplot(aes(income, score, weight = weight)) +
  geom_col() +
  facet_wrap(vars(measure)) +
  theme_ipsum() + 
  coord_flip()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(unemployment) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>% 
  ggplot(aes(factor(unemployment), ipl12s, weight = weight)) +
  geom_col() +
  theme_ipsum()


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
df_baseline %>%
  group_by(unemployment) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(
    unemployment = as_factor(unemployment),
    unemployment = fct_inorder(unemployment, ipl12s) %>% fct_rev()
    ) %>%
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  ggplot(aes(unemployment, score, weight = weight)) +
  geom_col() +
  facet_wrap(vars(measure)) +
  theme_ipsum() + 
  coord_flip()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(bcountryBinned) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(bcountryBinned = fct_inorder(bcountryBinned, ipl12s) %>% fct_rev()) %>% 
  ggplot(aes(bcountryBinned, ipl12s, weight = weight)) +
  geom_col() +
  theme_ipsum()


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(bcountryBinned) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  mutate(
    bcountryBinned = as_factor(bcountryBinned),
    bcountryBinned = fct_inorder(bcountryBinned, ipl12s) %>% fct_rev()
    ) %>%
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  ggplot(aes(bcountryBinned, score, weight = weight)) +
  geom_col() +
  facet_wrap(vars(measure)) +
  theme_ipsum() + 
  coord_flip()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>%
  filter(householdSize <= 11) %>% 
  group_by(householdSize) %>% 
  summarise(
    ipl12s = mean(ipl12s, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE)
    ) %>%
  drop_na() %>% 
  ggplot(aes(householdSize, ipl12s, weight = weight)) +
  geom_col() +
  theme_ipsum()


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(householdSize) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  drop_na(householdSize) %>% 
  filter(householdSize <= 11) %>% 
  mutate(householdSize = as.integer(householdSize)) %>%
  ggplot(aes(householdSize, score)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  theme_ipsum() +
  facet_wrap(vars(measure))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
# preprocessing some vars
df_baseline <-
  df_baseline %>% 
  mutate(
    read = case_when(
      read == "Very well" ~ 5,
      read == "Well" ~ 4,
      read == "Moderately well" ~ 3,
      read == "Not well" ~ 2,
      read == "Not well at all" ~ 1
    ),
    speak = case_when(
      speak == "Very well" ~ 5,
      speak == "Well" ~ 4,
      speak == "Moderately well" ~ 3,
      speak == "Not well" ~ 2,
      speak == "Not well at all" ~ 1
    ),
    educationTerciles = factor(educationTerciles, levels = c("Low education", "Medium education", "High education")),
    citizen = factor(citizen, levels = c("No", "Yes"))
  )


## ---- results='asis'--------------------------------------------------------------------------------------------------------------------------------------
lm1 <- df_baseline %>% lm_robust(ipl12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm2 <- df_baseline %>% lm_robust(ling12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm3 <- df_baseline %>% lm_robust(pol12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm4 <- df_baseline %>% lm_robust(soc12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm5 <- df_baseline %>% lm_robust(econ12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm6 <- df_baseline %>% lm_robust(psy12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm7 <- df_baseline %>% lm_robust(nav12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

knitreg(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7), include.ci = FALSE, custom.model.names = c("Overall", "Linguistic", "Political", "Social", "Economic", "Psychological", "Navigational"), stars = c(0.01, 0.05, 0.1), digits = 3)


## ---- results='asis'--------------------------------------------------------------------------------------------------------------------------------------
lm1 <- df_baseline %>% lm_robust(ipl12s ~ years_in_us, data = ., se_type = "stata", weights = weight)
lm2 <- df_baseline %>% lm_robust(ling12s ~ years_in_us, data = ., se_type = "stata", weights = weight)
lm3 <- df_baseline %>% lm_robust(pol12s ~ years_in_us, data = ., se_type = "stata", weights = weight)
lm4 <- df_baseline %>% lm_robust(soc12s ~ years_in_us, data = ., se_type = "stata", weights = weight)
lm5 <- df_baseline %>% lm_robust(econ12s ~ years_in_us, data = ., se_type = "stata", weights = weight)
lm6 <- df_baseline %>% lm_robust(psy12s ~ years_in_us, data = ., se_type = "stata", weights = weight)
lm7 <- df_baseline %>% lm_robust(nav12s ~ years_in_us, data = ., se_type = "stata", weights = weight)

knitreg(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7), include.ci = FALSE, custom.model.names = c("Overall", "Linguistic", "Political", "Social", "Economic", "Psychological", "Navigational"), stars = c(0.01, 0.05, 0.1), digits = 3)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  filter(yearArrive >= 2010) %>% 
  ggplot() +
  geom_smooth(aes(years_in_us, ipl12s), method = "loess") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_ipsum()


## ---- out.width = "100%"----------------------------------------------------------------------------------------------------------------------------------
df_baseline %>% 
  group_by(years_in_us) %>% 
  summarise_at(vars(contains("12s"), weight), mean, na.rm = T) %>%
  drop_na() %>%
  arrange(ipl12s) %>% 
  pivot_longer(
    cols = contains("12s"),
    names_to = "measure",
    values_to = "score"
  ) %>% 
  filter(measure != "ipl12s") %>% 
  drop_na(years_in_us) %>% 
  filter(years_in_us <= 10) %>% 
  ggplot() +
  geom_smooth(aes(years_in_us, score), method = "loess") +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  facet_wrap(vars(measure))


## ---- results='asis'--------------------------------------------------------------------------------------------------------------------------------------
lm1 <- df_baseline %>% lm_robust(ipl12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm2 <- df_baseline %>% lm_robust(ling12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm3 <- df_baseline %>% lm_robust(pol12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm4 <- df_baseline %>% lm_robust(soc12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm5 <- df_baseline %>% lm_robust(econ12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm6 <- df_baseline %>% lm_robust(psy12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm7 <- df_baseline %>% lm_robust(nav12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

knitreg(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7), include.ci = FALSE, custom.model.names = c("Overall", "Linguistic", "Political", "Social", "Economic", "Psychological", "Navigational"), stars = c(0.01, 0.05, 0.1), digits = 3)


## ---- results='asis'--------------------------------------------------------------------------------------------------------------------------------------
lm1 <- df_baseline %>% lm_robust(ipl12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight, fixed_effects = ~ stateCurrent)

lm2 <- df_baseline %>% lm_robust(ling12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight, fixed_effects = ~ stateCurrent)

lm3 <- df_baseline %>% lm_robust(pol12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight, fixed_effects = ~ stateCurrent)

lm4 <- df_baseline %>% lm_robust(soc12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight, fixed_effects = ~ stateCurrent)

lm5 <- df_baseline %>% lm_robust(econ12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight, fixed_effects = ~ stateCurrent)

lm6 <- df_baseline %>% lm_robust(psy12s ~ years_in_us + female + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight, fixed_effects = ~ stateCurrent)

lm7 <- df_baseline %>% lm_robust(nav12s ~ years_in_us + female + read + speak + householdSize +bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight, fixed_effects = ~ stateCurrent)

knitreg(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7), include.ci = FALSE, custom.model.names = c("Overall", "Linguistic", "Political", "Social", "Economic", "Psychological", "Navigational"), stars = c(0.01, 0.05, 0.1), digits = 3)


## ---- results='asis'--------------------------------------------------------------------------------------------------------------------------------------
lm1 <- df_baseline %>% lm_robust(ipl12s ~ female * years_in_us + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm2 <- df_baseline %>% lm_robust(ling12s ~ female * years_in_us + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm3 <- df_baseline %>% lm_robust(pol12s ~ female * years_in_us + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm4 <- df_baseline %>% lm_robust(soc12s ~ female * years_in_us + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm5 <- df_baseline %>% lm_robust(econ12s ~ female * years_in_us + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm6 <- df_baseline %>% lm_robust(psy12s ~ female * years_in_us + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm7 <- df_baseline %>% lm_robust(nav12s ~ female * years_in_us + read + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

knitreg(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7), include.ci = FALSE, custom.model.names = c("Overall", "Linguistic", "Political", "Social", "Economic", "Psychological", "Navigational"), stars = c(0.01, 0.05, 0.1), digits = 3)


## ---- results='asis'--------------------------------------------------------------------------------------------------------------------------------------
lm1 <- df_baseline %>% lm_robust(ipl12s ~ female + read  * years_in_us + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm2 <- df_baseline %>% lm_robust(ling12s ~ female + read * years_in_us + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm3 <- df_baseline %>% lm_robust(pol12s ~ female + read * years_in_us + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm4 <- df_baseline %>% lm_robust(soc12s ~ female + read * years_in_us + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm5 <- df_baseline %>% lm_robust(econ12s ~ female + read * years_in_us + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm6 <- df_baseline %>% lm_robust(psy12s ~ female + read * years_in_us + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm7 <- df_baseline %>% lm_robust(nav12s ~ female + read * years_in_us + speak + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

knitreg(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7), include.ci = FALSE, custom.model.names = c("Overall", "Linguistic", "Political", "Social", "Economic", "Psychological", "Navigational"), stars = c(0.01, 0.05, 0.1), digits = 3)


## ---- results='asis'--------------------------------------------------------------------------------------------------------------------------------------
lm1 <- df_baseline %>% lm_robust(ipl12s ~ female + read + speak * years_in_us + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm2 <- df_baseline %>% lm_robust(ling12s ~ female + read + speak * years_in_us + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm3 <- df_baseline %>% lm_robust(pol12s ~ female + read + speak * years_in_us + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm4 <- df_baseline %>% lm_robust(soc12s ~ female + read + speak * years_in_us + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm5 <- df_baseline %>% lm_robust(econ12s ~ female + read + speak * years_in_us + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm6 <- df_baseline %>% lm_robust(psy12s ~ female + read + speak * years_in_us + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm7 <- df_baseline %>% lm_robust(nav12s ~ female + read + speak * years_in_us + householdSize + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

knitreg(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7), include.ci = FALSE, custom.model.names = c("Overall", "Linguistic", "Political", "Social", "Economic", "Psychological", "Navigational"), stars = c(0.01, 0.05, 0.1), digits = 3)


## ---- results='asis'--------------------------------------------------------------------------------------------------------------------------------------
lm1 <- df_baseline %>% lm_robust(ipl12s ~ female + read + speak + householdSize * years_in_us + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm2 <- df_baseline %>% lm_robust(ling12s ~ female + read + speak + householdSize * years_in_us + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm3 <- df_baseline %>% lm_robust(pol12s ~ female + read + speak + householdSize * years_in_us + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm4 <- df_baseline %>% lm_robust(soc12s ~ female + read + speak + householdSize * years_in_us + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm5 <- df_baseline %>% lm_robust(econ12s ~ female + read + speak + householdSize * years_in_us + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm6 <- df_baseline %>% lm_robust(psy12s ~ female + read + speak + householdSize * years_in_us + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

lm7 <- df_baseline %>% lm_robust(nav12s ~ female + read + speak + householdSize * years_in_us + bcountryBinned + educationTerciles, data = ., se_type = "stata", weights = weight)

knitreg(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7), include.ci = FALSE, custom.model.names = c("Overall", "Linguistic", "Political", "Social", "Economic", "Psychological", "Navigational"), stars = c(0.01, 0.05, 0.1), digits = 3)


## ---- results='asis'--------------------------------------------------------------------------------------------------------------------------------------
lm1 <- df_baseline %>% lm_robust(ipl12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles * years_in_us, data = ., se_type = "stata", weights = weight)

lm2 <- df_baseline %>% lm_robust(ling12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles * years_in_us, data = ., se_type = "stata", weights = weight)

lm3 <- df_baseline %>% lm_robust(pol12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles * years_in_us, data = ., se_type = "stata", weights = weight)

lm4 <- df_baseline %>% lm_robust(soc12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles * years_in_us, data = ., se_type = "stata", weights = weight)

lm5 <- df_baseline %>% lm_robust(econ12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles * years_in_us, data = ., se_type = "stata", weights = weight)

lm6 <- df_baseline %>% lm_robust(psy12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles * years_in_us, data = ., se_type = "stata", weights = weight)

lm7 <- df_baseline %>% lm_robust(nav12s ~ female + read + speak + householdSize + bcountryBinned + educationTerciles * years_in_us, data = ., se_type = "stata", weights = weight)

knitreg(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7), include.ci = FALSE, custom.model.names = c("Overall", "Linguistic", "Political", "Social", "Economic", "Psychological", "Navigational"), stars = c(0.01, 0.05, 0.1), digits = 3)

