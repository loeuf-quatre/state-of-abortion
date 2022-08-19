
library(tabulizer)

# Tidy data -------------------------------------------------------------------

# Population ----------------------------------------------

# Abortion rate -------------------------------------------

# Polling -------------------------------------------------

polling <- osf_retrieve_file("62ffecb57eb6d303431fade3")
polling <- osf_download(polling, temp_dir, conflicts = "overwrite")

mothers_life <- extract_tables(polling$local_path, pages = 62:64)

clean_pdf_table <- function(l) {
  
}

mothers_life <- mothers_life %>%
  map(
    .f = ~ .x[, 1:5]
  ) %>%
  do.call(
    what = "rbind"
  ) %>%
  as_tibble()

mothers_life <- mothers_life %>%
  map(
    # Some table columns span multiple rows
    .f = ~ c(paste0(unique(.x[1:3]), collapse = " "), .x)
  ) %>%
  as_tibble() %>%
  .[-c(2:4), ] %>%
  row_to_names(
    row_number = 1
  ) %>%
  clean_names() %>%
  rename(
    state = x
  ) %>%
  mutate(
    across(!state, ~ as.numeric(.x))
  ) %>%
  pivot_longer(
    cols = !state
  ) %>%
  group_by(
    state
  ) %>%
  mutate(
    support_value = value[name == "support"]
  ) %>%
  ungroup() %>%
  mutate(
    support_rk = dense_rank(-support_value)
  ) %>%
  arrange(
    support_rk
  )

mothers_life$state <- factor(mothers_life$state, unique(mothers_life$state))

# % White Women 18-39 -------------------------------------

# % Urban -------------------------------------------------

# % GOP ---------------------------------------------------

# % College -----------------------------------------------

# % Religious ---------------------------------------------


# Population ----------------------------------------------

pop <- read.delim("/Users/edwardgivens/Downloads/Single-Race Population Estimates 2010-2019 by State and Single-Year Age (21).txt")

pop <- clean_names(pop)
pop$race_ethnicity <- with(pop, paste(race, ethnicity, sep = " "))

pop %>%
  group_by(
    states,
    race_ethnicity,
    five_year_age_groups_code
  ) %>%
  summarize(
    n_race_age = sum(population)
  ) %>%
  group_by(
    states
  ) %>%
  mutate(
    n_total = sum(n_race_age)
  ) %>%
  filter(
    race_ethnicity == "White Not Hispanic or Latino" &
    five_year_age_groups_code %in% c("15-19", "20-24", "25-29", "30-34", "35-39")
  ) %>%
  group_by(
    states
  ) %>%
  summarize(
    n_race_age = sum(n_race_age),
    n_total = max(n_total),
    per_race_age = n_race_age / n_total
  )
