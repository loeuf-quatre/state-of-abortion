library(janitor)
library(knitr)
library(purrr)
library(rvest)
library(tidyr)

# Tidy data -------------------------------------------------------------------

guttmacher <- read_html(
  "https://www.guttmacher.org/state-policy/explore/overview-abortion-laws"
)

regs <- guttmacher %>%
  html_elements("table") %>% 
  html_table()

regs[[1]] <- regs[[1]][-c(1:2), ]
regs[[2]] <- regs[[2]][-1, ]

tidy_colnames <- map(
  regs[[2]], 
  .f = ~ paste0(unique(.x[1:2]), collapse = " ")
)

regs[[2]][1, ] <- tidy_colnames
regs[[2]] <- regs[[2]][-2, ]

regs <- regs %>%
  map(
    .f = ~row_to_names(.x, row_number = 1)
  ) %>%
  map(
    .f = ~clean_names(.x)
  ) %>%
  map(
    .f = ~ pivot_longer(
      .x,
      !state,
      names_to = "regulation",
      values_to = "state_law"
    )
  ) %>%
  bind_rows() %>%
  filter(
    nchar(state) <= 2
  ) 

regs %>% 
  group_by(
    regulation
  ) %>%
  summarize(
    levels = list(unique(sort(state_law)))
  ) %>%
  rowwise() %>%
  mutate(
    levels = list(levels[levels != ""]),
    levels = paste0(unlist(levels), collapse = ", ")
  ) %>%
  kable(
    "simple"
  ) %>%
  write_clip()

# regulation                                                                   levels                                                                                                                                                                                                                 
# ---------------------------------------------------------------------------  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# funds_all_or_most_medically_necessary_abortions                              X                                                                                                                                                                                                                      
# funds_limited_to_life_endangerment_rape_and_incest                           Life Only, X, X*, X* ,Ω, XΩ, ϴ                                                                                                                                                                                         
# mandated_counseling_includes_information_on_breast_cancer_link               X                                                                                                                                                                                                                      
# mandated_counseling_includes_information_on_fetal_pain                       X, XФ                                                                                                                                                                                                                  
# mandated_counseling_includes_information_on_negative_psychological_effects   X                                                                                                                                                                                                                      
# must_be_performed_by_a_licensed_physician                                    X, Xξ, XФ                                                                                                                                                                                                              
# must_be_performed_in_a_hospital_if_at                                        14 weeks, 20 weeks, 24 weeks, 2nd trimester, 3rd trimester, Viability                                                                                                                                                  
# parental_involvement_required_for_minors                                     ▼, Consent, Consent and Notice, Consentþ, Consentβ, Consentξ, Notice, Noticeβ, Noticeξ                                                                                                                                 
# partial_birth_abortion_banned                                                ▼, Postviability, X                                                                                                                                                                                                    
# private_insurance_coverage_limited                                           X                                                                                                                                                                                                                      
# prohibited_except_in_cases_of_life_or_health_endangerment_if_at              0 weeks, 0 weeks*,‡, 0 weeks†, 0 weeks‡, 0 weeks‡,Ω, 0 weeks‡*, 20 weeks*, 22 weeks*, 24 weeks, 24 weeks*, 24 weeksΩ, 3rd trimester, 6 weeks*, 6 weeks†, Viability, Viability*, Viability‡, Viability‡,†,Ω, ViabilityΩ 
# providers_may_refuse_to_participate_individual                               X                                                                                                                                                                                                                      
# providers_may_refuse_to_participate_institution                              Private, Religious, X                                                                                                                                                                                                  
# second_physician_must_participate_if_at                                      15 weeks, 20 weeks, 3rd trimester, Viability                                                                                                                                                                           
# waiting_period_in_hours_after_counseling                                     §, ▼, 18, 24, 48, 72, 72◊                                                                                                                                                                                              

# Mapping conditions --------------------------------------

affirmed <- "grepl('X|Private|Religious', state_law)"
excepted <- "grepl('†|‡|Ω|ϴ|Only', state_law)"
enjoined <- "grepl('▼|§', state_law)"

first_tri <- "grepl('0 weeks|6 weeks|14 weeks|15 weeks', state_law)"
second_tri <- "grepl('20 weeks|22 weeks|24 weeks|2nd trimester|iability', state_law)"
third_tri <- "state_law == '3rd trimester'"

consent <- "grepl('Consent', state_law)"
notice <- "grepl('Notice', state_law)"

waiting <- "regulation == 'waiting_period_in_hours_after_counseling'"
waiting_period <- "grepl('[0-9]', state_law)"

f1 <- tibble::lst(
  affirmed,
  excepted,
  enjoined,
  first_tri,
  second_tri,
  third_tri,
  consent,
  notice,
  waiting,
  waiting_period
)

f1 <- lapply(f1, function(x) parse_expr(x))
f1 <- list2env(f1, env = .GlobalEnv)

regs <- regs %>%
  mutate(
    state_law = trimws(state_law),
    status = case_when(
      !!affirmed ~ "yes",
      !!exception ~ "exception",
      !!enjoined ~ "enjoined"
    ),
    timing = case_when(
      !!first_tri ~ "first_trimester",
      !!second_tri ~ "second_trimester"
    ),
    consent = case_when(
      !!consent & !!notice ~ "consent_and_notice",
      !!consent ~ "consent",
      !!notice ~ "notice"
    ),
    waiting = case_when(
      !!waiting & !!waiting_period ~ "yes"
    )
  ) %>%
  unite(
    "state_law_condensed", 
    status:waiting, 
    sep = "__", 
    na.rm = TRUE
  ) %>%
  select(
    -state_law
  ) %>%
  pivot_wider(
    names_from = regulation,
    values_from = state_law_condensed
  ) %>%
  mutate(
    across(!state, .fns = ~ifelse(.x == "", "none", .x))
  )

# Correspondence analysis ---------------------------------

regs <- tibble::column_to_rownames(regs, "state")

regs_mca <- FactoMineR::MCA(regs, ncp = 10, graph = FALSE)

# 
factoextra::fviz_screeplot(regs_mca)

"
~4 dimensions (~50% variance) look reasonable
"

# https://osf.io/kthnf/
# https://osf.io/2aczd/



factoextra::fviz_contrib(regs_mca, "var", axes = 1, top = 10)
factoextra::fviz_mca_var(regs_mca, "var", axes = c(1, 2))
factoextra::fviz_mca_biplot(regs_mca, axes = c(1, 2))

loads <- regs_mca$ind$coord[, 1:4] %>%
  data.frame() %>%
  clean_names()

factoextra::fviz_nbclust(loads, FUNcluster = factoextra::hcut, method = "wss")

d <- dist(loads)
hc <- hclust(d, method = "ward.D")
dend <- as.dendrogram(hc)

dend %>% color_branches(k = 5) %>% color_labels(k = 5) %>% set("labels_cex", .7) %>% set("branches_lwd", .5) %>% as.ggdend() %>% ggplot(horiz = TRUE)

regs_tidy$cluster <- cutree(hc1, k = 4)

km <- kmeans(loads[, -1], 8)
loads$clu <- km$cluster
loads$state <- ar$state

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
