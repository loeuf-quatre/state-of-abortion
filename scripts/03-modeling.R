###############################################################################
# Title: 01-regulations.R                                                     #
# Description: Cull and organize Guttmacher Institute data on abortion        #
# regulations by US state                                                     #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(dplyr) # Wrangling
library(janitor) # Formatting
library(knitr) # Inline tables
library(purrr) # Iterative functions
library(rvest) # Web scraping
library(tidyr) # Wrangling

# Tidy data -------------------------------------------------------------------

# Scrape and tidy HTML table ------------------------------

guttmacher <- read_html(
  "https://www.guttmacher.org/state-policy/explore/overview-abortion-laws"
)

regs <- guttmacher %>%
  html_elements(
    "table"
  ) %>% 
  html_table()

regs <- regs %>%
  map(
    .f = ~ .x[-1, ] # Remove first row
  ) %>%
  map(
    # Some table columns span multiple rows
    .f = ~ map(.x, ~ c(paste0(unique(.x[1:2]), collapse = " "), .x))
  ) %>%
  map(
    .f = ~ map(.x, ~ .x[-c(2:3)])
  ) %>%
  map(
    .f = ~ as_tibble(.x)
  ) %>%
  map(
    .f = ~ row_to_names(.x, row_number = 1)
  ) %>%
  map(
    .f = ~ clean_names(.x)
  )

regs <- regs %>%
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
    nchar(state) == 2 # Remove table footnotes
  ) %>%
  mutate(
    state_law = replace(state_law, state_law == "", NA)
  ) %>%
  arrange(
    state
  )

kable(head(regs, 25), "simple")

# state   regulation                                                                      state_law 
# ------  ------------------------------------------------------------------------------  ----------
# AK      must_be_performed_by_a_licensed_physician                                       X         
# AK      must_be_performed_in_a_hospital_if_at                                           NA        
# AK      second_physician_must_participate_if_at                                         NA        
# AK      prohibited_except_in_cases_of_life_or_health_endangerment_if_at                 NA        
# AK      partial_birth_abortion_banned                                                   ▼         
# AK      public_funding_of_abortion_funds_all_or_most_medically_necessary_abortions      X         
# AK      public_funding_of_abortion_funds_limited_to_life_endangerment_rape_and_incest   NA        
# AK      private_insurance_coverage_limited                                              NA        
# AK      providers_may_refuse_to_participate_individual                                  X         
# AK      providers_may_refuse_to_participate_institution                                 Private   
# AK      mandated_counseling_includes_information_on_breast_cancer_link                  X         
# AK      mandated_counseling_includes_information_on_fetal_pain                          X         
# AK      mandated_counseling_includes_information_on_negative_psychological_effects      NA        
# AK      waiting_period_in_hours_after_counseling                                        NA        
# AK      parental_involvement_required_for_minors                                        ▼         
# AL      must_be_performed_by_a_licensed_physician                                       X         
# AL      must_be_performed_in_a_hospital_if_at                                           Viability 
# AL      second_physician_must_participate_if_at                                         Viability 
# AL      prohibited_except_in_cases_of_life_or_health_endangerment_if_at                 0 weeks   
# AL      partial_birth_abortion_banned                                                   ▼         
# AL      public_funding_of_abortion_funds_all_or_most_medically_necessary_abortions      NA        
# AL      public_funding_of_abortion_funds_limited_to_life_endangerment_rape_and_incest   X         
# AL      private_insurance_coverage_limited                                              NA        
# AL      providers_may_refuse_to_participate_individual                                  NA        
# AL      providers_may_refuse_to_participate_institution                                 NA        
  
regs %>%
  group_by(
    regulation
  ) %>%
  summarize(
    state_laws = paste0(sort(unique(state_law)), collapse = ", ")
  ) %>%
  kable(
    "simple"
  )

# regulation                                                                      state_laws                                                                                                                                                                                                             
# ------------------------------------------------------------------------------  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# mandated_counseling_includes_information_on_breast_cancer_link                  X                                                                                                                                                                                                                      
# mandated_counseling_includes_information_on_fetal_pain                          X, XФ                                                                                                                                                                                                                  
# mandated_counseling_includes_information_on_negative_psychological_effects      X                                                                                                                                                                                                                      
# must_be_performed_by_a_licensed_physician                                       X, Xξ, XФ                                                                                                                                                                                                              
# must_be_performed_in_a_hospital_if_at                                           14 weeks, 20 weeks, 24 weeks, 2nd trimester, 3rd trimester, Viability                                                                                                                                                  
# parental_involvement_required_for_minors                                        ▼, Consent, Consent and Notice, Consentþ, Consentβ, Consentξ, Notice, Noticeβ, Noticeξ                                                                                                                                 
# partial_birth_abortion_banned                                                   ▼, Postviability, X                                                                                                                                                                                                    
# private_insurance_coverage_limited                                              X                                                                                                                                                                                                                      
# prohibited_except_in_cases_of_life_or_health_endangerment_if_at                 0 weeks, 0 weeks*,‡, 0 weeks†, 0 weeks‡, 0 weeks‡,Ω, 0 weeks‡*, 20 weeks*, 22 weeks*, 24 weeks, 24 weeks*, 24 weeksΩ, 3rd trimester, 6 weeks*, 6 weeks†, Viability, Viability*, Viability‡, Viability‡,†,Ω, ViabilityΩ 
# providers_may_refuse_to_participate_individual                                  X                                                                                                                                                                                                                      
# providers_may_refuse_to_participate_institution                                 Private, Religious, X                                                                                                                                                                                                  
# public_funding_of_abortion_funds_all_or_most_medically_necessary_abortions      X                                                                                                                                                                                                                      
# public_funding_of_abortion_funds_limited_to_life_endangerment_rape_and_incest   Life Only, X, X*, X* ,Ω, XΩ, ϴ                                                                                                                                                                                         
# second_physician_must_participate_if_at                                         15 weeks, 20 weeks, 3rd trimester, Viability                                                                                                                                                                           
# waiting_period_in_hours_after_counseling                                        §, ▼, 18, 24, 48, 72, 72◊                                                                                                                                                                                              

# Condense factor levels ----------------------------------

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

