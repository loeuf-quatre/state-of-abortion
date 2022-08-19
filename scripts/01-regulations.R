###############################################################################
# Title: 01-regulations.R                                                     #
# Description: Cull and organize Guttmacher Institute data on abortion        #
# regulations by US state                                                     #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(dplyr) # Wrangling
library(glue) # String pasting
library(janitor) # Formatting
library(knitr) # Inline tables
library(osfr) # Read/write OSF projects
library(purrr) # Iterative functions
library(rvest) # Web scraping
library(tidyr) # Wrangling

 # OSF authentication. Project location https://osf.io/nt5fd/
pat <- keyring::key_get('OSF')
osf_auth(token = pat)

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

"
Compare California and Texas across Guttmacher's tracked abortion regulations
"

regs %>%
  filter(
    state %in% c("CA", "TX")
  ) %>%
  pivot_wider(
    names_from = state,
    values_from = state_law
  ) %>%
  kable(
    "simple",
    align = c("r", "c", "c")
  )

#                                                                     regulation      CA                TX         
# ------------------------------------------------------------------------------  -----------  --------------------
#                                      must_be_performed_by_a_licensed_physician      NA                X          
#                                          must_be_performed_in_a_hospital_if_at      NA                NA         
#                                        second_physician_must_participate_if_at      NA                NA         
#                prohibited_except_in_cases_of_life_or_health_endangerment_if_at   Viability        0 weeks‡*      
#                                                  partial_birth_abortion_banned      NA                X          
#     public_funding_of_abortion_funds_all_or_most_medically_necessary_abortions       X                NA         
#  public_funding_of_abortion_funds_limited_to_life_endangerment_rape_and_incest      NA                X          
#                                             private_insurance_coverage_limited      NA                X          
#                                 providers_may_refuse_to_participate_individual       X                X          
#                                providers_may_refuse_to_participate_institution   Religious         Private       
#                 mandated_counseling_includes_information_on_breast_cancer_link      NA                X          
#                         mandated_counseling_includes_information_on_fetal_pain      NA                X          
#     mandated_counseling_includes_information_on_negative_psychological_effects      NA                X          
#                                       waiting_period_in_hours_after_counseling      NA                24         
#                                       parental_involvement_required_for_minors       ▼        Consent and Notice
                                      
"
Observe current state law category levels within each regulation
"

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
excepted <- "grepl('\\\\*|†|‡|Ω|ϴ|Only', state_law)"
enjoined <- "grepl('▼|§', state_law)"

first_tri <- "grepl('^0 weeks|^6 weeks|14 weeks|15 weeks', state_law)"
second_tri <- "'20 weeks|22 weeks|24 weeks|2nd trimester|iability'"
second_tri <- glue("grepl({second_tri}, state_law)")
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
f1 <- map(f1, ~ parse_expr(.x))

list2env(f1, env = .GlobalEnv)

regs <- regs %>%
  mutate(
    state_law = trimws(state_law),
    status = case_when(
      !!affirmed ~ "yes",
      !!excepted ~ "exception",
      !!enjoined ~ "enjoined"
    ),
    timing = case_when(
      !!first_tri  ~ "first_trimester",
      !!second_tri ~ "second_trimester",
      !!third_tri  ~ "third_trimester"
    ),
    consent = case_when(
      !!consent & !!notice ~ "consent_and_notice",
      !!consent            ~ "consent",
      !!notice             ~ "notice"
    ),
    waiting = case_when(
      !!waiting & !!waiting_period ~ "yes"
    )
  )

regs <- regs %>%
  unite(
    "state_law_condensed", 
    status:waiting, 
    sep = "__", 
    na.rm = TRUE
  )

"
Confirm mapping fidelity
"

regs %>%
  select(
    state_law,
    state_law_condensed
  ) %>%
  distinct() %>%
  arrange(
    state_law
  ) %>%
  kable(
    "simple"
  )

# state_law            state_law_condensed         
# -------------------  ----------------------------
# §                    enjoined                    
# ▼                    enjoined                    
# 0 weeks              first_trimester             
# 0 weeks*,‡           exception__first_trimester  
# 0 weeks†             exception__first_trimester  
# 0 weeks‡             exception__first_trimester  
# 0 weeks‡,Ω           exception__first_trimester  
# 0 weeks‡*            exception__first_trimester  
# 14 weeks             first_trimester             
# 15 weeks             first_trimester             
# 18                   yes                         
# 20 weeks             second_trimester            
# 20 weeks*            exception__second_trimester 
# 22 weeks*            exception__second_trimester 
# 24                   yes                         
# 24 weeks             second_trimester            
# 24 weeks*            exception__second_trimester 
# 24 weeksΩ            exception__second_trimester 
# 2nd trimester        second_trimester            
# 3rd trimester        third_trimester             
# 48                   yes                         
# 6 weeks*             exception__first_trimester  
# 6 weeks†             exception__first_trimester  
# 72                   yes                         
# 72◊                  yes                         
# Consent              consent                     
# Consent and Notice   consent_and_notice          
# Consentþ             consent                     
# Consentβ             consent                     
# Consentξ             consent                     
# Life Only            exception                   
# Notice               notice                      
# Noticeβ              notice                      
# Noticeξ              notice                      
# Postviability        second_trimester            
# Private              yes                         
# Religious            yes                         
# Viability            second_trimester            
# Viability*           exception__second_trimester 
# Viability‡           exception__second_trimester 
# Viability‡,†,Ω       exception__second_trimester 
# ViabilityΩ           exception__second_trimester 
# X                    yes                         
# X*                   yes                         
# X* ,Ω                yes                         
# Xξ                   yes                         
# XΩ                   yes                         
# XФ                   yes                         
# ϴ                    exception                   
# NA                                               

regs <- regs %>%
  select(
    -state_law
  ) %>%
  pivot_wider(
    names_from = regulation,
    values_from = state_law_condensed
  ) %>%
  mutate(
    across(!state, .fns = ~ ifelse(.x == "", "none", .x))
  )

"
Compare California and Texas again
"

regs %>%
  filter(
    state %in% c("CA", "TX")
  ) %>%
  kable(
    "simple"
  )

# state   must_be_performed_by_a_licensed_physician   must_be_performed_in_a_hospital_if_at   second_physician_must_participate_if_at   prohibited_except_in_cases_of_life_or_health_endangerment_if_at   partial_birth_abortion_banned   public_funding_of_abortion_funds_all_or_most_medically_necessary_abortions   public_funding_of_abortion_funds_limited_to_life_endangerment_rape_and_incest   private_insurance_coverage_limited   providers_may_refuse_to_participate_individual   providers_may_refuse_to_participate_institution   mandated_counseling_includes_information_on_breast_cancer_link   mandated_counseling_includes_information_on_fetal_pain   mandated_counseling_includes_information_on_negative_psychological_effects   waiting_period_in_hours_after_counseling   parental_involvement_required_for_minors 
# ------  ------------------------------------------  --------------------------------------  ----------------------------------------  ----------------------------------------------------------------  ------------------------------  ---------------------------------------------------------------------------  ------------------------------------------------------------------------------  -----------------------------------  -----------------------------------------------  ------------------------------------------------  ---------------------------------------------------------------  -------------------------------------------------------  ---------------------------------------------------------------------------  -----------------------------------------  -----------------------------------------
# CA      none                                        none                                    none                                      second_trimester                                                  none                            yes                                                                          none                                                                            none                                 yes                                              yes                                               none                                                             none                                                     none                                                                         none                                       enjoined                                 
# TX      yes                                         none                                    none                                      exception__first_trimester                                        yes                             none                                                                         yes                                                                             yes                                  yes                                              yes                                               yes                                                              yes                                                      yes                                                                          yes                                        consent_and_notice                       

glimpse(regs)

# Rows: 51
# Columns: 16
# $ state                                                                         <chr> "AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA…
# $ must_be_performed_by_a_licensed_physician                                     <chr> "yes", "yes", "yes", "yes", "none", "none", "none", "none", "none", "yes", …
# $ must_be_performed_in_a_hospital_if_at                                         <chr> "none", "second_trimester", "none", "second_trimester", "none", "none", "se…
# $ second_physician_must_participate_if_at                                       <chr> "none", "second_trimester", "second_trimester", "second_trimester", "none",…
# $ prohibited_except_in_cases_of_life_or_health_endangerment_if_at               <chr> "none", "first_trimester", "exception__first_trimester", "second_trimester"…
# $ partial_birth_abortion_banned                                                 <chr> "enjoined", "enjoined", "yes", "yes", "none", "none", "none", "none", "none…
# $ public_funding_of_abortion_funds_all_or_most_medically_necessary_abortions    <chr> "yes", "none", "none", "none", "yes", "none", "yes", "none", "none", "none"…
# $ public_funding_of_abortion_funds_limited_to_life_endangerment_rape_and_incest <chr> "none", "yes", "yes", "exception", "none", "yes", "none", "yes", "yes", "ye…
# $ private_insurance_coverage_limited                                            <chr> "none", "none", "none", "yes", "none", "none", "none", "none", "none", "non…
# $ providers_may_refuse_to_participate_individual                                <chr> "yes", "none", "yes", "yes", "yes", "none", "yes", "none", "yes", "yes", "y…
# $ providers_may_refuse_to_participate_institution                               <chr> "yes", "none", "yes", "yes", "yes", "none", "none", "none", "yes", "yes", "…
# $ mandated_counseling_includes_information_on_breast_cancer_link                <chr> "yes", "none", "none", "none", "none", "none", "none", "none", "none", "non…
# $ mandated_counseling_includes_information_on_fetal_pain                        <chr> "yes", "none", "yes", "none", "none", "none", "none", "none", "none", "none…
# $ mandated_counseling_includes_information_on_negative_psychological_effects    <chr> "none", "none", "none", "none", "none", "none", "none", "none", "none", "no…
# $ waiting_period_in_hours_after_counseling                                      <chr> "none", "yes", "yes", "yes", "none", "none", "none", "none", "none", "enjoi…
# $ parental_involvement_required_for_minors                                      <chr> "enjoined", "consent", "consent", "consent", "enjoined", "notice", "none", …

# Export data to OSF ----------------------------------------------------------

regs_path <- file.path(tempdir(), "abortion-regs-by-state.csv")
write.csv(regs, regs_path, row.names = FALSE)

project_data <- osf_retrieve_node("pcaj6")
osf_upload(project_data, regs_path, conflicts = "overwrite")