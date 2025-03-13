# libraries ---------------------------------------------------------------


library(pacman)
p_load(
  broom,
  ggtext,
  scales,
  lmerTest,
  broom.mixed,
  kableExtra,
  jtools, 
  here,
  survey,
  ggeffects,
  emmeans,
  MASS,
  rsample,
  tidyverse,
  conflicted
)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lmer", "lmerTest")

`%nin%` <- Negate(`%in%`)


# 0.  Regression functions --------------------------------------------


# mdt = melodic discrimination
# mpt = mistuning perception 
# cabat = beat alignment 

calculate_marginals <- function(data_x, test_x) {
  # 1. calculate marginal effects
  output <- ggemmeans(data_x, terms = c("Tone", "musicLessons")) %>% tibble %>% 
    mutate(test = test_x)
  
  # 2. apply scalar transform for interpretability
  scalar_adjust <- output %>% 
    filter(x == "Non-tonal", group == "No") %>% 
    pull(predicted)
  
  output <- output %>% 
    mutate(across(where(is.numeric), ~ .x - scalar_adjust))
  
  return(output)
}

compute_models <- function(data, pred_var, model_type, expression, marginal = FALSE, weighted_data) {
  output <- list()
  formular <- reformulate(termlabels = expression, pred_var)
  # multiple regression
  if (model_type == "lm") { mod <- lm(formular, data = data) } 
  # mixed-effect regression
  else if (model_type == "lmer") { mod <- lmer(formular, data = data) } 
  # weighted regression
  else if (model_type == "ipw") {
    design <- svydesign(ids = ~1, weights = weighted_data$weights, data = data)
    mod <- svyglm(formular, design = design, family = "gaussian")
  }
  # tidy output
  output$t <- mod %>% tidy(conf.int = T, conf.level = .95)
  
  # calculate marginal effect
  if (marginal == TRUE) {
    output$marginal <- calculate_marginals(mod, pred_var)
  }
  
  return(output)
}

# 1.  Load data -----------------------------------------------------------


miq_confirm_filtered <- read.csv(here("data", "Confirmatory", "Confirm_filtered.csv"))
miq_confirm_matched <- read.csv(here("data", "Confirmatory","Confirm_matched.csv"))
miq_confirm <- read.csv(here("data", "Confirmatory", "Confirm_full.csv")) 

miq_explore_filtered <- read.csv(here("data", "Exploratory", "Explore_filtered.csv"))
miq_explore_matched <- read.csv(here("data", "Exploratory", "Explore_matched.csv"))
miq_explore <- read.csv(here("data", "Exploratory", "Explore_full.csv"))

miq_combined_filtered <- read.csv(here("data", "Combined", "Combined_filtered.csv"))
miq_combined <- bind_rows(miq_explore, miq_confirm)

languages <- read.csv(here("data", "language.csv"))
headphone_score <- read.csv(here("data", "headphone_score.csv"))

load(here("data", "Confirmatory", "ipw_confirm.RData"))
load(here("data", "Exploratory", "ipw_explore.RData"))


users <- list()
users$starts <- 3519530 # hard coded due to large data file; need to be updated
users$explore <- nrow(miq_explore_filtered)
users$confirm <- nrow(miq_confirm_filtered)
users$total <- nrow(miq_explore) + nrow(miq_confirm)
users$final <- nrow(miq_combined_filtered)
users$speakers <- map(c("Tonal", "Non_tonal", "Pitch_accented") %>% set_names, ~ {
  .x <- str_replace(.x, "_", "-")
  miq_combined_filtered |> 
    filter(Tone == .x) |> 
    nrow()
})
users$speakers$other <- users$speakers$Non_tonal + users$speakers$Pitch_accented
users$langs <- map(c("Tonal", "Non_tonal", "Pitch_accented") %>% set_names, ~ {
  .x <- str_replace(.x, "_", "-")
  miq_combined_filtered |> 
    filter(Tone == .x) |> 
    with(n_distinct(language))
})

# 2.  Compute Exclusion Numbers -----------------------------------------------

compute_exclusions <- function(data) {
  exclude <- partial(if_else, true = 1, false = 0)
  data %>%
    # 1. mark each participant for exclusion criteria
    mutate( 
      ex_age           = exclude(age < 8 | age > 90),
      ex_hear          = exclude(hearingImp != "No"),
      ex_take          = exclude(takenBefore == "Yes"),
      ex_lessonAge     = exclude(musicLessons == "Yes" & (lessonsAge < 2 | lessonsAge > 90)),
      ex_work          = exclude(headphone == "No" & workspace == "I am in a very noisy place"),
      ex_age_consist   = exclude(musicLessons == "Yes" & (age < lessonsAge)),
      ex_language      = exclude(language %nin% unique(miq_combined_filtered$language)),
      exclude_sum      = ex_age + ex_hear + ex_take + ex_lessonAge + ex_work + ex_age_consist,
      ex_more_than_one = if_else(exclude_sum > 1, 1, 0)
    ) %>%
    # 2. tally things up
    summarise(across(starts_with("ex_"), ~ sum(.x, na.rm = TRUE), .names = "{.col}"))
}

excludes <- map(list(miq_confirm, miq_explore, miq_combined) |>
                  set_names(c("confirm", "explore", "full")), compute_exclusions)

excludes$full$lang <- miq_combined %>%
  filter(language %nin% unique(miq_combined_filtered$language)) %>%
  count(language) %>%
  summarise(
    n_lang = n(),
    median = median(n)
  )

# 3. Regression Models ---------

map_miq <- partial(map, .x = c("mdt", "mpt", "cabat") %>% set_names)

mods <- list()

## 3.1 Main mixed-effect model (Manuscript) ------

mods$total_mix_full <- map_miq(~ compute_models(miq_combined_filtered, .x, "lmer", "Tone*musicLessons + age + gender + (1|country) + (1|language)", TRUE))

### 3.1.1 Combined data language estimates-----

extract_language_estimates <- function(mod_x) {
  fixed <- mod_x %>% tidy() %>% 
    filter(effect == "fixed",
           term %in% c("ToneNon-tonal", "TonePitch-accented", "ToneTonal")) %>% 
    select(term, fixed_effect = estimate) %>%
    mutate(Tone = c("Non-tonal", "Pitch-accented", "Tonal"))
  
  # this adjusts the fixed-effect estimates so that non-tonal is a reference of 0
  fixed <- fixed %>%
    mutate(adj_fixed = fixed_effect - fixed %>% filter(Tone == "Non-tonal") %>% pull(fixed_effect))
  
  random <- tidy(mod_x, effects = "ran_vals", conf.int = TRUE, conf.level = 0.95) %>%
    filter(group == "language") %>%
    left_join(., languages %>% select(language, Tone), by = c("level" = "language")) %>%
    mutate(Tone = ifelse(is.na(Tone), "Tonal", Tone))
  
  combined <- left_join(random, fixed %>% select(-term), by = "Tone") %>%
    mutate(
      across(c(estimate, conf.low, conf.high), ~ .x + adj_fixed),
      language = fct_reorder(level, estimate)
    )
  return(combined)
}

run_mod <- function(term) {
  x <- reformulate("0 + Tone*musicLessons + age + gender + (1|country) + (1|language)", response = term)
  mod <- lmer(x, miq_combined_filtered)
  output <- extract_language_estimates(mod)
  return(output)
}

mods$languages <- map_miq(~ run_mod(.x))


## extract some stats about the random-effect estimates

mods$languages$stats$mdt <- mods$languages$mdt |> 
  filter(Tone == "Tonal") |> 
  mutate(sig = ifelse(conf.low > 0, 1, 0)) |> 
  summarise(
    sig = sum(sig),
    n = n()
  )

mods$languages$stats$cabat <- mods$languages$cabat |> 
  filter(Tone == "Tonal") |> 
  mutate(sig = ifelse(conf.high < 0, 1, 0)) |> 
  summarise(
    sig = sum(sig),
    n = n()
  )

## 3.2 Exploratory-confirmatory models (SI)------

### 3.2.1 Confirmatory models ----

# Total/main analysis with random effects for language and country and interaction 
mods$confirm$total_mixed <- map_miq(~ compute_models(miq_confirm_filtered, .x, "lmer", "Tone*musicLessons + age + gender + (1|country) + (1|language)", TRUE))

# Total Simple multiple regression
mods$confirm$total <- map_miq(~ compute_models(miq_confirm_filtered, .x, "lm", "Tone + musicLessons + age + gender", TRUE))
# Simple multiple regressions EXCLUDING participants with music lessons
mods$confirm$noLesson <- map_miq(~ compute_models(miq_confirm_filtered %>% filter(musicLessons == "No"), .x, "lm", "Tone + age + gender"))
# Mixed effects EXCLUDING participants with music lessons
mods$confirm$noLesson_mixed <- map_miq(~ compute_models(miq_confirm_filtered %>% filter(musicLessons == "No"), .x, "lmer", "Tone + age + gender+ (1|country) + (1|language)"))

# Simple regression on matched data
mods$confirm$matched <- map_miq(~ compute_models(miq_confirm_matched, .x, "lm", "Tone"))
# Mixed-effects on matched data
mods$confirm$matched_mixed <- map_miq(~ compute_models(miq_confirm_matched, .x, "lmer", "Tone + (1|language) + (1|country)"))

# simple regression on matched data EXCLUDING participants with music lessons
mods$confirm$matched_noLessons <- map_miq(~ compute_models(miq_confirm_matched %>% filter(musicLessons == "No"), .x, "lm", "Tone"))
# mixed-effects on matched data EXCLUDING participants with music lessons
mods$confirm$matched_noLessons_mixed <- map_miq(~ compute_models(miq_confirm_matched %>% filter(musicLessons == "No"),
                                                         .x, "lmer", "Tone + (1|country) + (1|language)"))
# Simple weighted regression using IPW weights
mods$confirm$ipw <- map_miq(~ compute_models(miq_confirm_filtered, .x, "ipw", "Tone", weighted_data = data_weighted_confirm))

### 3.2.2 Exploratory models ----------------------------------------------

load(here("data", "Exploratory", "ipw_explore.RData"))

# Replicating analyses with the exploratory only data
# simple multiple regressions & mixed-effects
mods$ex$total <- map_miq(~ compute_models(miq_explore_filtered, .x, "lm", "Tone + musicLessons + age + gender"))
mods$ex$total_mixed <- map_miq(~ compute_models(miq_explore_filtered, .x, "lmer", "Tone + musicLessons + age + gender + (1|country) + (1|language)"))

# simple multiple regressions and mixed-effects for matched participants
mods$ex$matched <- map_miq(~ compute_models(miq_explore_matched, .x, "lm", "Tone"))
mods$ex$matched_mixed <- map_miq(~ compute_models(miq_explore_matched, .x, "lmer", "Tone + (1|language) + (1|country)"))

# simple regression and mixed effects on matched data EXCLUDING participants with music lessons
mods$ex$matched_noLessons <- map_miq(~ compute_models(miq_explore_matched %>% filter(musicLessons == "No"), .x, "lm", "Tone"))
mods$ex$matched_noLessons_mixed <- map_miq(~ compute_models(miq_explore_matched %>% filter(musicLessons == "No"),
                                                            .x, "lmer", "Tone + (1|language) + (1|country)"))

# Simple weighted regression using IPW weights
mods$ex$ipw <- map_miq(~ compute_models(miq_explore_filtered, .x, "ipw", "Tone", weighted_data = data_weighted_explore))

# 4. Table 2 main analysis --------------------------------------------------------------

## This was the function used to generate table in the first submission (i.e. linear regression)
prepare_table <- function(data, drop_term = FALSE) {
  x <- data %>% 
    mutate(statistic = case_when(
      p.value < 0.001 ~ paste0(round(statistic, 3), "***"),
      p.value < 0.01 ~ paste0(round(statistic, 3), "**"),
      p.value < 0.05 ~ paste0(round(statistic, 3), "*"),
      TRUE ~ as.character(round(statistic,3))
    )) %>% 
    dplyr::select(term, estimate, std.error, statistic)
  
  if(drop_term) {x <- x %>% select(-term)}
  
  return(x)
}

## Current Table 2

### 4.1 Table 2 output----

Table2 <- bind_cols(
  prepare_table(mods$total_mix_full$mdt$t |> filter(effect == "fixed")),
  prepare_table(mods$total_mix_full$mpt$t |> filter(effect == "fixed"), TRUE),
  prepare_table(mods$total_mix_full$cabat$t |> filter(effect == "fixed"), TRUE)
) %>%
  mutate(
    # format numbers properly
    across(where(is.numeric), ~ round(.x, digits = 3)),
    #fix names
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "TonePitch-accented" ~ "Language: Pitch-accented", # %>% cell_spec(., bold = T, format = "latex") bolded
      term == "ToneTonal" ~ "Language: Tonal",
      term == "musicLessonsYes" ~ "Music lessons: Yes",
      term == "age" ~ "Age",
      term == "genderMale" ~ "Gender: Male",
      term == "genderOther" ~ "Gender: Other",
      term == "TonePitch-accented:musicLessonsYes" ~ "Pitch-accented\\ \\times \\ music lessons",
      term == "ToneTonal:musicLessonsYes" ~ "Tonal\\ \\times \\ music lessons",
      TRUE ~ term
    )
  )

# re-ordering rows so that 'Tonal' is on top
Table2 <- Table2[c(3,2,4:7, 9, 8, 1),]

### 4.2 Extract statistics ----

format_estimate <- function(entry) {
  return(list(beta = trimws(entry[2]), t = trimws(entry[3]), p = trimws(entry[4])))
}

extract_estimate <- function(mod) {
  mod_fixed <- mod |> filter(effect == "fixed") %>% 
    mutate(p.value = case_when(
      p.value < 0.001 ~ "< 0.001",
      p.value < 0.01 ~ "< 0.01",
      p.value < 0.05 ~ "< 0.05",
      TRUE ~ as.character(round(p.value,3))
    )) %>% 
    mutate(estimate = round(estimate, 3),
           statistic = round(statistic, 3)) %>%
    select(term, estimate, statistic, p.value)
    
  results <- apply(mod_fixed, 1, format_estimate)
  
  list = list()
  
  for (idx in c(1:length(results))){
    
    list = append(list, results[idx])
    }
  
  names(list) <- c(mod_fixed$term)
  return(list)
}

main_analysis_estimates = list()
main_analysis_estimates$mdt <- extract_estimate(mods$total_mix_full$mdt$t)
main_analysis_estimates$mpt <- extract_estimate(mods$total_mix_full$mpt$t)
main_analysis_estimates$cabat <- extract_estimate(mods$total_mix_full$cabat$t)

# 6. Fig data preprocessing -------------------------------------------------

## Fig 3 processing --------------------------------------------------------


# function to extract random-effect estimates for languages
extract_language_estimates <- function(mod_x) {
  fixed <- mod_x %>% tidy() %>% 
    filter(effect == "fixed",
           term %in% c("(Intercept)", "TonePitch-accented", "ToneTonal")) %>% 
    select(term, fixed_effect = estimate) %>%
    mutate(Tone = c("Non-tonal", "Pitch-accented", "Tonal"))
  
  exploratory <- mod_x %>% tidy() %>% filter(effect == "fixed", term == "dataexploratory") %>% pull(estimate)
  
  random <- tidy(mod_x, effects = "ran_vals") %>%
    filter(group == "language:data") %>%
    mutate(
      language = str_extract(level, ".*(?=:)"),
      data = str_extract(level, "(?<=:).*")
    ) %>%
    left_join(., languages %>% select(language, Tone), by = "language")
  
  combined <- left_join(random, fixed %>% select(-term), by = "Tone") %>%
    mutate(
      estimate = ifelse(data == "exploratory", 
                        estimate + fixed_effect + exploratory,
                        estimate + fixed_effect),
      language = fct_reorder(language, estimate)
    )
  return(combined)
}

run_mod <- function(data, term) {
  x <- reformulate("Tone*musicLessons + age + gender + data + (1|country) + (1|language:data)", response = term)
  mod <- lmer(x, data)
  output <- extract_language_estimates(mod)
  return(output)
}

fig3_data <- map_dfr(c("mdt", "mpt", "cabat"), ~ {
  run_mod(bind_rows(
    miq_explore_filtered |> mutate(data = "exploratory"),
    miq_confirm_filtered |> mutate(data = "confirmatory")
  ), .x) %>%
    select(language, estimate, Tone, data) |> mutate(test = .x) %>% 
    pivot_wider(names_from = data, values_from = estimate) %>%
    mutate(Tone = factor(Tone, levels = c("Non-tonal","Pitch-accented", "Tonal")))
}) 

## Figure 4 processing-----
bubble_data <- full_join(miq_confirm_filtered %>%
                           count(language) %>%
                           mutate(language = case_when(
                             language == "Chinese/Mandarin" ~ "Mandarin",
                             language == "Chinese/Cantonese/Yue" ~ "Cantonese",
                             language == "Chinese/Taiwanese/Min Nan/Hokkien" ~ "Hokkien",
                             TRUE ~ language
                           )) %>%
                           select(language, confirmatory = n),
                         miq_explore_filtered %>%
                           count(language) %>%
                           mutate(language = case_when(
                             language == "Chinese/Mandarin" ~ "Mandarin",
                             language == "Chinese/Cantonese/Yue" ~ "Cantonese",
                             language == "Chinese/Taiwanese/Min Nan/Hokkien" ~ "Hokkien",
                             TRUE ~ language
                           )) %>%
                           select(language, exploratory = n), by = "language")

bubble_data <- full_join(
  bubble_data,
  miq_confirm_filtered %>%
    mutate(language = case_when(
      language == "Chinese/Mandarin" ~ "Mandarin",
      language == "Chinese/Cantonese/Yue" ~ "Cantonese",
      language == "Chinese/Taiwanese/Min Nan/Hokkien" ~ "Hokkien",
      TRUE ~ language
    )) %>%
    select(language, Tone) %>%
    distinct(.), by = "language") 

confirm_plot_data <- full_join(miq_confirm_filtered %>%
                                 mutate(language = case_when(
                                   language == "Chinese/Mandarin" ~ "Mandarin",
                                   language == "Chinese/Cantonese/Yue" ~ "Cantonese",
                                   language == "Chinese/Taiwanese/Min Nan/Hokkien" ~ "Hokkien",
                                   TRUE ~ language
                                 )), bubble_data, by = c("language", "Tone"))
confirm_plot_data <- confirm_plot_data %>%
  mutate(lang_num = paste0(language, " (", crayon::italic("n"), "=", confirmatory, ")"))  


# 5.  Exploratory/Confirmatory Correlations -------------------------------------------


# Exploratory/Confirmatory Correlations
cors <- map(c("mdt", "mpt", "cabat") |> set_names(), ~ {
  filter(fig3_data, test == .x) |> with(cor.test(exploratory, confirmatory, test = "Pearson"))
})

# Wilcox tests
wilcox <- map(c("Pitch_accent", "Tonal", "Non-tonal") |> set_names("T.NT", "PA.NT", "T.PA"), \(.x)
              map(c("mdt", "mpt", "cabat") |> set_names(), \(.y) {
                temp <- filter(fig3_data, test == .y, Tone != .x) |>
                  pivot_wider(names_from = Tone, values_from = confirmatory)
                wilcox.test(temp[[5]], temp[[4]], alternative = "two.sided")
              })
)


# 7. SI tables ------

df_clean <- function (x){ 
  x <- x %>%
    # tidy(., conf.int = T, conf.level = .95) %>%
    mutate(sign = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.05 ~ "* ",
      TRUE ~ "  "
    )) %>%
    mutate(coef = paste0(round(estimate, 3), sign))
  return (x)
}

pull_tonal_coef <- function(analysis, test, analysis_text, test_text, coef_only = FALSE) {
  x <- df_clean(mods[[analysis]][[test]]$t) %>%
    filter(term %in% c("ToneTonal", "TonePitch-accented")) %>%
    select(term, coef) %>% 
    rename({{test_text}} := coef, Term = term) %>% 
    bind_rows(tibble(Term = analysis_text, {{test_text}} := ""), .)
  
  if (coef_only) {x <- x %>% select({{test_text}})}
  
  return(x)
}

## SI Table 2 Confirmatory mixed and linear models ---------------------------------------------------

analyses_to_include <- c("total", "matched", "matched_noLessons", "ipw")
analyses_labels <- c("Main Analysis", "Matched", "Matched (No Lessons Only)", "Inverse Probability Weighted") %>% cell_spec(., bold = T, format = "latex")
analyses_to_include_mixed <- c("total_mixed", "matched_mixed", "matched_noLessons_mixed", "ipw")

### Confirmatory Tables S2A & S2B -----
confirm_analyses_to_include <- map(analyses_to_include, ~ c("confirm", .x))

TableS2B <- bind_cols(
  # Melodic Discrimination
  map2_df(confirm_analyses_to_include, analyses_labels, ~ pull_tonal_coef(.x, "mdt", .y, "Melodic Discrimination")),
  # Mistuning Perception
  map2_df(confirm_analyses_to_include, analyses_labels, ~ pull_tonal_coef(.x, "mpt", .y, "Mistuning Perception", TRUE)),
  # Beat Alignment
  map2_df(confirm_analyses_to_include, analyses_labels, ~ pull_tonal_coef(.x, "cabat", .y, "Beat Alignment", TRUE))
) %>% 
  # clean up Term text
  mutate(Term = case_when(
    Term == "TonePitch-accented" ~ "Language: Pitch-accented",
    Term == "ToneTonal" ~ "Language: Tonal",
    TRUE ~ Term
  ))

## ipw still linear regression for now! 

confirm_analyses_to_include_mixed <- map(analyses_to_include_mixed, ~ c("confirm", .x))

TableS2A <- bind_cols(
  # Melodic Discrimination
  map2_df(confirm_analyses_to_include_mixed, analyses_labels, ~ pull_tonal_coef(.x, "mdt", .y, "Melodic Discrimination")),
  # Mistuning Perception
  map2_df(confirm_analyses_to_include_mixed, analyses_labels, ~ pull_tonal_coef(.x, "mpt", .y, "Mistuning Perception", TRUE)),
  # Beat Alignment
  map2_df(confirm_analyses_to_include_mixed, analyses_labels, ~ pull_tonal_coef(.x, "cabat", .y, "Beat Alignment", TRUE))
) %>% 
  # clean up Term text
  mutate(Term = case_when(
    Term == "TonePitch-accented" ~ "Language: Pitch-accented",
    Term == "ToneTonal" ~ "Language: Tonal",
    TRUE ~ Term
  ))

### Exploratory Tables 5A and 5B  ------

ex_analyses_to_include <- map(analyses_to_include, ~ c("ex", .x))

TableS5B <- bind_cols(
  # Melodic Discrimination
  map2_df(ex_analyses_to_include, analyses_labels, ~ pull_tonal_coef(.x, "mdt", .y, "Melodic Discrimination")),
  # Mistuning Perception
  map2_df(ex_analyses_to_include, analyses_labels, ~ pull_tonal_coef(.x, "mpt", .y, "Mistuning Perception", TRUE)),
  # Beat Alignment
  map2_df(ex_analyses_to_include, analyses_labels, ~ pull_tonal_coef(.x, "cabat", .y, "Beat Alignment", TRUE))
) %>% 
  # clean up Term text
  mutate(Term = case_when(
    Term == "TonePitch-accented" ~ "Language: Pitch-accented",
    Term == "ToneTonal" ~ "Language: Tonal",
    TRUE ~ Term
  ))

ex_analyses_to_include_mixed <- map(analyses_to_include_mixed, ~ c("ex", .x))

TableS5A <- bind_cols( ## ipw still linear regression!! 
  # Melodic Discrimination
  map2_df(ex_analyses_to_include_mixed, analyses_labels, ~ pull_tonal_coef(.x, "mdt", .y, "Melodic Discrimination")),
  # Mistuning Perception
  map2_df(ex_analyses_to_include_mixed, analyses_labels, ~ pull_tonal_coef(.x, "mpt", .y, "Mistuning Perception", TRUE)),
  # Beat Alignment
  map2_df(ex_analyses_to_include_mixed, analyses_labels, ~ pull_tonal_coef(.x, "cabat", .y, "Beat Alignment", TRUE))
) %>% 
  # clean up Term text
  mutate(Term = case_when(
    Term == "TonePitch-accented" ~ "Language: Pitch-accented",
    Term == "ToneTonal" ~ "Language: Tonal",
    TRUE ~ Term
  ))

## Table S4B demographics --------------------------------------------------------

Demo <- c("Gender", "Female", "Male", "Other", "Mean age (SD)", "Music lesson", "Yes", "No", "Mean age of onset of music lessons (SD)") 

# Gender:
ex.Gender <- miq_explore_filtered %>%
  group_by(Tone)  %>%
  count(gender) %>%
  mutate(prop = n/sum(n))

ex.T.female <- ex.Gender[ex.Gender$Tone == "Tonal" & ex.Gender$gender == "Female",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.T.male <- ex.Gender[ex.Gender$Tone == "Tonal" & ex.Gender$gender == "Male",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.T.other <- ex.Gender[ex.Gender$Tone == "Tonal" & ex.Gender$gender == "Other",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.NT.female <- ex.Gender[ex.Gender$Tone == "Non-tonal" & ex.Gender$gender == "Female",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.NT.male <- ex.Gender[ex.Gender$Tone == "Non-tonal" & ex.Gender$gender == "Male",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.NT.other <- ex.Gender[ex.Gender$Tone == "Non-tonal" & ex.Gender$gender == "Other",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.PA.female <- ex.Gender[ex.Gender$Tone == "Pitch-accented" & ex.Gender$gender == "Female",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.PA.male <- ex.Gender[ex.Gender$Tone == "Pitch-accented" & ex.Gender$gender == "Male",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.PA.other <- ex.Gender[ex.Gender$Tone == "Pitch-accented" & ex.Gender$gender == "Other",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)

# Music Lessons
ex.Lessons <- miq_explore_filtered %>%
  group_by(Tone)  %>%
  count(musicLessons) %>%
  mutate(prop = round(n/sum(n),2))

ex.T.lessonNo <- ex.Lessons[ex.Lessons$Tone == "Tonal" & ex.Lessons$musicLessons == "No",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.T.lessonYes <- ex.Lessons[ex.Lessons$Tone == "Tonal" & ex.Lessons$musicLessons == "Yes",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.NT.lessonNo <- ex.Lessons[ex.Lessons$Tone == "Non-tonal" & ex.Lessons$musicLessons == "No",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.NT.lessonYes <- ex.Lessons[ex.Lessons$Tone == "Non-tonal" & ex.Lessons$musicLessons == "Yes",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.PA.lessonNo <- ex.Lessons[ex.Lessons$Tone == "Pitch-accented" & ex.Lessons$musicLessons == "No",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)
ex.PA.lessonYes <- ex.Lessons[ex.Lessons$Tone == "Pitch-accented" & ex.Lessons$musicLessons == "Yes",]$prop %>% percent(suffix = "\\%", accuracy = 0.1)

# Age
ex.Age <- miq_explore_filtered %>%
  group_by(Tone) %>%
  summarise(mean = round(mean(age),2), sd = round(sd(age),2)) %>%
  mutate(age = paste0(mean, " (", sd, ")"))

ex.T.age <- ex.Age[ex.Age$Tone == "Tonal",]$age
ex.NT.age <- ex.Age[ex.Age$Tone == "Non-tonal",]$age
ex.PA.age <- ex.Age[ex.Age$Tone == "Pitch-accented",]$age

# Lesson Age
ex.lesson.Age <- miq_explore_filtered %>%
  filter(!is.na(lessonsAge)) %>%
  group_by(Tone) %>%
  summarise(mean = round(mean(lessonsAge),2),
            sd = round(sd(lessonsAge),2))
ex.NT.lessonAge <- paste0(ex.lesson.Age[1,2], " (", ex.lesson.Age[1,3], ")")
ex.T.lessonAge <- paste0(ex.lesson.Age[3,2], " (", ex.lesson.Age[3,3], ")")
ex.PA.lessonAge <- paste0(ex.lesson.Age[2,2], " (", ex.lesson.Age[2,3], ")")

ex.Tone.demo <- c("", ex.T.female, ex.T.male, ex.T.other, ex.T.age, "", ex.T.lessonYes, ex.T.lessonNo, ex.T.lessonAge)
ex.NT.demo <- c("", ex.NT.female, ex.NT.male, ex.NT.other, ex.NT.age, "", ex.NT.lessonYes, ex.NT.lessonNo, ex.NT.lessonAge)
ex.PA.demo <- c("", ex.PA.female, ex.PA.male, ex.PA.other, ex.PA.age, "", ex.PA.lessonYes, ex.PA.lessonNo, ex.PA.lessonAge)

TableS5 <- cbind(Demo, ex.Tone.demo, ex.NT.demo, ex.PA.demo)
TableS5[1,1] <- cell_spec(TableS5[1,1], bold = T)
TableS5[5,1] <- cell_spec(TableS5[5,1], bold = T)
TableS5[6,1] <- cell_spec(TableS5[6,1], bold = T)
TableS5[9,1] <- cell_spec(TableS5[9,1], bold = T)

Ex.lang.num <- miq_explore_filtered %>% # add sample numbers
  count(Tone)

colnames(TableS5) <- c("Demographics", paste0("Tonal ($n$ = ", Ex.lang.num[3,2] %>% format(., big.mark = ","),")"),
                       paste0("Non-tonal ($n$ = ", Ex.lang.num[1,2] %>% format(., big.mark = ","),")"),
                       paste0("Pitch-accented ($n$ = ", Ex.lang.num[2,2] %>% format(., big.mark = ","),")"))


## Table 3 Controlling for additional confounds -----

### Education ---- 
miq_education <- miq_combined_filtered %>%
  mutate(education = case_when(
    education %in% c("Completed elementary/middle school<br>(primary school)", "Some elementary/middle school<br>(primary school)") ~ "Primary school",
    education %in% c("Completed high school<br>(secondary school)",
                     "Some high school") ~ "Secondary school",
    education %in% c("Some undergrad<br>(higher education)",
                     "Completed undergrad degree<br>(~3-5 years higher education)") ~ "Undergraduate",
    education %in% c("Some graduate school", "Completed graduate school") ~ "Graduate school"
  )) %>%
  filter(!is.na(education))

education_n <- nrow(miq_education)

miq_education$education <- factor(miq_education$education, 
                                  levels = c("Primary school",
                                             "Secondary school",
                                             "Undergraduate",
                                             "Graduate school"))

mods$combined_education <- map_miq(~ compute_models(miq_education, .x, "lmer", "Tone * musicLessons + age + gender + education + (1|country) + (1|language)", TRUE))

Table_combined_education <- bind_cols(
  prepare_table(mods$combined_education$mdt$t[mods$combined_education$mdt$t$effect == "fixed",]),
  prepare_table(mods$combined_education$mpt$t[mods$combined_education$mpt$t$effect == "fixed",], TRUE),
  prepare_table(mods$combined_education$cabat$t[mods$combined_education$cabat$t$effect == "fixed",], TRUE)
) %>%
  mutate(
    # format numbers properly
    across(where(is.numeric), ~ round(.x, digits = 3)),
    #fix names
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "TonePitch-accented" ~ "Language: Pitch-accented", # bolded
      term == "ToneTonal" ~ "Language: Tonal", # bolded
      term == "musicLessonsYes" ~ "Music lessons: Yes",
      term == "age" ~ "Age",
      term == "genderMale" ~ "Gender: Male",
      term == "genderOther" ~ "Gender: Other",
      term == "educationSecondary school" ~ "Education: Secondary School",
      term == "educationUndergraduate" ~ "Education: Undergraduate",
      term == "educationGraduate school" ~ "Education: Graduate School",
      term == "TonePitch-accented:musicLessonsYes" ~ "Pitch-accented\\ \\times \\ music lessons",
      term == "ToneTonal:musicLessonsYes" ~ "Tonal\\ \\times \\ music lessons",
      TRUE ~ term
    )
  )

Table_combined_education <- Table_combined_education[c(3,2,4:12, 1),]

# education_estimates = list()
# education_estimates$mdt <- extract_estimate(mods$combined_education$mdt$t)
# education_estimates$mpt <- extract_estimate(mods$combined_education$mpt$t)
# education_estimates$cabat <- extract_estimate(mods$combined_education$cabat$t)

### Income----- 
miq_income <- miq_combined_filtered %>%
  filter(
    !is.na(income),
    income != "I'd prefer not to say",
    country == "United States"
    )

income_n <- nrow(miq_income)
income_total_n <- miq_combined_filtered %>%
  filter(country == "United States") %>%
  nrow()

# add sample size info about income subsample
users$income <- miq_income |>
  mutate(Tone = str_replace_all(Tone, "-", "_")) |> 
  group_by(Tone) |> 
  count(language) |> 
  summarise(
    n = sum(n),
    n_langs = n()
  ) |> 
  split(~ Tone)

miq_income$income <- factor(miq_income$income, 
                            levels = c("Under $10,000", "$10,000 to $19,999",
                                       "$20,000 to $29,999", "$30,000 to $39,999",
                                       "$40,000 to $49,999", "$50,000 to $74,999",
                                       "$75,000 to $99,999", "$100,000 to $150,000",
                                       "Over $150,000"))

mods$combined_income <- map_miq(~ compute_models(miq_income, .x, "lmer", "Tone * musicLessons + age + gender + income + (1|language)", TRUE))

Table_combined_income <- bind_cols(
  prepare_table(mods$combined_income$mdt$t[mods$combined_income$mdt$t$effect == "fixed",]),
  prepare_table(mods$combined_income$mpt$t[mods$combined_income$mpt$t$effect == "fixed",], TRUE),
  prepare_table(mods$combined_income$cabat$t[mods$combined_income$cabat$t$effect == "fixed",], TRUE)
) %>%
  mutate(
    # format numbers properly
    across(where(is.numeric), ~ round(.x, digits = 3)),
    #fix names
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "TonePitch-accented" ~ "Language: Pitch-accented", 
      term == "ToneTonal" ~ "Language: Tonal", 
      term == "musicLessonsYes" ~ "Music lessons: Yes",
      term == "age" ~ "Age",
      term == "genderMale" ~ "Gender: Male",
      term == "genderOther" ~ "Gender: Other",
      term == "TonePitch-accented:musicLessonsYes" ~ "Pitch-accented\\ \\times \\ music lessons",
      term == "ToneTonal:musicLessonsYes" ~ "Tonal\\ \\times \\ music lessons",
      term == "income$10,000 to $19,999" ~ "Income: \\$10,000 to \\$19,999",
      term == "income$20,000 to $29,999" ~ "Income: \\$20,000 to \\$29,999",
      term == "income$30,000 to $39,999" ~ "Income: \\$30,000 to \\$39,999",
      term == "income$40,000 to $49,999" ~ "Income: \\$40,000 to \\$49,999",
      term == "income$50,000 to $74,999" ~ "Income: \\$50,000 to \\$74,999",
      term == "income$75,000 to $99,999" ~ "Income: \\$75,000 to \\$99,999",
      term == "income$100,000 to $150,000" ~ "Income: \\$100,000 to \\$150,000",
      term == "incomeOver $150,000" ~ "Income: Over \\$150,000",
      TRUE ~ term
    )
  )
  
Table_combined_income <- Table_combined_income[c(3,2,4:17, 1),]

# income_estimates = list()
# income_estimates$mdt <- extract_estimate(mods$combined_income$mdt$t)
# income_estimates$mpt <- extract_estimate(mods$combined_income$mpt$t)
# income_estimates$cabat <- extract_estimate(mods$combined_income$cabat$t)

### culture -------

miq_culture <- miq_combined_filtered %>%
  filter(country %in% c("China", "Hong Kong", "Taiwan", "Thailand", "Vietnam",
                        "Canada", "United States", "Australia", "New Zealand", "United Kingdom")) %>%
  mutate(Region = ifelse(country %in% c("China", "Hong Kong", "Taiwan", "Thailand", "Vietnam"), "East", "West"))

miq_culture <- miq_culture %>%
  filter(Tone != "Pitch-accented")

culture_n <- nrow(miq_culture)

mods$combined_culture <- map_miq(~ compute_models(miq_culture, .x, "lmer", "Tone * musicLessons + age + gender + Region + (1|language) + (1|country)", TRUE))

Table_combined_culture <- bind_cols(
  prepare_table(mods$combined_culture$mdt$t[mods$combined_culture$mdt$t$effect == "fixed",]),
  prepare_table(mods$combined_culture$mpt$t[mods$combined_culture$mdt$t$effect == "fixed",],TRUE),
  prepare_table(mods$combined_culture$cabat$t[mods$combined_culture$mdt$t$effect == "fixed",],TRUE)
) %>%
  mutate(
    # format numbers properly
    across(where(is.numeric), ~ round(.x, digits = 3)),
    #fix names
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "ToneTonal" ~ "Language: Tonal", 
      term == "musicLessonsYes" ~ "Music lessons: Yes",
      term == "age" ~ "Age",
      term == "genderMale" ~ "Gender: Male",
      term == "genderOther" ~ "Gender: Other",
      term == "TonePitch-accented:musicLessonsYes" ~ "Pitch-accented\\ \\times \\ music lessons",
      term == "ToneTonal:musicLessonsYes" ~ "Tonal\\ \\times \\ music lessons",
      term == "RegionWest" ~ "Region: West",
      TRUE ~ term
    )
  )

Table_combined_culture <- Table_combined_culture[c(2:8, 1),]

# culture_estimates = list()
# culture_estimates$mdt <- extract_estimate(mods$combined_culture$mdt$t)
# culture_estimates$mpt <- extract_estimate(mods$combined_culture$mpt$t)
# culture_estimates$cabat <- extract_estimate(mods$combined_culture$cabat$t)

# 9. permutation analyses -------------------------------------------------


#################################################
# 1. Define functions for running pDFA analysis #
#################################################

sample_data <- function(data, n_langs, n_participants) {
  # return the two language types to compare between
  comparison <- unique(data$Tone) |> str_replace("-", "_")
  
  # sample {n_langs} tonal and non-tonal languages
  sampled_languages <- c(sample(langs[[ comparison[[1]] ]], n_langs), sample(langs[[comparison[[2]]]], n_langs))
  dat <- data |> filter(language %in% sampled_languages)
  
  # sample {n_participants} of participants for each language
  map_dfr(sampled_languages, \(.lang) {
    dat |> 
      filter(language == .lang) |> 
      slice_sample(n = n_participants, replace = TRUE)
  })
}

classification_accuracy <- function(sampled_data, test) {
  # create training dataset with 100 participants per language
  split <- initial_split(sampled_data, prop = .30, strata = "language")
  train <- training(split)
  testing <- testing(split)
  
  # run LDA 
  x <- reformulate(termlabels = test, response = "Tone")
  mod <- lda(x, train)
  
  tibble(
    truth = testing$Tone,
    prediction = predict(mod, testing)$class
  ) |> 
    mutate(accurate = ifelse(truth == prediction, 1, 0)) |> 
    summarise(prop = mean(accurate)) |> 
    pull(prop)
}

compute_p <- function(null, actual) {
  out <- sum(null > actual) / sum(null < actual)
  round(out, 3)
}

permutation_test <- function(data, test, permutations) {
  null_dist <- map_dbl(1:permutations, \(.i) {
    # sample data
    sampled_data <- sample_data(data, 5, n_participants = 100)
    # shuffle Tone labels
    sampled_data$Tone <- sample(sampled_data$Tone)
    # compute discrimination performance
    classification_accuracy(sampled_data, test)
  })
  
  avg_prop <- map_dbl(1:100, \(.i) {
    # sample data
    sampled_data <- sample_data(data, 5, n_participants = 100)
    # compute discrimination performance
    classification_accuracy(sampled_data, test)
  }) |> 
    mean()
  
  return(list(null = null_dist, actual = avg_prop, p = compute_p(null_dist, avg_prop)))
}

#########################
# 2. Tonal vs Non-tonal #
#########################

filtered_data <- miq_combined_filtered |>
  filter(
    musicLessons == "No",
    Tone %in% c("Tonal", "Non-tonal")
  )

langs <- list()

# get non-tonal langs
langs$Non_tonal <- filtered_data |>
  filter(Tone == "Non-tonal") |>
  summarise(lang = unique(language)) |>
  pull(lang)

# assign the Tonal langs (only large sample size ones)
langs$Tonal <- c("Chinese/Cantonese/Yue", "Chinese/Mandarin", "Chinese/Taiwanese/Min Nan/Hokkien", "Thai", "Vietnamese")

# run permutation tests
permutation_tests$T_NT <- map(c("mdt", "mpt", "cabat") |> set_names(),
                              \(.test) permutation_test(filtered_data, .test, 1e4))

##################################
# 4. Pitch-accented vs non-tonal #
##################################

# filtering data to Pitch-accented vs non-tonal
filtered_data <- miq_combined_filtered |>
  filter(
    musicLessons == "No",
    Tone %in% c("Pitch-accented", "Non-tonal")
  )

# get languages in sample
langs <- list()
langs <- map(c("Pitch-accented", "Non-tonal") |> set_names(), \(.langType) {
  filtered_data |>
    filter(Tone == .langType) |>
    with(unique(language))
})

# run permutation tests
permutation_tests$PA_NT <- map(c("mdt", "mpt", "cabat") |> set_names(),
                               \(.test) permutation_test(filtered_data, .test, 1e4))

##################
# 4. Save result #
##################

save(permutation_tests, file = here("results", "permutation_tests.RData"))

# 8. Saving Data --------------------------------------------------------

# miq_confirm_filtered, confirm_plot_data, Table2, languages, TableS3, Table4, TableS6, mods, headphone_score
save(list = c("miq_confirm_filtered", "miq_explore_filtered", "miq_confirm", "excludes", "miq_combined_filtered",
             "Table2", "languages",
              "TableS2A", "TableS2B", "TableS5A", "TableS5B",
              "mods", "users",
              "bubble_data", "Table_combined_income",
              "Table_combined_education", "Table_combined_culture",
              "fig3_data", "main_analysis_estimates",
              # "culture_estimates", "income_estimates", "education_estimates",
             "culture_n", "education_n", "income_n", "income_total_n"),
     file = here("results", "analyses.RData"))
