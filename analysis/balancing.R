
# libraries ---------------------------------------------------------------

library(pacman)
p_load(here,
       tidyverse,
       WeightIt,
       gbm)

# data --------------------------------------------------------------------

users_full <- 1993012 # hard coded due to large data file 
users <- 716855
explore_n <- 297600
confirm_n <- 404306

miq_confirm <- read.csv(here("data", "Confirmatory", "Confirm_full.csv"))
miq_confirm_filtered <- read.csv(here("data", "Confirmatory", "Confirm_filtered.csv"))
miq_confirm_matched <- read.csv(here("data", "Confirmatory","Confirm_matched.csv"))
load(here("data", "Confirmatory", "ipw_confirm.RData"))

miq_explore <- read.csv(here("data", "Exploratory", "Explore_full.csv"))
miq_explore_filtered <- read.csv(here("data", "Exploratory", "Explore_filtered.csv"))
miq_explore_matched <- read.csv(here("data", "Exploratory", "Explore_matched.csv"))
load(here("data", "Exploratory", "ipw_explore.RData"))

languages <- read.csv(here("data", "language.csv"))

headphone_score <- read.csv(here("data", "headphone_score.csv"))


# more --------------------------------------------------------------------

# This code chunk contains scripts that turns the pre-exclusion exploratory set data into the post-exclusion exploratory set, the 1-1 matched exploratory set, and the inverse probability weighted exploratory set

## Running the below code would produce the post-exclusion version of the exploratory data from the pre-exclusion exploratory data:

miq_explore_filt <- miq_explore %>%
  filter(language %in% languages$language) # language

miq_explore_filt$age <- as.numeric(miq_explore_filt$age)
miq_explore_filt$lessonsAge <- as.numeric(miq_explore_filt$lessonsAge)

miq_explore_filt <- miq_explore_filt %>%
  filter(hearingImp == "No") %>%
  filter(takenBefore == "No") %>%
  filter(age >=8 & age <=90)

exclude_id <- miq_explore_filt %>%
  filter(lessonsAge < 2 | lessonsAge > 90) %>%
  .$user_id # lesson age

miq_explore_filt <- miq_explore_filt %>%
  filter(!user_id %in% exclude_id)

exclude_id <- miq_explore_filt %>%
  filter(headphone == "No") %>%
  filter(workspace == "I am in a very noisy place") %>%
  .$user_id

miq_explore_filt <- miq_explore_filt %>%
  filter(!user_id %in% exclude_id)

miq_explore_filt <- full_join(miq_explore_filt, languages %>%
                                select(language, Tone), by = "language")
write.csv(miq_explore_filt, here("data", "Exploratory", "Explore_filtered.csv"))

## Running the below code would produce the 1-to-1 matched version of the exploratory data from the post-exclusion exploratory data

miq_TBsampled <- miq_explore_filt %>%
  mutate(age_level = case_when(
    age >= 10 & age <= 19~ "10-19",
    age >= 20 & age <= 29 ~ "20-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 ~ "50 and above"
  ))

miq_TBsampled$X <- c(1:nrow(miq_TBsampled))

mvr_total <- miq_TBsampled  %>%
  filter(Tone != "Non-tonal", !is.na(age_level)) %>%
  group_by(gender, age_level, lessonsAge) %>%
  count() %>%
  select(gender, age_level, lessonsAge)

mvr_total$matching = c(1:nrow(mvr_total))

miq_TBsampled <- full_join(miq_TBsampled, mvr_total, by = c("gender", "age_level", "lessonsAge")) %>%
  filter(!is.na(matching)) # join mvr with miq_TBsampled to stick the mvr values onto the miq_TBsampled dataset


matching_total <- miq_TBsampled %>%
  group_by(Tone, matching) %>%
  count() %>%
  pivot_wider(names_from = Tone, values_from = n) %>%
  na.omit(.)

min_num_total = rep(0, nrow(matching_total))
for (x in c(1:nrow(matching_total))){
  min_num_total[x] <- min(matching_total[x, 2:4])
}

sample_size = sum(min_num_total)

Tone_idx_total <- rep(0, sample_size) # vector recording for sampled tone participants' ID
Nontone_idx_total <- rep(0,sample_size) # vector recording for #sampled non-tone participants' ID
Pitch_idx_total <- rep(0, sample_size)
idx = 1

for (num in c(1:nrow(matching_total))){ # sampling through each co-variate (pitch_mvr) level
  mvr = matching_total$matching[num]
  n = min_num_total[num]
  
  Nontone_idx_total[idx:(idx+n-1)]<-sample_n(miq_TBsampled[miq_TBsampled$Tone == "Non-tonal" & miq_TBsampled$matching == mvr, ], n, replace = FALSE) %>%
    .$X
  
  Pitch_idx_total[idx:(idx+n-1)]<-sample_n(miq_TBsampled[miq_TBsampled$Tone == "Pitch_accent" & miq_TBsampled$matching == mvr, ], n, replace = FALSE) %>%
    .$X
  
  Tone_idx_total[idx:(idx+n-1)]<-sample_n(miq_TBsampled[miq_TBsampled$Tone == "Tonal" & miq_TBsampled$matching == mvr, ], n, replace = FALSE) %>%
    .$X
  
  idx = idx + n
}

Tone_sample <- miq_TBsampled %>%
  filter(X %in% Tone_idx_total) # creating the samples by subsetting
Nontone_sample <- miq_TBsampled %>%
  filter(X %in% Nontone_idx_total)
Pitch_sample <- miq_TBsampled %>%
  filter(X %in% Pitch_idx_total)

Total_sample <- rbind(Tone_sample, Nontone_sample, Pitch_sample) # here is our pooled sample!

write.csv(Total_sample, here("data", "Exploratory", "Explore_matched.csv"))

# Other -------------------------------------------------------------------

## Running the below code would produce the inverse-probability weighted exploratory sample from the post-exclusion exploratory sample (note: this approach is computationally expensive and may take hours to run)

miq_filtered_wide <- miq_explore_filt
missing_lessonage <- miq_filtered_wide %>%
  filter(musicLessons == "Yes" & is.na(lessonsAge)) %>%
  .$user_id

data_clean <- miq_filtered_wide %>%
  filter(!user_id %in% missing_lessonage)

data_clean <- data_clean %>%
  # remove participants who are doing the test again
  filter(takenBefore == "No",
         hearingImp == "No") %>%
  mutate(lesson_no = as.numeric(musicLessons == "No"),
         lesson_v_early = as.numeric(musicLessons == "Yes" & lessonsAge < 6),
         lesson_early = as.numeric(musicLessons == "Yes" & between(lessonsAge, 6,9)),
         lesson_mid = as.numeric(musicLessons == "Yes" & between(lessonsAge, 9,12)),
         lesson_late = as.numeric(musicLessons == "Yes" & between(lessonsAge,12,20)),
         lesson_v_late = as.numeric(musicLessons == "Yes" & lessonsAge > 20),
         headphone = as.numeric(headphone == "Yes")) %>%
  select(-X, -user_id, -takenBefore, -hearingImp)

data_clean$Tone <- as.factor(data_clean$Tone)
data_clean$gender <- as.factor(data_clean$gender)
data_clean$musicLessons <- as.factor(data_clean$musicLessons)

data_weighted <- weightit(Tone ~ age + gender + lesson_no +
                            lesson_v_early + lesson_early + lesson_mid + lesson_late +
                            lesson_v_late, data = data_clean, estimand = "ATE", method = "gbm")

save(data_weighted, file = here("data", "Exploratory", "ipw_explore.RData"))


# more --------------------------------------------------------------------

# This code chunk contains scripts that turns the pre-exclusion confirmatory set data into the post-exclusion confirmatory set, the 1-1 matched confirmatory set, and the inverse probability weighted confirmatory set

## Running the below code would produce the post-exclusion version of the confirmatory data from the pre-exclusion confirmatory data:

miq_confirm_filt <- miq_confirm %>%
  filter(language %in% languages$language) # language

miq_confirm_filt$age <- as.numeric(miq_confirm_filt$age)
miq_confirm_filt$lessonsAge <- as.numeric(miq_confirm_filt$lessonsAge)

miq_confirm_filt <- miq_confirm_filt %>%
  filter(hearingImp == "No") %>%
  filter(takenBefore == "No") %>%
  filter(age >=8 & age <=90)

exclude_id <- miq_confirm_filt %>%
  filter(lessonsAge < 2 | lessonsAge > 90) %>%
  .$user_id # lesson age

miq_confirm_filt <- miq_confirm_filt %>%
  filter(!user_id %in% exclude_id)

exclude_id <- miq_confirm_filt %>%
  filter(headphone == "No") %>%
  filter(workspace == "I am in a very noisy place") %>%
  .$user_id

miq_confirm_filt <- miq_confirm_filt %>%
  filter(!user_id %in% exclude_id)

miq_confirm_filt <- full_join(miq_confirm_filt, languages %>%
                                select(language, Tone), by = "language")

write.csv(miq_confirm_filt, here("data", "Confirmatory", "Confirm_filtered.csv"))


# er ----------------------------------------------------------------------

## Running the below code would produce the 1-to-1 matched version of the confirmatory data from the post-exclusion confirmatory data:

miq_TBsampled <- miq_confirm_filt %>%
  mutate(age_level = case_when(
    age >= 10 & age <= 19~ "10-19",
    age >= 20 & age <= 29 ~ "20-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 ~ "50 and above"
  ))

miq_TBsampled$X <- c(1:nrow(miq_TBsampled))

mvr_total <- miq_TBsampled  %>%
  filter(Tone != "Non-tonal", !is.na(age_level)) %>%
  group_by(gender, age_level, lessonsAge) %>%
  count() %>%
  select(gender, age_level, lessonsAge)

mvr_total$matching = c(1:nrow(mvr_total))

miq_TBsampled <- full_join(miq_TBsampled, mvr_total, by = c("gender", "age_level", "lessonsAge")) %>%
  filter(!is.na(matching)) # join mvr with miq_TBsampled to stick the mvr values onto the miq_TBsampled dataset

matching_total <- miq_TBsampled %>%
  group_by(Tone, matching) %>%
  count() %>%
  pivot_wider(names_from = Tone, values_from = n) %>%
  na.omit(.)

min_num_total = rep(0, nrow(matching_total))
for (x in c(1:nrow(matching_total))){
  min_num_total[x] <- min(matching_total[x, 2:4])
}

sample_size = sum(min_num_total)

Tone_idx_total <- rep(0, sample_size) # vector recording for sampled tone participants' ID
Nontone_idx_total <- rep(0,sample_size) # vector recording for sampled non-tone participants' ID
Pitch_idx_total <- rep(0, sample_size)
idx = 1
for (num in c(1:nrow(matching_total))){ # sampling through each co-variate (pitch_mvr) level
  mvr = matching_total$matching[num]
  n = min_num_total[num]
  
  Nontone_idx_total[idx:(idx+n-1)]<-sample_n(miq_TBsampled[miq_TBsampled$Tone == "Non-tonal" & miq_TBsampled$matching == mvr, ], n, replace = FALSE) %>%
    .$X
  
  Pitch_idx_total[idx:(idx+n-1)]<-sample_n(miq_TBsampled[miq_TBsampled$Tone == "Pitch_accent" & miq_TBsampled$matching == mvr, ], n, replace = FALSE) %>%
    .$X
  
  Tone_idx_total[idx:(idx+n-1)]<-sample_n(miq_TBsampled[miq_TBsampled$Tone == "Tonal" & miq_TBsampled$matching == mvr, ], n, replace = FALSE) %>%
    .$X
  
  idx = idx + n
}

Tone_sample <- miq_TBsampled %>%
  filter(X %in% Tone_idx_total) # creating the samples by subsetting
Nontone_sample <- miq_TBsampled %>%
  filter(X %in% Nontone_idx_total)
Pitch_sample <- miq_TBsampled %>%
  filter(X %in% Pitch_idx_total)

Total_sample <- rbind(Tone_sample, Nontone_sample, Pitch_sample) # here is our pooled sample!

write.csv(Total_sample, here("data", "Confirmatory", "Confirm_matched.csv"))

## Running the below code would produce the inverse-probability weighted confirmatory sample from the post-exclusion confirmatory sample (note: this approach is computationally expensive and may take hours to run)

data_clean <- miq_confirm_filt %>%
  # remove participants who are doing the test again
  filter(takenBefore == "No",
         hearingImp == "No") %>%
  mutate(lesson_no = as.numeric(musicLessons == "No"),
         lesson_v_early = as.numeric(musicLessons == "Yes" &  lessonsAge < 6),
         lesson_early = as.numeric(musicLessons == "Yes" & between(lessonsAge, 6,9)),
         lesson_mid = as.numeric(musicLessons == "Yes" & between(lessonsAge, 9,12)),
         lesson_late = as.numeric(musicLessons == "Yes" & between(lessonsAge,12,20)),
         lesson_v_late = as.numeric(musicLessons == "Yes" & lessonsAge > 20),
         headphone = as.numeric(headphone == "Yes")) %>%
  select(-X, -user_id, -takenBefore, -hearingImp, -workspace, -country)

data_clean$Tone <- as.factor(data_clean$Tone)
data_clean$gender <- as.factor(data_clean$gender)
data_clean$musicLessons <- as.factor(data_clean$musicLessons)

data_weighted <- weightit(Tone ~ age + gender + lesson_no +
                            lesson_v_early + lesson_early + lesson_mid + lesson_late +
                            lesson_v_late, data = data_clean, estimand = "ATE", method = "gbm")

save(data_weighted, file = here("data", "Confirmatory", "ipw_confirm.RData"))

