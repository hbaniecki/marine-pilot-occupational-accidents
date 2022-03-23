library(dplyr)

length_ton <- readxl::read_xlsx("../data/length_ton.xlsx")
df_raw <- readxl::read_xlsx("../data/mpilot_accidents_dataset.xlsx", skip = 1)
colnames(df_raw) <- tolower(gsub(" ", "_", colnames(df_raw)))
df_raw$gross_ton[df_raw$gross_ton %in% 0] <- NA
df_raw$length_overall[df_raw$length_overall %in% 0] <- NA
df_raw$age_of_vessel[df_raw$age_of_vessel %in% 0] <- NA

df_subset_1 <- df_raw %>%
  select(-age_of_person,
         -working_hrs,
         -number_of_accident,
         -dd,
         -accident_type,
         -reporting_state) %>%
  filter(!(is.na(ude_type) | is.na(year_of_occurance))) %>%
  select(-year_of_occurance)

df_subset_2 <- df_subset_1 %>%
  mutate(accident_location = recode_factor(as.factor(accident_location),
       "6"="Pilot/Combination ladder", "5"="Boat Navigation",
       "8"="Use of pilot door", "7"="Deck to bridge",
       "3"="Pier to boat transfer", "4"="Gangway/Accom ladder climbing"
  ),
         month = as.factor(mm),
         caused_by = as.factor(caused_by),
         part_of_day = recode_factor(as.factor(part_of_day),
       "3"="Daytime", "6"="Night", "2"="Morning", "1"="Dawn-Twilight-Sunrise",
       "5"="Evening", "4"="Sunset-Twilight-Dusk",
       "7"=NA_character_
  ),
  ##       wave_height = sea_state,
         ship_type = recode_factor(as.factor(ship_type),
       "9"="Passenger", "8"="Pilot/Tug Boat", "2"="Bulk carrier",
       "1"="Tanker", "5"="Offshore", "3"="Container", "10"="Ro-ro /Car carrier",
       "6"="Miscellaneous", "4"="Fishing", "7"="NavyShip",
       "11"=NA_character_
  ),
         ship_position = recode_factor(as.factor(ship_position_by_definition),
       "2"="Channel, river", "5"="Pier", "3"="Port area",
       "1"="Coastal waters <= 12 nm", "4"="Open sea", 
       "7"=NA_character_
  ),
         dynamic_status = recode_factor(as.factor(dynamic_status),
       "3"="Underway", "1"="Moored", "5"="Maneuvering", "2"="Anchored",
       "8"=NA_character_
  ),
  ## visibility serves better as an ordered variable
  #        visibility = recode_factor(as.factor(visibility), 
  #      "1"="Good", "2"="Moderate", "3"="Poor", "4"=NA_character_
  # ),
         length_overall = cut(length_overall,
                              breaks = length_ton$length_overall),
         gross_ton = cut(gross_ton,
                         breaks = length_ton$gross_ton)) %>%
  select(-mm, -ship_position_by_definition)

df_subset_3 <- df_subset_2

df_subset_3$target <- ifelse(df_subset_3$ude_type %in% 5, 1, 0)
df_subset_3$ude_type <- NULL

df_subset_4 <- df_subset_3 %>%
  select(-caused_by) %>%
  relocate(target) %>%
  distinct()

id1 <- df_subset_4 %>% select(-target) %>% duplicated(fromLast = F)
id2 <- df_subset_4 %>% select(-target) %>% duplicated(fromLast = T)

df_subset_5 <- df_subset_4[!(id1 | id2), ]

# table(apply(df_subset_5, 1, function(x) sum(is.na(x))))
# df_subset_6 <- df_subset_5[apply(df_subset_5, 1, function(x) sum(is.na(x))) != 8, ]
df_subset_6 <- df_subset_5

## -------------- outlier detection
library(isotree)
iso <- isolation.forest(df_subset_6[,-1], max_depth = 5, seed = 1) # same results for 2, 3, 4
pred_iso <- predict(iso, df_subset_6[,-1])
hist(pred_iso)
as.data.frame(df_subset_6[pred_iso > 0.52, ])
## --------------

df <- df_subset_6 %>% filter(pred_iso <= 0.52)

dim(df)
mean(df$target)

saveRDS(df, "../data/mpilot_accidents_dataset_preprocessed.rds")
xlsx::write.xlsx(
  as.data.frame(df), 
  "../data/mpilot_accidents_dataset_preprocessed.xlsx", 
  showNA = FALSE, 
  row.names = FALSE
)

