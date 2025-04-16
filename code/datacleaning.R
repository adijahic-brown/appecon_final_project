#### Hyderabad + Karnataka data
if (hyderabad_karnataka == 1) {
  print("Cleaning Hyderabad data")
  # Read the two CSV files
  df_rs_rf <- read_csv("data/raw/ReplicationPackage_ChangesInSocialNetworkStructure_2024/data_RS_RF.csv")
  names(df_rs_rf)[1] <- "hhid"

  df_joined <- read_csv("data/raw/ReplicationPackage_ChangesInSocialNetworkStructure_2024/joined_data_ml_0.73_training_1.csv")
  overlap_vars <- intersect(names(df_rs_rf), names(df_joined))
  overlap_vars <- setdiff(overlap_vars, "hhid")  # don't drop hhid
  df_joined_cleaned <- df_joined %>% select(-all_of(overlap_vars))

  # Inner join on 'hhid', keeping df_rs_rf’s version of overlapping columns
  hyderabad_df <- inner_join(df_rs_rf, df_joined_cleaned, by = "hhid")

  hyderabad_df <- mutate(hyderabad_df, 
                     any_old_biz = if_else(!is.na(any_old_biz), if_else(any_old_biz == "Yes", 1L, 0L),as.integer(NA)),
                     anychild1318_1 = if_else(!is.na(anychild1318_1), if_else(anychild1318_1 == "Yes", 1L, 0L),as.integer(NA)),
                     spouse_literate_1 = if_else(!is.na(spouse_literate_1), if_else(spouse_literate_1 == "Yes", 1L, 0L),as.integer(NA)),
                     spouse_works_wage_1 = if_else(!is.na(spouse_works_wage_1), if_else(spouse_works_wage_1 == "Yes", 1L, 0L),as.integer(NA)),
                     ownland_hyderabad_1 = if_else(!is.na(ownland_hyderabad_1), if_else(ownland_hyderabad_1 == "Yes", 1L, 0L),as.integer(NA)),
                     ownland_village_1 = if_else(!is.na(ownland_village_1), if_else(ownland_village_1 == "Yes", 1L, 0L),as.integer(NA))
  )

  # Save the cleaned Hyderabad data
  write_csv(hyderabad_df, "data/clean/hyderabad_data.csv")


  print("Cleaning Karnataka data")
  # Read the .dta file
  karnataka_df <- read.dta("data/raw/ReplicationPackage_ChangesInSocialNetworkStructure_2024/node_data_ml_RF_0.73_training_1_spandana_1.dta")

  karnataka_df <- mutate(karnataka_df, 
                     treatment = if_else(treatment == "Treatment", 1L, 0L),
                     any_old_biz = if_else(!is.na(any_old_biz), if_else(any_old_biz == "Yes", 1L, 0L),as.integer(NA)),
                     anychild1318_1 = if_else(!is.na(anychild1318_1), if_else(anychild1318_1 == "Yes", 1L, 0L),as.integer(NA)),
                     spouse_literate_1 = if_else(!is.na(spouse_literate_1), if_else(spouse_literate_1 == "Yes", 1L, 0L),as.integer(NA)),
                     spouse_works_wage_1 = if_else(!is.na(spouse_works_wage_1), if_else(spouse_works_wage_1 == "Yes", 1L, 0L),as.integer(NA)),
                     ownland_hyderabad_1 = if_else(!is.na(ownland_hyderabad_1), if_else(ownland_hyderabad_1 == "Yes", 1L, 0L),as.integer(NA)),
                     ownland_village_1 = if_else(!is.na(ownland_village_1), if_else(ownland_village_1 == "Yes", 1L, 0L),as.integer(NA))
  )

  # Save the cleaned Karnataka data
  write_csv(karnataka_df, "data/clean/karnataka_data.csv")
}

#### GEM data
if (gem == 1) {
# Add your GEM data processing code here
  print("Cleaning GEM data")

  countries_to_keep <- c("India", "Uruguay", "South Africa", "Burkina Faso", "Brazil", "Chile", "Colombia", "Guatemala", "Panama")

  gem_2020_df <- read.spss("data/raw/gem data_raw/GEM 2020 APS Global Individual Level Data_Jan2022.sav", to.data.frame = TRUE)
  gem_2020_df <- gem_2020_df |> filter(country %in% countries_to_keep)

  # Save the cleaned GEM data
  write_csv(gem_2020_df, "data/clean/gem_data.csv")
}


#### Enterprise data
if (enterprise == 1) {
  print("Cleaning Enterprise data")

  enterprise_df <- read.dta("data/raw/Enterprise_Kenya/Kenya_2007_2013_2018.dta")
  # Drop duplicates of panelid while prioritizing rows based on k9
  enterprise_df_cleaned <- enterprise_df %>%
    filter(!is.na(k9)) %>%
    mutate(priority = if_else(k9 == "on-bank financial institutions", 1, 0)) %>%
    arrange(panelid, desc(priority)) %>%
    group_by(panelid) %>%
    slice(1) %>%
    ungroup()

  missing_k9_panelids <- setdiff(enterprise_df$panelid, enterprise_df_cleaned$panelid)
  if (length(missing_k9_panelids) > 0) {
    random_rows <- enterprise_df %>%
      filter(panelid %in% missing_k9_panelids) %>%
      group_by(panelid) %>%
      slice_sample(n = 1) %>%
      ungroup()
    enterprise_df <- bind_rows(enterprise_df_cleaned, random_rows)
  }

  # Save the cleaned Enterprise data
  write_csv(enterprise_df, "data/clean/enterprise_data.csv")
}


