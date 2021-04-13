### merge finance, election and picture data ###


# load df -----------------------------------------------------------------

fec_df <- readRDS("~/Documents/SEDS/3. Semester/Web Data with R/Web Scraping with R/Data/Term Paper/fec_df.RDS")



# merging -----------------------------------------------------------------


fec_df %>%
  left_join(election_results_df, by = c("district" = "state")) %>% 
  select(-firstlastp) -> first_join


first_join %>% 
  left_join(race_gender_df, by = c("district" = "state")) %>% 
  rename("state_long" = "text.x") %>% 
  select(-c(text.y, Member)) -> final_join



# save df which is ready to analyse ---------------------------------------

saveRDS(final_join, "Data/Term Paper/analysis_df.RDS")



