### get election results ###


# read html from guardian -------------------------------------------------


results_url <- "https://en.wikipedia.org/wiki/2020_United_States_House_of_Representatives_elections"

results_session <- polite::bow(url = results_url, user_agent = "Web Data with R - philipp.bosch@uni-konstanz.de")

results_session %>% 
  polite::scrape() -> results_html


results_html %>% 
  rvest::html_node(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]") %>% 
  rvest::html_nodes(xpath = "//table") %>%
  rvest::html_table() %>% 
  .[c(8,10:59)] -> house_results


house_special <- house_results[1]
house_regular <- house_results[c(2:51)]




clean_results <- function(list_element){
  
  list_element %>% 
    select(1,4,7) %>% 
    slice(-1) %>% 
    rename("results" = 3) %>% 
    separate(results, into = c("results_keep","rest"), sep = "%") %>% 
    mutate(second_best = str_extract(rest, "[0-9].\\.[0-9]")) %>% 
    select(-rest) %>% 
    separate(results_keep, into = c("name", "party", "pct"), sep = "[()]") %>% 
    mutate(name = str_remove(name, "Y")) %>% 
    mutate(across(.cols = everything(), ~ str_squish(.x)),
           pct = as.numeric(pct)) %>% 
    filter(party == "Republican")
}

house_regular_clean <- map(house_regular, clean_results) 

map2(house_regular_clean, state.abb, bind_cols) %>% 
  bind_rows() %>% 
  rename("state_abb" = 7) -> regular_house_df



regular_house_df %>%
  separate(District, 
           into = c("text", "num"), 
           sep = "(?<=[A-Za-z]) (?=[0-9])") %>% 
  mutate(num = as.numeric(num)) %>% 
  mutate(state_num = case_when(
      is.na(num) == TRUE ~ 0,
      num < 10  ~ 0,
      TRUE ~ num)) %>% 
  unite("state_numeric", state_num, num, sep = "") %>% 
  mutate(state_numeric = str_sub(state_numeric, 1,2)) %>% 
  mutate(state_numeric = str_replace_all(state_numeric, "N", "0")) %>% 
  unite("state", state_abb, state_numeric, sep = "") %>% 
  mutate(rep_adv = pct - as.numeric(second_best)) %>% 
  mutate(rep_adv = case_when(
    is.na(second_best) == TRUE ~ pct,
    TRUE ~ rep_adv)) -> election_results_regular





election_results_df <- election_results_regular %>% 
  dplyr::mutate(name = stringr::str_remove_all(name, pattern = "\\[[\\s\\S]*\\]"))
  

election_results_df %>% group_by(state) %>% count() %>% 
  arrange(desc(n))

# save as df --------------------------------------------------------------

saveRDS(election_results_df, "Data/Term Paper/election_results_df.RDS")



  