# ---------------------------------------------------------------------------------------
# WEB DATA COLLECTION WITH R
# Philipp Bosch
# Script for FEC-API Term Paper
# Winter Semester 2020/21
# ---------------------------------------------------------------------------------------


# load packages -----------------------------------------------------------

source("Code/packages.R")


# load data ---------------------------------------------------------------

read_delim("data/Term Paper/ccl.txt", delim = "|", col_names = F) -> commitee_link

rep_objectors <- read_delim("Data/Term Paper/Rep_Objectors.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename("district" = 2)

# prepare data ------------------------------------------------------------


var_names <- c("CAND_ID","CAND_ELECTION_YR","FEC_ELECTION_YR","CMTE_ID","CMTE_TP",
               "CMTE_DSGN","LINKAGE_ID")

names(commitee_link) <- var_names

commitee_link %>% 
  filter(CAND_ELECTION_YR == 2020)



# API calls ---------------------------------------------------------------

key <- read_lines("fec_key.txt")



# Build set of house candidates -------------------------------------------

# As the legislator package does not include newest us house data, I scrape the
# data from wikipedia

# assign and parse urls ---------------------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives"

# polite scraping with the polite package
session <- polite::bow(url = url, user_agent = "Web Data with R - philipp.bosch@uni-konstanz.de")

session %>% 
  polite::scrape() -> house_html


# work with html
house_html %>%
  rvest::html_elements("#votingmembers") %>% 
  rvest::html_table() %>% 
  purrr::pluck(1) %>%
  dplyr::select(-3) %>% 
  dplyr::filter(Party == "Republican")  %>% 
  rename("prior_job" = 4, "office_since" = 6, "age" = 8) -> rep_house

# clean wiki artefacts

rep_house <- rep_house %>% 
  mutate(across(.cols = everything(), ~ stringr::str_squish(.x))) %>% 
  mutate(across(.cols = everything(), ~ stringr::str_remove_all(.x, pattern = "\\[[0-9]*\\]"))) %>% 
  mutate(birthday = lubridate::ymd(stringr::str_extract(age, "[0-9]{4}-[0-9]{2}-[0-9]{2}")),
         office_since = as.integer(stringr::str_remove(office_since, "\\(special\\)")),
         Member = stringr::str_replace_all(Member, c("é"="e", "í" ="i", 
                                                     "Mike" = "Michael", 
                                                     "Jim" = "James",
                                                     "Bob" = "Robert",
                                                     "Tom McClintock" = "Thomas McClintock",
                                                     "Buddy Carter" = "Earl Leroy Carter",
                                                     "Rick W. Allen" = "Richard Allen",
                                                     "Randy Feenstra" = "Randall Feenstra",
                                                     "Hal Rogers" = "Harold Rogers",
                                                     "Andy Harris" = "Andrew Harris",
                                                     "Jack Bergman" = "John Bergman",
                                                     "Bill Huizenga" = "William Huizenga",
                                                     "Tom Emmer" = "Thomas Emmer",
                                                     "Tom Reed" = "Thomas Reed",
                                                     "Ted Budd" = "Theodore Budd",
                                                     "Chuck Fleischmann" = "Charles Fleischmann",
                                                     "Mark E. Green" = "Mark Green",
                                                     "Louie Gohmert" = "Louis Gohmert",
                                                     "Van Taylor" = "Nicholas Taylor",
                                                     "Beth Van Duyne" = "Elizabeth van Duyne",
                                                     "Liz Cheney" = "Elizabeth Cheney"))) %>%
  select(-age)






# API calls for candidate - committee match -------------------------------

### function



fec_committee <- function(candidate){
  

  base_url <- "https://api.open.fec.gov/v1/candidates/search/"
  query <- list(page = 1, name = candidate, api_key = key,
                election_year = 2020, office = "H")
  url <- httr::modify_url(url = base_url, query = query)
  
  httr::GET(url) -> raw_json
     
  

  
}



### call with possibly

rep_house %>% 
  select(Member) %>% 
  pull() %>% 
  map(possibly(.f = fec_committee, otherwise = NA_real_)) -> try_map

saveRDS(try_map, "Data/Term Paper/list_of_raw_json.RDS")




### extract committee information from raw json objects

extract_committee <- function(list_element) {
  
  list_element %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>% 
    purrr::pluck("results","principal_committees", 1) %>% 
    dplyr::select(candidate_ids, committee_id, name, cycles, affiliated_committee_name) %>% 
    tidyr::tibble() %>% 
    dplyr::mutate(cycles = sapply(cycles, toString),
                  candidate_ids = sapply(candidate_ids, toString)) %>% 
    filter(str_detect(cycles, pattern = "2020")) -> df
  
}

list_of_raw_json %>% 
  map(possibly(.f = extract_committee, otherwise = NA_real_)) -> map_committee





### for three cases the api did not work
### manual research of informations

calvert_tibble <- tibble(candidate_ids = "H2CA37023", committee_id = "C00257337",
                        name = "KEN CALVERT FOR CONGRESS COMMITTEE", cycles = NA,
                        affiliated_committee_name = NA)

donalds_tibble <- tibble(candidate_ids = "H2FL14186", committee_id = "C00733329",
                         name = "BYRON DONALDS FOR CONGRESS", cycles = NA,
                         affiliated_committee_name = NA)

kelly_tibble <- tibble(candidate_ids = "H0PA03271", committee_id = "C00474189",
                       name = "MIKE KELLY FOR CONGRESS", cycles = NA,
                       affiliated_committee_name = NA)

map_committee[[24]] <- calvert_tibble
map_committee[[42]] <- donalds_tibble
map_committee[[155]] <- kelly_tibble

### add wiki name to each df in list

rep_house %>% select(Member) %>% pull() -> list_of_names

map_committee[[1]] %>% bind_cols(list_of_names[[1]])

map2(map_committee, list_of_names, .f = bind_cols) -> fec_id_df

fec_id_df <- bind_rows(fec_id_df) %>% 
  rename("cand_name" = 6) %>% 
  mutate(candidate_ids = str_remove_all(candidate_ids, ",.*"))

### first we focus on candidates and not on committees, therefore we filter for unique candidates

fec_id_df %>% distinct(candidate_ids, .keep_all = T) -> fec_id_df_unique



### get all independent expenditures for candidates





### function to get independent expenditures

fec_expenditures <- function(candidate){
  
  
  base_url <- "https://api.open.fec.gov/v1/schedules/schedule_e/by_candidate/"
  query <- list(cycle = 2020, api_key = key,
                election_full = "true", page = 1, per_page = 100,
                candidate_id = candidate)
  url <- httr::modify_url(url = base_url, query = query)
  
  raw_json <- httr::GET(url) 
  
  pages <- fromJSON(content(raw_json, "text", encoding = "UTF-8"))$pagination$pages
  
  
  if(pages > 1){
      stop("build pagination")
  }
  
  else{
    return(raw_json)
  }
}





### query independent expenditures


pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = 100, clear = FALSE, width= 60)

for (i in seq_along(fec_id_df$candidate_ids)){
  
  pb$tick()
  
  indep_expenditures_list[[i]] <- fec_expenditures(fec_id_df$candidate_ids[[i]])
  
  if(as.integer(indep_expenditures_list[[i]]$headers$`x-ratelimit-remaining`) == 0){
    print(str_c("Rate limit reached, going to sleep for an hour and will wake up again at: ", Sys.time() + 3600))
    Sys.sleep(3600)
    next
  }
  print(fec_id_df$candidate_ids[[i]])
}



### map calls

indep_expenditures_list <- vector(mode = "list", length = length(fec_id_df_unique$candidate_ids))

fec_id_df_unique %>% 
  select(candidate_ids) %>% 
  pull() %>% 
  map(possibly(.f = fec_expenditures, otherwise = NA_real_)) -> indep_expenditures_list



saveRDS(indep_expenditures_list, "Data/Term Paper/list_of_raw_indep_exp.RDS")

fromJSON(content(indep_expenditures_list[[1]], "text", encoding = "UTF-8"))$results


### we don't need the committees right now, so only keep one observation per candidate


fec_id_df %>% distinct(candidate_ids, .keep_all = T) -> fec_id_df_unique


extract_indep_exp <- function(list_element){
  
  list_element %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>% 
    purrr::pluck("results") -> df
  return(df)

}



indep_expenditures_list %>% 
  map(possibly(.f = extract_indep_exp, otherwise = NA_real_)) -> indep_expenditures_df

indep_expenditures_df %>% 
  keep( ~ !is.null(.x)) -> indep_expenditures_df

indep_expenditures_df <- indep_expenditures_df[-80]


### bind rows together

bind_rows(indep_expenditures_df) %>% 
  tibble() -> indep_expenditures_df

### first summaries

indep_expenditures_df %>%
  filter(support_oppose_indicator == "O") %>% 
  group_by(candidate_name) %>% 
  summarise(total_exp = sum(total)) %>%
  arrange(desc(total_exp))
    

### build indicator for objection



map(list_of_raw_json, ~ httr::content(.x, as = "text", encoding = "UTF-8")) %>%
  map(~ jsonlite::fromJSON(.x)) %>% 
  map(~ purrr::pluck(.x, "results")) %>% 
  map(possibly(~ select(.x, election_districts, state, candidate_id), otherwise = NA)) %>% 
  map(~ tibble(.x)) -> district_info

### again the candidates calvert, donalds and kelly seem to behave odd
### manual cleaning

calvert_district <- tibble(election_districts = 42, state = "CA",
                           candidate_id = "H2CA37023")
                           

donalds_district <- tibble(election_districts = 19, state = "FL",
                           candidate_id = "H2FL14186")
                           

kelly_district <- tibble(election_districts = 16, state = "PA",
                          candidate_id = "H0PA03271")

district_info[[24]] <- calvert_district
district_info[[42]] <- donalds_district
district_info[[155]] <- kelly_district

# get district of most recent election

district_info <- map(district_info, ~ mutate(.x, election_districts = sapply(election_districts, toString)))

district_info <- map(district_info, ~ mutate(.x, election_districts = str_sub(election_districts, start = -2)))


district_info_df <- bind_rows(district_info)

district_info_df <- unite(district_info_df, "district", state, election_districts, sep = "")

district_info_df %>% 
  left_join(rep_objectors) %>% 
  select(district, candidate_id, firstlastp) -> district_info_df

district_info_df <- district_info_df %>% 
  mutate(
    objector = case_when(
      is.na(firstlastp) == TRUE ~ 0,
      !is.na(firstlastp) == TRUE ~ 1,
    )
  )

### join vote objection df with independent expenditure df

final_df <- inner_join(indep_expenditures_df, district_info_df, by = "candidate_id")



# save fec data frame -----------------------------------------------------

saveRDS(final_df, file = "Data/Term Paper/fec_df.RDS")

