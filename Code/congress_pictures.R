### scrape pictures and predict gender

source("Code/packages.R")

url_pictures <- "https://en.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives"


session_pictures <- polite::bow(url = url_pictures, user_agent = "Web Data with R - philipp.bosch@uni-konstanz.de")

session_pictures %>% 
  polite::scrape() -> pictures_html


# work with html
pictures_html %>%
  rvest::html_elements("#votingmembers") %>% 
  rvest::html_nodes("img") %>% 
  rvest::html_attr("src") %>% tibble() -> image_links



pictures_html %>%
  rvest::html_elements("#votingmembers") %>% 
  rvest::html_table() %>% 
  purrr::pluck(1) %>%
  dplyr::select(-3) %>% 
  dplyr::filter(Member != "VACANT") %>%
  dplyr::select(Party, District, Member) -> aux_info_pictures
  

bind_cols(aux_info_pictures, image_links) %>% 
  rename("url" = 4) -> pictures_df

# kairos ------------------------------------------------------------------

devtools::install_github('methodds/facerec')

Sys.setenv(kairos_id = read_lines(file = "kairos_key.txt")[1])
Sys.setenv(kairos_key = read_lines(file = "kairos_key.txt")[2])

library(facerec)
facerec_init()



pictures_df <- pictures_df %>% 
  mutate(url = str_c("https:", url),
         url = str_replace_all(url, "\\d{2}(?=px)", "600")) %>% 
  filter(Party == "Republican")

prediction_list <- vector(mode = "list", length = length(pictures_df$url))

pb <- progress_bar$new(total = length(pictures_df$url))

for (i in seq_along(pictures_df$url)) {
  
  control <- seq(from = 55, to = length(pictures_df$url), by = 55)
  pb$tick()
  
  prediction_list[[i]] <- detect(image = pictures_df$url[i])
  
  if (i %in% control) {
    print("sleepy")
    Sys.sleep(60)
    next
  }
  
  
}


bind_rows(prediction_list) %>% 
  distinct(img_source, .keep_all = T) -> predict_df

saveRDS(predict_df, file = "Data/Term Paper/predict_df.RDS")


pictures_df %>% 
  left_join(predict_df, by = c("url" = "img_source")) %>% 
  select(face_asian, face_black, face_hispanic, face_white, face_gender_type, 
         Party, District, Member, url) %>% 
  rename(gender = "face_gender_type") %>% 
  pivot_longer(cols = starts_with("face"),
               names_to = "race") %>%
  group_by(Member) %>% 
  slice_max(value) %>% 
  pivot_wider(names_from = race, values_from = value) %>% 
  mutate(race = case_when(
    is.na(face_white) != TRUE ~ "white",
    is.na(face_hispanic) != TRUE ~ "hispanic",
    is.na(face_black) != TRUE ~ "black",
    is.na(face_asian) != TRUE ~ "asian"
  )) %>% 
  select(-starts_with("face")) %>% 
  ungroup() -> race_gender_df




### mutate district variable to prepare for merge

race_gender_df %>%
  arrange(District) %>% 
  mutate(District = str_squish(District)) %>% 
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
  mutate(text = str_remove_all(text, "at-large"),
         text = str_squish(text)) %>% 
  left_join(tibble(state.abb, state.name), by = c("text" = "state.name")) %>% 
  unite("state", state.abb, state_numeric, sep = "") -> race_gender_df


saveRDS(race_gender_df, file = "Data/Term Paper/race_gender_df.RDS")

### morgen weiter
### Daten abschließen: special elections dazu, alle aux Daten joinen
### PACS finalisieren, lollipop plot

### Dokument aufsetzen, beginnen mit outline
### wie framen, beobachtung der Entscheidung, hintergründe etc


### Donnerstag: House Freedom Donors ziehen


