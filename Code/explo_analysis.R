
# load analysis df --------------------------------------------------------

source(file = "Code/packages.R")
analysis_df <- readRDS("~/Documents/SEDS/3. Semester/Web Data with R/Web Scraping with R/Data/Term Paper/analysis_df.RDS")


### explore data

analysis_df %>% 
  filter(objector == 1 & support_oppose_indicator == "S") %>% 
  group_by(candidate_id)




analysis_df %>% 
  filter(objector == 0 & support_oppose_indicator == "S") %>% 
  group_by(committee_name) %>% 
  summarise(total_spend = sum(total)) %>% 
  summarise(sum = sum(total_spend))
arrange(desc(total_spend))

analysis_df %>% 
  filter(objector == 1 & support_oppose_indicator == "S") %>% 
  group_by(committee_name) %>% 
  summarise(total_spend = sum(total)) %>% 
  summarise(sum = sum(total_spend))



### idea: generate index with percentages, this pac sponsors % percent of objectors
### and only x % percent of non-objectors

### control for close races

### restrict to PACs which supported at least two candidates

analysis_df %>% 
  filter(objector == 1 & support_oppose_indicator == "S") %>% 
  count(committee_name) %>% 
  filter(n > 1) %>% 
  arrange(desc(n)) -> objector_pacs

analysis_df %>% 
  filter(objector == 0 & support_oppose_indicator == "S") %>% 
  count(committee_name) %>% 
  filter(n > 1) %>% 
  arrange(desc(n)) -> non_objector_pacs

### check if there are significant pacs which only support one group

non_objector_pacs %>% 
  anti_join(objector_pacs, by = c("committee_name" = "committee_name"))

objector_pacs %>% 
  anti_join(non_objector_pacs, by = c("committee_name" = "committee_name"))


### plotting around
analysis_df %>% 
  filter(support_oppose_indicator == "S") %>% 
  group_by(name) %>% 
  summarise(spending = sum(total),
            rep_share = mean(rep_adv),
            objector = objector,
            gender  = gender) %>% 
  ggplot(aes(x = rep_share, y = spending, colour = as.factor(objector), shape = gender)) + 
  geom_point() +
  theme_minimal()

### percentages of objectors

analysis_df %>% 
  group_by(name) %>% 
  filter(objector == 1) %>% 
  n_groups() -> n_objectors

analysis_df %>% 
  group_by(name) %>% 
  filter(objector == 0) %>% 
  n_groups() -> n_normal


objector_pacs %>% 
  bind_cols(tibble(size_objectors = n_objectors)) %>% 
  mutate(percentage = n/size_objectors) %>% 
  left_join(non_objector_pacs, by = "committee_name") %>% 
  bind_cols(tibble(size_non_objectors = n_normal)) %>% 
  mutate(percentage_non_objectors = n.y / size_non_objectors) %>% 
  mutate(percentage_diff = percentage - percentage_non_objectors) %>% 
  arrange(desc(percentage_diff)) -> diff_df



lollipop_1_df <- diff_df %>% 
  filter(percentage_diff > 0) %>% 
  arrange(percentage_diff) %>% 
  mutate(rank = row_number()) 

lollipop_1_df %>% 
  ggplot() +
  geom_segment( aes(x=rank, xend=rank, y=percentage, yend=percentage_non_objectors), color="grey") +
  geom_point( aes(x=rank, y=percentage), color=rgb(0.2,0.7,0.1,0.8), size=3 ) +
  geom_point( aes(x=rank, y=percentage_non_objectors), color=rgb(0.7,0.2,0.1,0.8), size=3 ) +
  coord_flip() +
  scale_x_continuous(
    breaks = lollipop_1_df$rank, # specify tick breaks using rank column
    labels = lollipop_1_df$committee_name # specify tick labels using x column
  ) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Value of Y")



# nokken poole scores -----------------------------------------------------

Hall_members <- read_csv("Data/Term Paper/Hall_members.csv") %>% 
  filter(congress == 117,
         party_code == 200)


Hall_members %>%
  mutate(state_num = case_when(
    state_abbrev == "AK" ~ 0,
    state_abbrev == "MT" ~ 0,
    state_abbrev == "ND" ~ 0,
    state_abbrev == "SD" ~ 0,
    state_abbrev == "WY" ~ 0,
    district_code < 10  ~ 0,
    TRUE ~ district_code),
    district_code = case_when(
      district_code == 1 & state_abbrev %in% c("AK","MT", "ND", "WY", "SD") ~ 0,
      TRUE ~ district_code)) %>% 
  unite("state_numeric", state_num, district_code, sep = "") %>% 
  mutate(state_numeric = str_sub(state_numeric, 1,2)) %>% 
  unite("district", state_abbrev, state_numeric, sep = "") %>% 
  select(district, bioname, nominate_dim1, nokken_poole_dim1, 
         nominate_number_of_errors, nominate_geo_mean_probability) -> dw_scores_df
  
  
analysis_df %>% 
  left_join(dw_scores_df, by = "district") -> analysis_df

### plotting around
analysis_df %>% 
  filter(support_oppose_indicator == "S") %>% 
  group_by(name)  %>% 
  ggplot(aes(x = nokken_poole_dim1, y = as.factor(objector), fill = as.factor(objector))) +
  geom_violin() +
  coord_flip() + theme_minimal()



house_freedom_fund_donors <- read_csv("Data/Term Paper/schedule_a-2021-04-07T10 43 34.csv")
View(house_freedom_fund_donors)
