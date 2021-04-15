
# load analysis df --------------------------------------------------------

source(file = "Code/packages.R")
analysis_df <- readRDS("Data/Term Paper/analysis_df.RDS")


### explore data


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


# create plot for appendix ------------------------------------------------

analysis_df %>% 
  filter(support_oppose_indicator == "S") %>% 
  group_by(name) %>% 
  summarise(spending = sum(total),
            rep_share = mean(rep_adv),
            objector = objector,
            gender  = gender) %>% 
  ggplot(aes(x = rep_share, y = spending, color = as.factor(objector))) + 
  geom_point() + scale_color_brewer(palette = "Dark2", labels = c("No", "Yes"), name = "Objection") +
  theme_ipsum() + 
  labs(title = "Independent Expenditures vs. Vote Margin of Candidate",
       subtitle = "Unit under observation: Republican members of the House",
       caption = "Note: Only independent expenditures in support of specific candidate are presented") +
  xlab("Vote margin of candidate") +
  ylab("Independent Expenditures") +
  theme(legend.position = "right",
        legend.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
        plot.title.position = "plot") +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  guides(color = guide_legend(reverse = T)) -> spending_graph


ggsave(filename = "spending.png", plot = spending_graph, device = "png", 
       path = "graphs/",width=10, height=8)


# create plot for differences in group spending ---------------------------


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
  geom_point( aes(x=rank, y=percentage, color = ), color = "#D95F02", size=3 ) +
  geom_point( aes(x=rank, y=percentage_non_objectors), color = "#1B9E77", size=3 ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  coord_flip() +
  scale_x_continuous(
    breaks = lollipop_1_df$rank, # specify tick breaks using rank column
    labels = lollipop_1_df$committee_name # specify tick labels using x column
  ) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5)) +
  theme_ipsum() +
  labs(title = "Percentage of Republican lawmaker supported by a PAC",
     subtitle = "Unit under observation: Republican members of the House",
     caption = "Note: Only PACs are considered that sponsored at least two candidates.") +
  xlab("Contributors") +
  ylab("Percentage of lawmakers") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(1.25, "cm"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
        plot.title.position = "plot") -> lollipo_graph


ggsave(filename = "lollipop.png", plot = lollipo_graph, device = "png", 
       path = "graphs/",width=10, height=8)


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
  select(district, bioname, nominate_dim1, nominate_dim2, nokken_poole_dim1, nokken_poole_dim2,
         nominate_number_of_errors, nominate_geo_mean_probability) -> dw_scores_df
  
  
analysis_df %>% 
  left_join(dw_scores_df, by = "district") -> analysis_df

# create dw nominate plot -------------------------------------------------

analysis_df %>% 
  filter(support_oppose_indicator == "S") %>% 
  group_by(name)  %>% 
  ggplot(aes(x = nominate_dim1.x, y = as.factor(objector), fill = as.factor(objector))) +
  geom_violin(trim = F) +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_brewer(palette="Dark2", labels = c("No", "Yes")) + 
  theme_ipsum() + 
  labs(title = "Comparism of DW-Nominate scores", fill = "Objection",
       subtitle = "Unit under observation: Republican members of the House",
       caption = "Number of objectors: 126\nNumber of non-objectors: 69\nNote: DW-Nominate scores range from -1 (liberal) to 1 (conservative)") +
  xlab("DW-Nominate Score") +
  ylab("Objection") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(1.25, "cm"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12)) +
  guides(fill = guide_legend(label.position = "bottom",
                             title.position = "top",
                             reverse = T)) + 
  scale_y_discrete(labels = c("No", "Yes")) -> nominate_graph




ggsave(filename = "nominate.png", plot = nominate_graph, device = "png", 
       path = "graphs/",width=10, height=8)





# plot most spending by pac -----------------------------------------------

analysis_df %>% 
  filter(support_oppose_indicator == "S") %>% 
  group_by(committee_name) %>% 
  summarise(spending = sum(total)) %>% 
  arrange(desc(spending)) %>% 
  ungroup() %>% 
  slice_head(n = 10) -> support_pacs

analysis_df %>% 
  filter(support_oppose_indicator == "S") %>% 
  semi_join(support_pacs) %>% 
  group_by(committee_name, objector) %>% 
  summarise(spending = sum(total)) %>% 
  arrange(desc(spending)) %>% 
  mutate(committee_name = forcats::fct_reorder(committee_name, spending)) %>%
  ggplot(aes(x = reorder(committee_name, spending), y = spending, fill = as.factor(objector))) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_dollar(scale = 1/1000000, suffix = "M")) +
  scale_fill_brewer(palette="Dark2", labels = c("No", "Yes"), name = "Objectors") +
  theme_ipsum() +
  xlab("Contributors") +
  ylab("Independent Expenditures in million $") +
  theme(legend.position = "right",
        legend.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 14),
        #legend.spacing.x = unit(0.5, "cm"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, hjust = 0.5),
        #axis.text.x = element_text(angle = -45),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
        plot.title.position = "plot") +
  guides(fill = guide_legend(label.position = "right",
                             title.position = "top",
                             reverse = T)) +
  labs(title = "Independent expenditures of top 10 PACs",
       subtitle = "Donating to Republican members of the House",
       caption = "Note: Only independent expenditures in support of candidate") -> top_10_plot


ggsave(filename = "top10.png", plot = top_10_plot, device = "png", 
       path = "graphs/",width=10, height=8)


# open secrects data ------------------------------------------------------

polite::bow("https://www.opensecrets.org/outsidespending/cycle_tots.php") %>% 
  polite::scrape() -> outside_spending

outside_spending %>% 
  html_nodes(".datadisplay") %>% 
  html_table() %>% 
  pluck(1) %>% 
  mutate(new = str_match_all(Total, "\\d")) %>% 
  mutate(new = sapply(new, toString)) %>% 
  mutate(new = str_remove_all(new, ",")) %>% 
  mutate(new = str_replace_all(new, " ", "")) %>% 
  mutate(new = as.numeric(new)) -> outside_spending

cost_of_election <- read_csv("Data/Term Paper/cost_of_election.csv")

cost_of_election %>% 
  rename(total = 2) %>% 
  mutate(total = str_remove(total, fixed("$"))) %>% 
  mutate(total = as.numeric(total)) %>% 
  mutate(Cycle = str_remove(Cycle, fixed("*"))) %>% 
  mutate(Cycle = as.numeric(Cycle)) -> cost_of_election


outside_spending %>% 
  left_join(cost_of_election) %>% 
  select(Cycle, new, total) %>% 
  rename("outside_spending" = "new") -> merged_election_cost

merged_election_cost %>% 
  filter(Cycle >= 2000) %>% 
  mutate(rest = total - outside_spending) %>% 
  pivot_longer(cols = c(outside_spending, rest)) -> election_cost_long


election_cost_long %>% 
  ggplot(aes(y = value, x = Cycle, fill = name)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum()

