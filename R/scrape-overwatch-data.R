## Web scraping
library(rvest)
library(jsonlite)
library(stringr)
library(tidyverse)

data <- list()

first_response <- read_html('https://overwatchtracker.com/leaderboards/psn/global/CompetitiveRank?mode=1') %>%
  html_node('.content-container') %>%
  html_nodes('table') %>%
  html_table(fill=TRUE)

first_response[[1]]$page_number <- 1
data[[1]] <- first_response[[1]]

tracker_url_tmp = 'https://overwatchtracker.com/leaderboards/psn/global/CompetitiveRank?page=%i&mode=1'

for (page in seq(2,1000)){
  response <- read_html(sprintf(tracker_url_tmp, page)) %>%
    html_node('.content-container') %>%
    html_nodes('table') %>%
    html_table(fill=TRUE)
  
  if (is.list(response) & length(response) == 0){
    break
  }
  response[[1]]$page_number <- page
  data[[page]] <- response[[1]]
}

response[[1]]
# for each gamer in df above, convert their name to insert into URL call
length(data)
summary_df <- do.call(rbind, data)
# summary_df %>% filter(grepl('if \\(window', Rank)) %>% dim
# write_csv(summary_df %>% filter(!grepl('if \\(window', Rank)), 'data/summary-data.csv')

summary_df %>%
  filter(!grepl('if \\(window', Rank)) %>%
  mutate(newlines = str_count(Gamer, '\n'),
         Rank = as.numeric(Rank),
         newlines_sr = str_extract(`Skill Rating`, '\\d,\\d{3}')) %>%
  filter(newlines == 1) %>%
  dim()


# clean and mutate
cleaned_summary <- summary_df %>%
  filter(!grepl('if \\(window', Rank)) %>%
  mutate(newlines = str_count(Gamer, '\n'),
         rank = parse_number(Rank),
         gamer_name = word(Gamer, 1, sep = fixed('\n')),
         skill_rating = str_extract(`Skill Rating`, '\\d,\\d{3}') %>%
           gsub(',','' ,.) %>% as.numeric(),
         games_played = parse_number(`# Games`)
  ) %>%
  select(rank, gamer_name, skill_rating, games_played) %>% 
  distinct()

write_csv(cleaned_summary, 'data/clean-summary-data-2.csv')


ow_df %>% select(ga) %>% head()
ow_df %>% select(name) %>% head()

mean(cleaned_summary$gamer_name %in% (ow_df$name %>% trimws()))


'%!in%' <- function(x,y)!('%in%'(x,y))
cleaned_summary$gamer_name[cleaned_summary$gamer_name %!in% (ow_df$name %>% trimws())]

unique(ow_df$name) %>% length

ow_df %>% 
  distinct() %>% 
  group_by(name) %>% 
  summarise(n=n()) %>% 
  filter(n>1)

ow_df %>% select(-starts_with('competitive')) %>% filter(name=='KKalon') %>% 
  distinct()


total_df <- ow_df %>%
  inner_join(cleaned_summary, by=c('name'='gamer_name'))

total_df_2 <- total_df %>% filter(!is.na(skill_rating))

write_csv(total_df_2, 'data/total-data-2.csv')

total_df <- total_df %>% type.convert()

total_df %>% head()

plot(total_df$skill_rating, log(total_df$competitiveStats.careerStats.allHeroes.assists.healingDone))

hist(total_df_2$skill_rating)
shapiro.test(total_df$skill_rating)
qqnorm(total_df_2$skill_rating)
