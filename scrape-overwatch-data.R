## Web scraping
library(rvest)
library(jsonlite)
library(stringr)
library(tidyverse)

# ow <- read_html('https://overwatchtracker.com/profile/pc/global/Doge-21561')
# ow %>% html_node('.infobox') %>% str_subset(., 'viewers')
#
#
# ow2 <- read_html('https://ovrstat.com/stats/pc/us/Viz-1213')
# json_ow <- fromJSON('https://ovrstat.com/stats/pc/us/Viz-1213')
# json_ow$name
#
# ow3 <- read_html('https://overwatchtracker.com/leaderboards/pc/global/CompetitiveRank?mode=1')
# tmp <- ow3 %>% html_node('.content-container') %>% html_nodes('table') %>%
#   html_table(fill=TRUE)
#
# https://overwatchtracker.com/leaderboards/psn/global/CompetitiveRank?page=2&mode=1
#

# for (page in seq(2,10)){
#   print(sprintf(url_tmp, page))
# }

data <- list()

first_response <- read_html('https://overwatchtracker.com/leaderboards/psn/global/CompetitiveRank?mode=1') %>%
  html_node('.content-container') %>%
  html_nodes('table') %>%
  html_table(fill=TRUE)

first_response[[1]]$page_number <- 1
data[[1]] <- first_response[[1]]

tracker_url_tmp = 'https://overwatchtracker.com/leaderboards/psn/global/CompetitiveRank?page=%i&mode=1'

for (page in seq(2,20)){
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

# for each gamer in df above, convert their name to insert into URL call
length(data)
summary_df <- do.call(rbind, data)
summary_df %>% filter(grepl('if \\(window', Rank)) %>% dim
write_csv(summary_df %>% filter(!grepl('if \\(window', Rank)), 'data/summary-data.csv')

summary_df %>%
  filter(!grepl('if \\(window', Rank)) %>%
  mutate(newlines = str_count(Gamer, '\n'),
         Rank = as.numeric(Rank),
         newlines_sr = str_extract(`Skill Rating`, '\d,\d{3}')) %>%
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
  select(rank, gamer_name, skill_rating, games_played)

write_csv(cleaned_summary, 'data/clean-summary-data.csv')

# look up on ovrstat.com
ovrstat_url_tmp <- 'https://ovrstat.com/stats/psn/%s'
ovrstat_data = list()
num_gamers = length(cleaned_summary$gamer_name)

# Start the clock!
ptm <- proc.time()

for (i in 1:num_gamers){
  tryCatch({
    gamer = cleaned_summary$gamer_name[i]
    # print(sprintf(ovrstat_url_tmp, gamer))
    json_response <- fromJSON(sprintf(ovrstat_url_tmp, gamer))
    if (json_response$private) stop('profile is private')
    table_response <- enframe(unlist(json_response))
    df <- table_response %>%
      filter(!grepl('quickPlayStats', name)) %>%
      spread(name, value)
    
    ovrstat_data[[i]] <- df
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Stop the clock
proc.time() - ptm

ow_df <- bind_rows(ovrstat_data)
ow_df %>% dim()

write_csv(ow_df, 'data/ow-data.csv')


# WIP below ---------------------------------------------------------------


# sample parse json response
json_response <- rjson::fromJSON(file=sprintf(ovrstat_url_tmp, 'atanupamz'))
json_response$private
table_response <- enframe(unlist(json_response))
df <- table_response %>%
  filter(!grepl('quickPlayStats', name)) %>%
  spread(name, value)


tmp = read_csv('data/summary-data.csv')

tmp %>% head()
tmp3 %>% dim
tmp3 %>% head(20)

tmp[[1]]$Gamer
tmp[[1]]$`Skill Rating`
tmp[[1]]$`# Games`
head(tmp[[1]]) # this is a starting dataframe

# use that url call to try and get json
json_ow <- fromJSON('https://ovrstat.com/stats/pc/us/Viz-1213')

# then parase that json in a wide dataframe

# join back to first dataframe on Gamer == Name, make sure theyre' in same format first



# lst = c()
# length(lst)
# lst = append(lst, tmp[[1]]$Gamer)
# length(lst)
# lst = append(lst, tmp[[1]]$Gamer)
# length(lst)
# lst

# make structured data out of json
str(json_ow)
library(tidyverse)
glimpse(json_ow, max.level = 3, list.len = 4)

tmp2 <- enframe(unlist(json_ow))

tmp2 %>% head(20)
tmp2 %>% 
  filter(grepl('competitiveStats', name)) %>% 
  dim

glimpse(tmp2)
tmp_wide = spread(tmp2, name, value)
type.convert(tmp_wide) %>% glimpse()
