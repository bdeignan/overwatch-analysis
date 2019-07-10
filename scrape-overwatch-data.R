## Web scraping
library(rvest)
library(jsonlite)
library(stringr)

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
# url_tmp = 'https://overwatchtracker.com/leaderboards/pc/global/CompetitiveRank?country=United%%20States&page=%i&mode=1'
# 

# for (page in seq(2,10)){
#   print(sprintf(url_tmp, page))
# }

data <- list()

first_response <- read_html('https://overwatchtracker.com/leaderboards/pc/global/CompetitiveRank?mode=1') %>% 
  html_node('.content-container') %>%
  html_nodes('table') %>% 
  html_table(fill=TRUE)

data[[1]] <- first_response[[1]]

for (page in seq(2,1000)){
  response <- read_html(sprintf(url_tmp, page)) %>% 
    html_node('.content-container') %>%
    html_nodes('table') %>% 
    html_table(fill=TRUE)
  
  if (is.list(response) & length(response) == 0){
    break
  }
  
  data[[page]] <- response[[1]]
}

# for each gamer in df above, convert their name to insert into URL call
length(data)
summary_df <- do.call(rbind, data)
summary_df %>% dim
write_csv(summary_df, 'data/summary-data.csv')


# WIP below ---------------------------------------------------------------

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
