
# install.packages("httr") ## httr2 too new for my current version i think
# install.packages("jsonlite")

library(httr)
library(jsonlite)
library(data.table)
library(googlesheets4)
library(dplyr)
library(janitor)
library(tidyverse)



# Look at all sets to get the Abbreviations needed to identify Andrews cards ---
req <- GET("https://api.tcgdex.net/v2/en/sets")

sets_data <- fromJSON(rawToChar(req$content))

sets_dataframe <- as.data.frame(sets_data)

sets_dataframe[which(sets_dataframe$name == 'Chaos Rising'),]

write.csv(sets_dataframe[,c(1,2)], "pokemon_sets_abbrv.csv", row.names = FALSE)

# Look at one set --------------------------------------------------------------
req <-GET("https://api.tcgdex.net/v2/en/sets/sv08.5")

sv08_5_data <- fromJSON(rawToChar(req$content))

sv08_5_dataframe <- as.data.frame(sv08_5_data)


# Read in Google Sheet Andrew Made With new Card IDs ---------------------------
gs4_deauth()
andrew_df <- read_sheet("https://docs.google.com/spreadsheets/d/1yKbs_aZr75_uabxtx7XiEBG1RJ_spL2iNTyYttZuG5M/edit?usp=sharing")

andrew_df <- andrew_df %>% clean_names()

andrew_df_clean <- andrew_df[!is.na(andrew_df$set_id),]

andrew_df_clean <- andrew_df_clean %>%
  mutate(tcgdex_id_new = paste0(set_id, "-", str_extract(number, "^[^/]+")))

# IGNORE CARD LEVEL FOR RIGHT NOW ----------------------------------------------


id <- "sv10.5w-094"
print(id)
url <- paste0("https://api.tcgdex.net/v2/en/cards/", id)

req <- GET(url)

req$status_code

single_card <- fromJSON(
  content(req, as = "text", encoding = "UTF-8"),
  flatten = TRUE
)

cols <- list(
  id = c("id"),
  name = c("name"),
  set = c("set", "name"),
  card_number = c("localId"),
  set_size = c("set", "cardCount", "official"),
  rarity = c("rarity"),
  illustrator = c("illustrator"),
  evolve_from = c("evolveFrom"),
  stage = c("stage"),
  market_price = c("pricing", "tcgplayer", "holofoil", "marketPrice"),
  low_price = c("pricing", "tcgplayer", "holofoil", "lowPrice"),
  mid_price = c("pricing", "tcgplayer", "holofoil", "midPrice"),
  high_price = c("pricing", "tcgplayer", "holofoil", "highPrice"),
  updated =  c("pricing", "tcgplayer", "updated")
)

card_to_df <- function(card, columns) {
  
  # Helper to extract nested values
  get_nested <- function(x, path) {
    Reduce(function(a, b) {
      if (is.null(a)) return(NULL)
      a[[b]]
    }, path, init = x)
  }
  
  values <- lapply(columns, function(path) {
    value <- get_nested(card, path)
    
    if (is.null(value)) {
      return(NA)
    }
    
    # Collapse vectors into a comma-separated string
    if (length(value) > 1 && !is.data.frame(value) && !is.list(value)) {
      return(paste(value, collapse = ", "))
    }
    
    # Leave single values alone
    if (length(value) == 1) {
      return(value)
    }
    
    # Ignore data.frames/lists unless specifically handled
    return(NA)
  })
  
  names(values) <- names(columns)
  
  as.data.frame(values, stringsAsFactors = FALSE)
}

get_card_df <- function(id) {
  tryCatch({
    url <- paste0("https://api.tcgdex.net/v2/en/cards/", id)
    
    req <- GET(url)
    
    card <- fromJSON(
      content(req, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )
    if(req$status_code == 200){
      list(card_df = card_to_df(card, cols), no_info = character(0))
    } else{
      list(card_df = NULL, no_info = id)
    }
  }, error = function(e) {
    message("Failed: ", id)
    list(card_df = NULL, no_info = id)
  })
}

single_card_dataframe <- card_to_df(single_card, cols)


results <- lapply(andrew_df_clean$tcgdex_id_new, get_card_df)

cards_df <- bind_rows(lapply(results, `[[`, "card_df"))

missing_df <- data.frame(
  id = unlist(lapply(results, `[[`, "no_info")),
  stringsAsFactors = FALSE
)


cards_df_clean <- cards_df[rowSums(!is.na(cards_df)) > 0, ]

duplicate_rows <- cards_df_clean %>%
  group_by(id) %>%
  filter(n() > 1) %>%
  arrange(id)

duplicate_rows


length(unique(andrew_df_clean$tcgdex_id))

summary(cards_df_clean$market_price)

names(cards_df_clean)
