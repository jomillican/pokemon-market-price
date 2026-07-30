
library(httr)
library(jsonlite)
library(data.table)
library(googlesheets4)
library(dplyr)
library(janitor)
library(tidyverse)
library(lubridate)

# Get All Set Abbreviations ----------------------------------------------------
req <- GET("https://api.tcgdex.net/v2/en/sets")

sets_data <- fromJSON(rawToChar(req$content))

abbreviations <- as.data.frame(sets_data)

# Read in Google Sheet Andrew Made With new Card IDs ---------------------------
gs4_deauth()
url <- "https://docs.google.com/spreadsheets/d/1-ZK5xb-zAsddRM5spovJpgV0HK0_ntaHFzjWEfqaCLE/edit?gid=0#gid=0"
andrew_df <- read_sheet(url)

andrew_df <- andrew_df %>% 
  clean_names() %>% 
  mutate(across(everything(), as.character))

andrew_df <- andrew_df %>% mutate(across(everything(), ~ na_if(.x, "n/a")))

andrew_df <- andrew_df %>%
  left_join((abbreviations %>% select(id, name, logo, symbol)), by = c('set'='name'))

andrew_df <- andrew_df %>%
  mutate(tcgdex_id = paste0(id, "-", str_extract(number, "^[^/]+"))) %>%
  mutate(evolution_line = gsub(' Line', '', evolution_line)) %>%
  mutate(classification = gsub('Yes, ', '', legendary_mythical_pseudo_ultra_beast_paradox_fossil_yes_no))

andrew_df <- andrew_df %>%
  mutate(set_logo = ifelse(is.na(logo), NA, paste0(logo, '.webp'))) %>%
  select(
    tcgdex_id,
    card,
    set,
    number,
    rarity,
    artist,
    pokemon_type,
    classification,
    stage,
    evolution_line,
    generation_introduced,
    is_starter = starter_yes_no,
    is_eeveelution = eeveelution_yes_no,
    is_trainer_card = trainer_card_yes_no,
    tcg_player_url,
    set_logo
  )


# Columns I want From the API
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
  updated =  c("pricing", "tcgplayer", "updated"),
  image = c("image")
)

# Turns Card Into a Data Frame
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

# Grabs Card From API -> Stores Data Frame Version or Added to List of Missing
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

# Get Every Card and put it into a dataframe format
results <- lapply(andrew_df$tcgdex_id, get_card_df)

# Put all the Cards into a dataframe
cards_df <- bind_rows(lapply(results, `[[`, "card_df"))

clean_df <- andrew_df %>%
  left_join(
    (cards_df %>% select(tcgdex_id = id, image, market_price, low_price, mid_price, high_price, updated)),
    by = 'tcgdex_id')

clean_df <- clean_df %>%
  mutate(image = ifelse(is.na(image), NA, paste0(image, '/high.webp'))) %>%
  mutate(tcg_player_url = paste0('<a href="', tcg_player_url, '" target="_blank">', 'TCG Player', '</a>')) %>%
  mutate(updated = as.Date(ymd_hms(updated, tz = "UTC")))






# UPDATE GOOGLE SHEET WITH MARKET PRICE DATA DAILY -----------------------------


# Authenticate invisibly using the JSON path
gs4_auth(path = Sys.getenv("GOOGLE_SHEETS_JSON_PATH"))

sheet_write(data = clean_df, ss = url, sheet = "Card Data With Market Price")
