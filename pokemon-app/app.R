library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(dplyr)
library(tidyr)
library(plotly)

#rsconnect::writeManifest()

source('get-api-data.R')

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      /* ── General ── */
      body { background-color: #f4f6f9; }

      /* ── Navbar ── */
      .navbar { margin-bottom: 20px; }

      /* ── Summary cards (value boxes) ── */
      .summary-box {
        background: #ffffff;
        border-radius: 8px;
        padding: 20px;
        text-align: center;
        box-shadow: 0 2px 6px rgba(0,0,0,.08);
        margin-bottom: 16px;
      }
      .summary-box .sb-value {
        font-size: 2.5rem;
        font-weight: 700;
        color: #2c3e50;
      }
      .summary-box .sb-label {
        font-size: 1.5rem;
        color: #7f8c8d;
        text-transform: uppercase;
        letter-spacing: .05em;
      }

      /* ── Section headers ── */
      .section-header {
        border-left: 4px solid #18bc9c;
        padding-left: 5px;
        margin: 24px 0 14px;
        font-size: 1.5rem;
        font-weight: 600;
        color: #2c3e50;
      }

      /* ── Card deep-dive panel ── */
      .card-panel {
        background: #ffffff;
        border-radius: 10px;
        padding: 24px;
        box-shadow: 0 2px 8px rgba(0,0,0,.08);
        margin-bottom: 20px;
      }
      .card-image img {
        border-radius: 10px;
        max-width: 100%;
        box-shadow: 0 4px 12px rgba(0,0,0,.15);
      }
      .set-logo img {
        display: block;        /* Makes margin auto work on img */
        margin: 0 auto;        /* Centers horizontally */
        max-height: 60px;
        margin-bottom: 10px;
      }
      .detail-label {
        font-size: 1.2rem;
        text-transform: uppercase;
        letter-spacing: .06em;
        color: #95a5a6;
        margin-bottom: 2px;
      }
      .detail-value {
        font-size: 1.5rem;
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 14px;
      }

      /* ── Price tiles ── */
      .price-tile {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 14px 10px;
        text-align: center;
        margin-bottom: 10px;
      }
      .price-tile .pt-label {
        font-size: 1.3rem;
        text-transform: uppercase;
        color: #95a5a6;
      }
      .price-tile .pt-value {
        font-size: 2.5rem;
        font-weight: 700;
      }
      .pt-market { color: #18bc9c; }
      .pt-low    { color: #3498db; }
      .pt-mid    { color: #f39c12; }
      .pt-high   { color: #e74c3c; }

      /* ── Missing-data alert ── */
      .missing-alert {
        background: #fef9e7;
        border: 1px solid #f39c12;
        border-radius: 8px;
        padding: 14px 18px;
        margin-bottom: 16px;
      }
      .missing-alert.ok {
        background: #eafaf1;
        border-color: #18bc9c;
      }

      /* ── Data tab ── */
      .datatable-panel {
        background: #ffffff;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,.08);
      }
    "))
  ),
  
  navbarPage(
    title = "🎴 Pokémon Market Price Tracker",
    
    # ── TAB 1: Summary ─────────────────────────────────────────────────────
    tabPanel(
      "Summary",
      icon = icon("chart-bar"),
      
      fluidRow(
        column(12,
               h4("Pokemon Collection Overview",
                  style = "margin-top:10px; font-weight:700; color:#2c3e50;")
        )
      ),
      
      # Value boxes row
      fluidRow(
        column(3, uiOutput("sb_total_cards")),
        column(3, uiOutput("sb_unique_sets")),
        column(3, uiOutput("sb_missing_price")),
        column(3, uiOutput("sb_last_updated"))
      ),
      
      # Missing price alert
      fluidRow(
        column(12, uiOutput("missing_price_alert"))
      ),
      
      # Breakdown tables side by side
      fluidRow(
        column(12,
               div(class = "section-header", "Collection Breakdown By Set and Rarity"),
               div(class = "card-panel",plotlyOutput("bar_plot_by_set"))
        )
      ),
      
      # fluidRow(
      #   column(6,
      #          div(class = "section-header", "Cards by Pokémon Type"),
      #          div(class = "card-panel", DTOutput("tbl_by_type"))
      #   ),
      #   column(6,
      #          div(class = "section-header", "Price Summary"),
      #          div(class = "card-panel", DTOutput("tbl_price_summary"))
      #   )
      # ),
      
      # Top 10 most expensive
      fluidRow(
        column(12,
               div(class = "section-header", "Top 10 Most Expensive Cards (Market Price)"),
               div(class = "card-panel", DTOutput("tbl_top10"))
        )
      )
    ),
    
    # ── TAB 2: Card Deep Dive ───────────────────────────────────────────────
    tabPanel(
      "Card Deep Dive",
      icon = icon("magnifying-glass"),
      
      # Filters
      fluidRow(
        column(4,
               div(class = "card-panel",
                   pickerInput(
                     inputId  = "filter_set",
                     label    = "Select Set",
                     choices  = sort(unique(clean_df$set)),
                     selected = sort(unique(clean_df$set))[1],
                     options  = list(
                       `live-search` = TRUE,
                       size = 10
                     )
                   )
               )
        ),
        column(4,
               div(class = "card-panel",
                   pickerInput(
                     inputId  = "filter_card",
                     label    = "Select Card",
                     choices  = NULL,   # populated server-side
                     options  = list(
                       `live-search` = TRUE,
                       size = 10
                     )
                   )
               )
        )
      ),
      
      fluidRow(
        # Card display
        column(12,
               uiOutput("card_deep_dive")
        )
      )
    ),
    
    # ── TAB 3: Full Data Table ──────────────────────────────────────────────
    tabPanel(
      "Data Table",
      icon = icon("table"),
      
      fluidRow(
        column(12,
               div(class = "datatable-panel",
                   downloadButton("download_csv", "Download CSV",
                                  class = "btn-success",
                                  style = "margin-bottom:14px;"),
                   DTOutput("cards_table")
               )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── Helpers ────────────────────────────────────────────────────────────────
  
  fmt_currency <- function(x) {
    ifelse(is.na(x), "N/A", paste0("$", formatC(x, format = "f", digits = 2, big.mark = ",")))
  }
  
  summary_box <- function(value, label, icon_name = NULL) {
    div(class = "summary-box",
        div(class = "sb-value", value),
        div(class = "sb-label", label)
    )
  }
  
  # ── Reactive: filter card list when set changes ─────────────────────────────
  observeEvent(input$filter_set, {
    cards_in_set <- clean_df %>%
      filter(set == input$filter_set) %>%
      pull(card) %>%
      unique() %>%
      sort()
    
    updatePickerInput(session,
                      inputId  = "filter_card",
                      choices  = cards_in_set,
                      selected = cards_in_set[1])
  })
  
  selected_card_data <- reactive({
    req(input$filter_set, input$filter_card)
    clean_df %>%
      filter(set == input$filter_set, card == input$filter_card) %>%
      slice(1)
  })
  
  # ── TAB 1: Summary value boxes ──────────────────────────────────────────────
  
  output$sb_total_cards <- renderUI({
    summary_box(
      value = format(nrow(clean_df), big.mark = ","),
      label = "Total Cards"
    )
  })
  
  output$sb_unique_sets <- renderUI({
    summary_box(
      value = n_distinct(clean_df$set),
      label = "Unique Sets"
    )
  })
  
  output$sb_missing_price <- renderUI({
    total_value <- sum(clean_df$market_price, na.rm = TRUE)
    summary_box(
      value = fmt_currency(total_value),
      label = "Total Market Value"
    )
  })
  
  output$sb_last_updated <- renderUI({
    last_update <- max(as.Date(clean_df$updated), na.rm = TRUE)
    summary_box(
      value = format(last_update, "%b %d, %Y"),
      label = "Most Recent Update"
    )
  })
  
  # Missing price alert
  output$missing_price_alert <- renderUI({
    n_missing <- sum(is.na(clean_df$market_price))
    
    if (n_missing == 0) {
      div(class = "missing-alert ok",
          tags$b("✅ All cards have market price data."),
          " No missing values detected."
      )
    } else {
      missing_cards <- clean_df %>%
        filter(is.na(market_price)) %>%
        pull(card) %>%
        #head(5) %>%
        paste(collapse = ", ")
      
      div(class = "missing-alert",
          tags$b(paste0("⚠️  ", n_missing, " card(s) are missing market price data.")),
          tags$br(),
          tags$span(style = "font-size:1.2rem; color:#7f8c8d;",
                    paste0("", missing_cards, '.')
                    #if (n_missing > 5) paste0(" … and ", n_missing - 5, " more.") else ".")
          )
      )
    }
  })
  
  output$bar_plot_by_set <- renderPlotly ({
    plot <- clean_df %>%
      group_by(set, rarity) %>%
      summarize(n_cards = n(), .groups = 'drop') %>%
      mutate(set = fct_reorder(set, n_cards, .desc = TRUE)) %>%
      ggplot(aes(x = set, y = n_cards, fill = rarity)) +
      geom_col(
        color = "black",
        linewidth = 0.5 
      ) +
      scale_fill_manual(values = c("SIR" = "steelblue3", "IR" = "azure4", "Promo" = "royalblue4")) +
      theme_minimal() +
      labs(y = "# of Cards", fill = 'Card Rarity') +
      theme(axis.text.x = element_text(angle = 25, hjust = 1), axis.title.x = element_blank())
    
    ggplotly(plot)
  })
  
  
  output$tbl_by_type <- renderDT({
    clean_df %>%
      count(pokemon_type, name = "Cards") %>%
      arrange(desc(Cards)) %>%
      rename(`Pokémon Type` = pokemon_type) %>%
      datatable(options = list(pageLength = 8, dom = "tp"),
                rownames = FALSE, class = "cell-border stripe")
  })
  
  output$tbl_price_summary <- renderDT({
    clean_df %>%
      summarise(
        `Avg Market`  = fmt_currency(mean(market_price, na.rm = TRUE)),
        `Median Market` = fmt_currency(median(market_price, na.rm = TRUE)),
        `Min Market`  = fmt_currency(min(market_price, na.rm = TRUE)),
        `Max Market`  = fmt_currency(max(market_price, na.rm = TRUE)),
        `Avg Low`     = fmt_currency(mean(low_price, na.rm = TRUE)),
        `Avg High`    = fmt_currency(mean(high_price, na.rm = TRUE))
      ) %>%
      tidyr::pivot_longer(everything(),
                          names_to  = "Metric",
                          values_to = "Value") %>%
      datatable(options = list(pageLength = 8, dom = "t"),
                rownames = FALSE, class = "cell-border stripe")
  })
  
  output$tbl_top10 <- renderDT({
    clean_df %>%
      arrange(desc(market_price)) %>%
      slice_head(n = 10) %>%
      select(card, set, rarity, pokemon_type, market_price, low_price, high_price) %>%
      rename(
        Card          = card,
        Set           = set,
        Rarity        = rarity,
        `Type`        = pokemon_type,
        `Market ($)`  = market_price,
        `Low ($)`     = low_price,
        `High ($)`    = high_price
      ) %>%
      datatable(options = list(pageLength = 10, dom = "t", scrollX = TRUE),
                rownames = FALSE, class = "cell-border stripe") %>%
      formatCurrency(c("Market ($)", "Low ($)", "High ($)"), currency = "$")
  })
  
  # ── TAB 2: Card Deep Dive ───────────────────────────────────────────────────
  
  output$card_deep_dive <- renderUI({
    cd <- selected_card_data()
    req(nrow(cd) > 0)
    
    # ── helper: one detail row
    detail <- function(label, value) {
      tagList(
        div(class = "detail-label", label),
        div(class = "detail-value",
            if (is.na(value) || value == "") "—" else as.character(value))
      )
    }
    
    # ── price tile
    price_tile <- function(label, value, css_class) {
      div(class = "price-tile",
          div(class = "pt-label", label),
          div(class = paste("pt-value", css_class), fmt_currency(value))
      )
    }
    
    # ── TCGPlayer link
    tcg_link <- if (!is.na(cd$tcg_player_url) && nchar(cd$tcg_player_url) > 0) {
      tags$a(href = cd$tcg_player_url, target = "_blank",
             class = "btn btn-primary btn-sm",
             "View on TCGPlayer ↗")
    } else {
      tags$span(style = "color:#95a5a6; font-size:0.85rem;", "No TCGPlayer link available")
    }
    
    div(class = "card-panel",
        
        # Set logo + card name header
        fluidRow(
          column(2,
                 if (!is.na(cd$set_logo) && nchar(cd$set_logo) > 0)
                   div(class = "set-logo",
                       tags$img(src = cd$set_logo, alt = paste(cd$set, "logo")))
                 else
                   NULL),
          column(10,
                 h3(cd$card,
                    style = "font-weight:700; color:#2c3e50; margin-top:4px; margin-bottom:4px;"),
                 tags$span(style = "color:#7f8c8d; font-size:1.2rem;",
                           paste0(cd$set, "  •  #", cd$number)),
                 #tags$hr()
          )
        ),
        fluidRow( tags$hr() ),
        
        fluidRow(
          
          # Card image
          column(4,
                 div(class = "card-image",
                     if (!is.na(cd$image) && nchar(cd$image) > 0)
                       tags$img(src = cd$image, alt = cd$card)
                     else
                       div(style = "background:#ecf0f1; border-radius:10px; height:300px;
                           display:flex; align-items:center; justify-content:center;
                           color:#bdc3c7; font-size:0.9rem;",
                           "No image available")
                 ),
                 tags$br(),
                 tcg_link
          ),
          
          # Details + prices
          column(8,
                 
                 # Price tiles
                 div(class = "section-header", style = "margin-top:0;", "Price Breakdown"),
                 fluidRow(
                   column(3, price_tile("Market",  cd$market_price, "pt-market")),
                   column(3, price_tile("Low",     cd$low_price,    "pt-low")),
                   column(3, price_tile("Mid",     cd$mid_price,    "pt-mid")),
                   column(3, price_tile("High",    cd$high_price,   "pt-high"))
                 ),
                 
                 tags$hr(),
                 
                 # Card details
                 div(class = "section-header", "Card Metadata"),
                 fluidRow(
                   column(6,
                          detail("Rarity",          cd$rarity),
                          detail("Pokémon Type",    cd$pokemon_type),
                          detail("Classification",  cd$classification),
                          detail("Stage",           cd$stage),
                          detail("Evolution Line",  cd$evolution_line),
                   ),
                   column(6,
                          detail("Generation Introduced",cd$generation_introduced),
                          detail("Starter Pokémon",
                                 ifelse(cd$is_starter == 'Yes', "Yes ⭐", "No") ),
                          detail("Eeveelution",
                                 ifelse(cd$is_eeveelution == 'Yes', "Yes ⭐", "No")  ),
                          detail("Trainer Card",
                                 ifelse(cd$is_trainer_card == 'Yes', "Yes ⭐", "No") )
                   )
                 ),
                 
                 tags$hr(),
                 
                 # Artist + last updated
                 div(class = "section-header", "Additional Info"),
                 fluidRow(
                   column(6,
                          detail("Artist",       cd$artist),
                          detail("Card Number",  cd$number)
                   ),
                   column(6,
                          detail("TCGDex ID",    cd$tcgdex_id),
                          detail("Last Updated",
                                 if (!is.na(cd$updated))
                                   format(as.Date(cd$updated), "%b %d, %Y") else NA)
                   )
                 )
          )
        )
    )
  })
  
  # ── TAB 3: Full Data Table ──────────────────────────────────────────────────
  
  output$cards_table <- renderDT({
    clean_df %>%
      select(-c(set_logo, image, tcg_player_url)) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX    = TRUE,
          autoWidth  = TRUE
        ),
        escape    = FALSE,
        rownames  = FALSE,
        class     = "cell-border stripe"
      ) %>%
      formatCurrency(
        c("market_price", "low_price", "mid_price", "high_price"),
        currency = "$"
      )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("pokemon_cards_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(clean_df, file, row.names = FALSE)
    }
  )
}

# ── Launch ────────────────────────────────────────────────────────────────────

shinyApp(ui, server)