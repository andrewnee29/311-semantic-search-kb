library(shiny)
library(httr)
library(jsonlite)

# ============================================
# CONFIG
# ============================================
# Set COHERE_API_KEY in your .Renviron or shinyapps.io environment variables
# Never hardcode API keys in source code
readRenviron(".Renviron")
COHERE_API_KEY <- Sys.getenv("COHERE_API_KEY")
KB_PATH        <- "knowledge_base_ai.csv"


# ============================================
# KNOWLEDGE BASE
# ============================================
parse_csv_knowledge_base <- function(filepath) {
  if (!file.exists(filepath)) stop(paste("CSV file not found:", filepath))
  
  df <- read.csv(filepath, stringsAsFactors = FALSE)
  
  if (!all(c("question", "answer") %in% names(df))) {
    stop("CSV must contain 'question' and 'answer' columns")
  }
  
  kb <- list()
  for (i in seq_len(nrow(df))) {
    q <- trimws(as.character(df$question[i]))
    a <- trimws(as.character(df$answer[i]))
    if (nzchar(q) && nzchar(a) && !is.na(q) && !is.na(a)) {
      kb <- append(kb, list(list(question = q, answer = a)))
    }
  }
  
  if (length(kb) == 0) stop("No valid Q&A pairs found in CSV")
  return(kb)
}


# ============================================
# COHERE EMBEDDINGS
# ============================================
get_cohere_embeddings_batch <- function(texts, api_key = COHERE_API_KEY,
                                        batch_size = 96, max_retries = 3) {
  if (!nzchar(api_key)) return(NULL)
  
  all_embeddings <- list()
  num_batches    <- ceiling(length(texts) / batch_size)
  
  cat(sprintf("Processing %d texts in %d batch(es)...\n", length(texts), num_batches))
  
  for (batch_num in seq_len(num_batches)) {
    start_idx   <- (batch_num - 1) * batch_size + 1
    end_idx     <- min(batch_num * batch_size, length(texts))
    batch_texts <- texts[start_idx:end_idx]
    
    cat(sprintf("  Batch %d/%d (items %d-%d)...", batch_num, num_batches, start_idx, end_idx))
    
    success  <- FALSE
    attempts <- 0
    
    # Retry loop — R for loops ignore variable reassignment so we use repeat
    repeat {
      attempts <- attempts + 1
      if (attempts > max_retries) {
        cat(" failed after max retries\n")
        for (i in seq_along(batch_texts)) {
          all_embeddings[[length(all_embeddings) + 1]] <- NULL
        }
        break
      }
      
      result <- tryCatch({
        resp <- POST(
          "https://api.cohere.com/v1/embed",
          add_headers(
            "Authorization" = paste("Bearer", api_key),
            "Content-Type"  = "application/json"
          ),
          body = toJSON(list(
            model      = "embed-english-light-v3.0",
            texts      = batch_texts,
            input_type = "search_document"
          ), auto_unbox = TRUE),
          encode  = "json",
          timeout(60)
        )
        list(status = status_code(resp), content = content(resp, as = "parsed"))
      }, error = function(e) {
        list(status = 0, error = e$message)
      })
      
      if (!is.null(result$error)) {
        cat(sprintf(" error: %s, retrying...\n", result$error))
        Sys.sleep(2 ^ attempts)  # exponential backoff
        next
      }
      
      if (result$status == 200) {
        for (embedding in result$content$embeddings) {
          all_embeddings[[length(all_embeddings) + 1]] <- unlist(embedding)
        }
        cat(" done\n")
        if (batch_num < num_batches) Sys.sleep(0.5)
        success <- TRUE
        break
        
      } else if (result$status == 429) {
        wait <- 10 * attempts
        cat(sprintf(" rate limited, waiting %ds...\n", wait))
        Sys.sleep(wait)
        next
        
      } else {
        cat(sprintf(" HTTP %d, retrying...\n", result$status))
        Sys.sleep(2)
        next
      }
    }
  }
  
  return(all_embeddings)
}

get_cohere_embedding_single <- function(text, api_key = COHERE_API_KEY) {
  if (!nzchar(api_key)) return(NULL)
  
  tryCatch({
    resp <- POST(
      "https://api.cohere.com/v1/embed",
      add_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ),
      body = toJSON(list(
        model      = "embed-english-light-v3.0",
        texts      = list(text),
        input_type = "search_query"
      ), auto_unbox = TRUE),
      encode  = "json",
      timeout(30)
    )
    if (status_code(resp) == 200) {
      unlist(content(resp, as = "parsed")$embeddings[[1]])
    } else {
      NULL
    }
  }, error = function(e) NULL)
}


# ============================================
# SIMILARITY SEARCH
# ============================================
cosine_similarity <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

find_best_answer_ai <- function(user_question, min_confidence = 30) {
  if (!nzchar(trimws(user_question))) {
    return("Please ask a question about Somerville city services.")
  }
  
  user_embedding <- get_cohere_embedding_single(user_question)
  if (is.null(user_embedding)) {
    return("I'm having trouble processing your question. Please try again or call 311 at 617-666-3311.")
  }
  
  similarities  <- vapply(question_embeddings, function(e) {
    if (is.null(e)) return(0)
    cosine_similarity(user_embedding, e)
  }, numeric(1))
  
  confidences  <- similarities * 100
  good_matches <- which(confidences >= min_confidence)
  
  if (length(good_matches) == 0) {
    return("I don't have specific information about that. Call 311 (617-666-3311) or visit somervillema.gov.")
  }
  
  good_matches <- head(good_matches[order(confidences[good_matches], decreasing = TRUE)], 5)
  
  parts <- character(0)
  for (i in seq_along(good_matches)) {
    idx  <- good_matches[i]
    item <- kb[[idx]]
    conf <- confidences[idx]
    parts <- c(parts,
               sprintf("**Result %d** (%.0f%% match)", i, conf),
               paste("**Q:**", item$question),
               paste("**A:**", item$answer),
               ""
    )
  }
  
  paste(c(
    sprintf("Found %d result%s above %.0f%% confidence:",
            length(good_matches),
            ifelse(length(good_matches) == 1, "", "s"),
            min_confidence),
    "", parts
  ), collapse = "\n")
}

find_best_answer_keywords <- function(user_question, min_confidence = 30) {
  if (!nzchar(trimws(user_question))) {
    return("Please ask a question about Somerville city services.")
  }
  
  stop_words <- c("i","me","my","the","a","an","and","or","but","in","on",
                  "at","to","for","of","with","by","how","can","do","does",
                  "is","are","was","were","be","been","have","has","had",
                  "will","would","could","should")
  
  user_words <- unique(Filter(
    function(w) nchar(w) > 2 && !w %in% stop_words,
    strsplit(gsub("[^a-zA-Z0-9 ]", "", tolower(user_question)), " ")[[1]]
  ))
  
  if (length(user_words) == 0) {
    return("I don't have specific information about that. Call 311 (617-666-3311) or visit somervillema.gov.")
  }
  
  scores <- vapply(kb, function(item) {
    ql <- tolower(item$question)
    al <- tolower(item$answer)
    qm <- sum(sapply(user_words, grepl, x = ql))
    am <- sum(sapply(user_words, grepl, x = al))
    score <- (qm * 3) + (am * 2)
    if ((qm + am) / length(user_words) < 0.3) score <- score * 0.5
    score
  }, numeric(1))
  
  max_score   <- length(user_words) * 3
  confidences <- pmin(100, (scores / max_score) * 100)
  confidences[is.nan(confidences)] <- 0
  
  good_matches <- which(confidences >= min_confidence)
  
  if (length(good_matches) == 0) {
    return("I don't have specific information about that. Call 311 (617-666-3311) or visit somervillema.gov.")
  }
  
  good_matches <- head(good_matches[order(confidences[good_matches], decreasing = TRUE)], 5)
  
  parts <- character(0)
  for (i in seq_along(good_matches)) {
    idx  <- good_matches[i]
    item <- kb[[idx]]
    conf <- confidences[idx]
    parts <- c(parts,
               sprintf("**Result %d** (%.0f%% match)", i, conf),
               paste("**Q:**", item$question),
               paste("**A:**", item$answer),
               ""
    )
  }
  
  paste(c(
    sprintf("Found %d result%s above %.0f%% confidence:",
            length(good_matches),
            ifelse(length(good_matches) == 1, "", "s"),
            min_confidence),
    "", parts
  ), collapse = "\n")
}

find_best_answer <- function(user_question, min_confidence = 30) {
  if (use_ai) find_best_answer_ai(user_question, min_confidence)
  else        find_best_answer_keywords(user_question, min_confidence)
}


# ============================================
# STARTUP — load KB and embeddings
# ============================================
cat("Loading knowledge base...\n")
kb    <- parse_csv_knowledge_base(KB_PATH)
cat(sprintf("Loaded %d Q&A pairs\n", length(kb)))

embeddings_cache_file <- "embeddings_cache.rds"
question_embeddings   <- list()
use_ai                <- FALSE

if (file.exists(embeddings_cache_file)) {
  cat("Loading cached embeddings...\n")
  tryCatch({
    cached <- readRDS(embeddings_cache_file)
    all_questions <- sapply(kb, function(x) x$question)
    
    # Validate cache matches current KB
    if (length(cached$embeddings) == length(kb) &&
        identical(cached$questions, all_questions)) {
      question_embeddings <- cached$embeddings
      use_ai              <- TRUE
      cat(sprintf("Loaded %d cached embeddings\n", length(question_embeddings)))
    } else {
      cat("Cache mismatch — regenerating\n")
    }
  }, error = function(e) cat(sprintf("Cache load failed: %s\n", e$message)))
}

if (!use_ai && nzchar(COHERE_API_KEY)) {
  cat("Generating Cohere embeddings...\n")
  all_questions       <- sapply(kb, function(x) x$question)
  question_embeddings <- get_cohere_embeddings_batch(all_questions)
  
  n_success <- sum(!sapply(question_embeddings, is.null))
  cat(sprintf("Generated %d/%d embeddings\n", n_success, length(kb)))
  
  if (n_success > 0) {
    tryCatch({
      saveRDS(
        list(embeddings = question_embeddings,
             questions  = all_questions,
             timestamp  = Sys.time()),
        embeddings_cache_file
      )
      cat("Embeddings cached\n")
    }, error = function(e) cat(sprintf("Cache save failed: %s\n", e$message)))
    use_ai <- TRUE
  }
}

if (!use_ai) cat("Using keyword matching fallback\n")


# ============================================
# UI
# ============================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', sans-serif; background: #f4f6f9; }
    .chat-container {
      height: 500px; overflow-y: auto; padding: 12px;
      border: 1px solid #dde; border-radius: 8px; background: #fff;
    }
    .chat-message {
      margin: 8px 4px; padding: 10px 14px; border-radius: 12px;
      max-width: 85%; word-break: break-word;
    }
    .user-message  { background: #0055a5; color: #fff; margin-left: auto; float: right; clear: both; }
    .bot-message   { background: #f0f4f8; border: 1px solid #dde; float: left; clear: both; white-space: pre-wrap; }
    .clearfix      { clear: both; }
    .mode-badge    { font-size: 11px; padding: 2px 8px; border-radius: 10px; display: inline-block; margin-top: 4px; }
    .mode-ai       { background: #e8f5e9; color: #2e7d32; }
    .mode-keyword  { background: #fff8e1; color: #f57f17; }
  "))),
  
  titlePanel("Somerville 311 Assistant"),
  
  fluidRow(
    # Sidebar
    column(4, wellPanel(
      h4("Knowledge Base"),
      p(strong(paste(length(kb), "topics available"))),
      div(class = if (use_ai) "mode-badge mode-ai" else "mode-badge mode-keyword",
          if (use_ai) "Semantic AI search" else "Keyword matching"),
      hr(),
      h5("Search settings"),
      sliderInput("min_confidence", "Minimum confidence %:",
                  min = 10, max = 80, value = 30, step = 5),
      p(style = "font-size: 11px; color: #888;",
        "Lower values show more results."),
      hr(),
      h5("Try asking about:"),
      div(
        actionButton("ex1", "Parking permits", class = "btn-sm btn-outline-primary", style = "margin: 2px;"),
        actionButton("ex2", "Trash pickup",    class = "btn-sm btn-outline-primary", style = "margin: 2px;"),
        actionButton("ex3", "Water bills",     class = "btn-sm btn-outline-primary", style = "margin: 2px;"),
        actionButton("ex4", "Voter registration", class = "btn-sm btn-outline-primary", style = "margin: 2px;")
      )
    )),
    
    # Chat panel
    column(8,
           wellPanel(
             h4("Chat"),
             div(class = "chat-container", id = "chat-container", uiOutput("chat_display"))
           ),
           fluidRow(
             column(12, textAreaInput("user_input", label = NULL,
                                      placeholder = "Ask about Somerville city services...",
                                      width = "100%", rows = 2, resize = "vertical"))
           ),
           fluidRow(
             column(12, actionButton("send_btn", "Send",
                                     class = "btn-primary btn-lg",
                                     style = "width: 100%;"))
           ),
           br(),
           p(class = "text-muted", style = "font-size: 11px;",
             if (use_ai) "Cohere semantic search · Returns answers from the Somerville 311 knowledge base"
             else        "Keyword matching · Returns answers from the Somerville 311 knowledge base")
    )
  ),
  
  # JS: scroll to bottom and Ctrl+Enter to send
  tags$script(HTML("
    Shiny.addCustomMessageHandler('scrollToBottom', function(msg) {
      var el = document.getElementById('chat-container');
      if (el) el.scrollTop = el.scrollHeight;
    });
    $(document).on('keydown', '#user_input', function(e) {
      if (e.which === 13 && (e.ctrlKey || e.metaKey)) {
        e.preventDefault();
        $('#send_btn').click();
      }
    });
    $(document).on('shiny:value', function(e) {
      if (e.name === 'chat_display') {
        setTimeout(function() {
          var el = document.getElementById('chat-container');
          if (el) el.scrollTop = el.scrollHeight;
        }, 100);
      }
    });
  "))
)


# ============================================
# SERVER
# ============================================
server <- function(input, output, session) {
  chat_history <- reactiveVal(list())
  
  add_message <- function(text, is_user = TRUE) {
    chat_history(append(chat_history(), list(
      list(text = as.character(text), is_user = is_user)
    )))
  }
  
  output$chat_display <- renderUI({
    msgs <- chat_history()
    if (length(msgs) == 0) {
      return(div(class = "alert alert-info",
                 if (use_ai) "Hello! Ask me anything about Somerville city services."
                 else        "Hello! Ask me anything about Somerville city services. (Running in keyword mode)"
      ))
    }
    
    msg_divs <- lapply(msgs, function(m) {
      cls <- if (m$is_user) "chat-message user-message" else "chat-message bot-message"
      tagList(
        div(class = cls, HTML(gsub("\n", "<br>", m$text))),
        div(class = "clearfix")
      )
    })
    tagList(msg_divs)
  })
  
  process_message <- function() {
    text <- trimws(input$user_input)
    if (!nzchar(text)) return()
    add_message(text, TRUE)
    add_message(find_best_answer(text, input$min_confidence), FALSE)
    updateTextAreaInput(session, "user_input", value = "")
    session$sendCustomMessage("scrollToBottom", list())
  }
  
  observeEvent(input$send_btn, process_message())
  observeEvent(input$ex1, updateTextAreaInput(session, "user_input", value = "How do I get a parking permit?"))
  observeEvent(input$ex2, updateTextAreaInput(session, "user_input", value = "When is my trash picked up?"))
  observeEvent(input$ex3, updateTextAreaInput(session, "user_input", value = "How do I pay my water bill?"))
  observeEvent(input$ex4, updateTextAreaInput(session, "user_input", value = "How do I register to vote?"))
}

shinyApp(ui = ui, server = server)