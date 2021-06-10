
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

# This function will check to see if the data is available in the project directory
# If not, it will download and store it in the /data folder
# Then it will provide options to load the data

read_text <- function(file_name, prows = 0.15, nrows = NULL, seed = 1234) {
  
  stopifnot(
    
    "Specify either nrows or prows (prows takes precedence if both set)" = !is.null(prows) | !is.null(nrows)
    
  )
  
  all_lines <- readLines(con <- file(file_name, encoding = "UTF-8"), encoding = "UTF-8")
  
  close.connection(con)
  
  if (!is.null(prows)) {
    
    l <- length(all_lines)
    
    set.seed(seed)
    
    distr <- as.logical(rbinom(l, 1, prows))
    
    lines <- all_lines[distr]  
    
  } else {
    
    set.seed(seed)
    
    lines <- sample(all_lines, size = nrows, replace = FALSE)
    
  }
  
  lines
  
}

# This function will read the specified texts and then split it in 
# training and testing sets based on a specified proportion
# seed is required for reproducibility

split_text <- function(..., names = NULL, split = NULL, prop = .1, seed = 1234) {
  
  args <- list(...)
  
  stopifnot(
    "Specify names for the texts that are being read" = !is.null(names), 
    "Specify as many names as provided texts" = length(names) == length(args)
            )

  con <- map(args, file, encoding = "UTF-8")
  
  texts <- map(con, readLines, encoding = "UTF-8")
  
  map(con, close.connection)
  
  if (is.null(split)) { 
    
    split <- .8
    message("Default split of 80/20 was selected as split variable was not specified")
    
  }
  
  l <- map(texts, length)
  
  set.seed(seed)
  
  proport <- map(l, rbinom, size = 1, prob = prop)
  proport <- map(proport, as.logical)
  
  texts_prop <- map2(texts, proport, `[`)
  
  ll <- map(texts_prop, length)
  
  distr <- map(ll, rbinom, size = 1, prob = split)
  distr <- map(distr, as.logical)
  not_distr <- map(distr, `!`)
  
  train <- map2(texts_prop, distr, `[`)
  train <- map(train, as_tibble)

  names <- as.list(names)
  
  for (i in 1:length(train)) {
  
    train[[i]] <- train[[i]] %>% mutate(source = names[[i]], id = paste0(names[[i]], "_", row_number()))
    
  }

  train <- do.call(bind_rows, train)
  
  test <- map2(texts_prop, not_distr, `[`)
  test <- map(test, as_tibble)
  
  for (i in 1:length(test)) {
    
    test[[i]] <- test[[i]] %>% mutate(source = names[[i]], id = paste0(names[[i]], "_", row_number()))
    
  }
  
  test <- do.call(bind_rows, test)
  
  lines <- list(train = train, test = test)
  
  lines
  
}

make_ngram <- function(data, n = 6, id = id, value = value, word_len_max = 18, top_word_freq = NULL, profanity_filter = NULL, train = TRUE) {
  id_q <- deparse(substitute(id))
  value_q <- deparse(substitute(value))
  word_vec <- paste0("word_", 1:n)
  ngram_final <- NULL
  data_dt <- copy(as.data.table(data))
  
  stopifnot(
    "Please specify existing id name" = id_q %in% colnames(data_dt),
    "Please specify existing text column name" = value_q %in% colnames(data_dt)
  )
  
  id_leftover <- data_dt[, get(id_q)]
  
  if (!is.null(top_word_freq) & (train)) {
    
    word_freq <- unnest_tokens(data_dt, output = word, input = value, token = "words")
    
    word_freq <- word_freq[str_length(word) < 19 & str_detect(word, "^[a-zA-Z']+$") & !str_detect(word, "([a-zA-Z]+)\\1{3,}"), 
                           .N, by = .(word)][
                             order(N, decreasing = TRUE)]
    
    word_freq[, freq := N / sum(N)][, cum_freq := cumsum(freq)]
    
    freq_words <- word_freq[cum_freq < top_word_freq, word]
    
  }  
  
  for (i in 1:(n - 1)) {
    ngram_init <- data_dt[get(id_q) %in% id_leftover]
    
    ngram_init <- unnest_tokens(ngram_init, output = ngram, input = value_q, token = "ngrams", n = n + 1 - i)
    
    # ngram_init <- unnest_tokens(data_dt, output = ngram, input = value_q, token = "ngrams", n = n)
    
    split <- quote(`:=`(
      eval(word_vec[i:n]),
      # eval(word_vec[1:n]),
      tstrsplit(
        eval(ngram),
        split = " ",
        fill = NA,
        fixed = TRUE
      )
    ))
    
    ngram_c <- copy(ngram_init)
    
    ngram_c[, eval(split)][, order := n + 1 - i]
    # ngram_c[, eval(split)][, order := n]
    
    ngram_final <- rbind(ngram_final, ngram_c[!is.na(ngram)], use.names = TRUE, fill = TRUE)
    # ngram_final <- ngram_c[!is.na(ngram)]
    
    
    id_leftover <- ngram_c[is.na(ngram), get(id_q)]
    
  }
  
  for (i in word_vec) {
    ngram_final <- ngram_final[str_length(get(i)) < (word_len_max + 1) &
                                 str_detect(get(i), "^[a-zA-Z']+$") &
                                 !str_detect(get(i), "([a-zA-Z]+)\\1{3,}") | is.na(get(i))]
    
    if (!is.null(profanity_filter)) {
      
      profanity <- unlist(profanity_filter)
      
      ngram_final <- ngram_final[!(get(i) %in% profanity)]
      
    }
    
    if (!is.null(top_word_freq) & (train)) {
      
      ngram_final <- ngram_final[get(i) %in% freq_words]
      
    }
    
  }
  
  ngram_final <- ngram_final[, .N, by = c(word_vec, "order")]
  
  ngram_final
}

kn_smoothing <- function(data, discount_factor = .75) {
  
  dt <- copy(as.data.table(data))
  # dt <- copy(as.data.table(ngram))
  n <- length(dt) - 2
  word_vec <- paste0("word_", 1:n)
  id_vec <- paste0("id_", 1:n)
  prob_vec <- paste0("prob_", 1:n)
  keep_cols <- c(id_vec, prob_vec)
  
  ngram_list <- map(1:n, ~ dt[!is.na(get(word_vec[.x])), .(count = sum(N)), by = c(word_vec[.x:n])])
  list_name <- paste0("order_", n:1)
  names(ngram_list) <- list_name
  ccount_nom <- paste0("ccount_nom_", list_name[2:n])
  ccount_denom <- paste0("ccount_denom_", list_name[2:n])
  ccount <- paste0("ccount_", list_name[2:n])
  lambda <- paste0("lambda_", list_name[1:n])
  pcont <- paste0("pcont_", list_name[1:n])
  
  # top ngram level
  
  ngram_list[[list_name[1]]][, probability := count / sum(count), by = c(word_vec[1:(n-1)])]
  
  # Recursive continuation count
  
  for (i in 1:(n-2)) {
    
    temp_1 <- copy(ngram_list[[list_name[i]]])
    # temp_1 <- ngram_list[[list_name[i]]][, (ccount_nom[i]) := .N, by = c(word_vec[(i + 1):n])]
    temp_1[, (ccount_nom[i]) := .N, by = c(word_vec[(i + 1):n])]
    
    temp_2 <- unique(temp_1[, .SD, .SDcols = c(word_vec[i:(n-1)])])
    temp_2 <- temp_2[, (ccount_denom[i]) := .N, by = c(word_vec[(i + 1):(n-1)])][
      , .SD, .SDcols = c(word_vec[(i + 1):(n-1)], ccount_denom[i])]
    temp_2 <- unique(temp_2)
    
    temp <- merge(temp_1, temp_2, by = c(word_vec[(i + 1):(n-1)]), all.x = TRUE)
    temp[, (ccount[i]) := (get(ccount_nom[i]) - discount_factor) / get(ccount_denom[i])]
    temp <- unique(temp[, .SD, .SDcols = c(word_vec[(i + 1):n], ccount_nom[i], ccount_denom[i], ccount[i])])
    
    ngram_list[[list_name[(i + 1)]]] <- merge(ngram_list[[list_name[(i + 1)]]],
                                              temp,
                                              by = c(word_vec[(i + 1):n]),
                                              all.x = TRUE,
                                              all.y = FALSE)
  }
  
  # Recursive calculations
  
  for (i in 2:(n-1)) {
    
    # Continuation Probability
    
    pcont_denom <- nrow(ngram_list[[list_name[i]]])
    pcont_temp <- unique(ngram_list[[list_name[i]]][, .SD, .SDcols = word_vec[i:n]])[
      , .N, by = c(word_vec[n])][
        , (pcont[i]) := N / as.numeric(pcont_denom)]
    
    ngram_list[[list_name[i]]] <- merge(ngram_list[[list_name[i]]], pcont_temp, by = c(word_vec[n]), all.x = TRUE)
    
    # Lambda
    
    lambda_temp <- ngram_list[[list_name[i]]][, .SD, .SDcols = c(word_vec[i:n], "count")]
    
    lambda_temp[, `:=`(lambda_nom = .N, lambda_denom = sum(count)), by = c(word_vec[i:(n-1)])][
      , (lambda[i]) := discount_factor * lambda_nom / lambda_denom]
    
    lambda_temp <- unique(lambda_temp[, .SD, .SDcols = c(word_vec[i:(n-1)], lambda[i])])
    
    ngram_list[[list_name[i]]] <- merge(ngram_list[[list_name[i]]], lambda_temp, by = c(word_vec[i:(n-1)]), all.x = TRUE)
    
    ngram_list[[list_name[i]]][, probability := get(ccount[(i - 1)]) + get(lambda[i]) * get(pcont[i])]
    
  }
  
  ngram_list[[list_name[n]]][, probability := count / sum(count)]
  
  for (i in 1:n) {
    
    ngram_list[[list_name[i]]] <- ngram_list[[list_name[i]]][, .SD, .SDcols = c(word_vec[i:n], "probability")]
    setnames(ngram_list[[list_name[i]]], old = "probability", new = prob_vec[i])
    
    if (i == 1) {
      
      table <- ngram_list[[list_name[1]]]
      
    } else {
      
      table <- merge(table, ngram_list[[list_name[i]]], by = word_vec[i:n])
      
    }
    
  }
  
  words <- as.data.table(unique(unlist(ngram[, ..word_vec])))
  setnames(words, old = "V1", new ="word")
  words[, id := .I]
  
  for (i in 1:n) {
    
    table <- merge(table, words, by.x = word_vec[i], by.y = "word")
    
    setnames(table, old = "id", new = id_vec[i])
    
  }
  
  table <- table[, .SD, .SDcols = keep_cols]
  
  l <- list(lookup = words, ngram_table = table)
  
  l
  
}