
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

make_ngram <- function(data, n = 5, text = text) {
  
  text_q <- deparse(substitute(text))
  word_vec <- paste0("word_", 1:n)
  list_names <- paste0(n:1, "gram")
  
  data_dt <- copy(as.data.table(data))
  # data_dt <- copy(as.data.table(corpora))
  
  stopifnot("Please specify existing text column name" = text_q %in% colnames(data_dt))
  
  ngram_list <- vector("list", length = n)
  names(ngram_list) <- list_names
  
  temp <- data_dt[, .(text = paste0(paste0(rep("bosbos ", n - 1), collapse = ""), text, paste0(rep(" eoseos", n - 1), collapse = "")))]
  
  temp <- unnest_tokens(temp, output = ngram, input = text, token = "ngrams", n = n)
  
  temp_c <- copy(temp)
  
  split <- quote(`:=`(
    eval(word_vec[1:n]),
    # eval(word_vec[1:n]),
    tstrsplit(
      eval(ngram),
      split = " ",
      fill = NA,
      fixed = TRUE
    )
  ))
  
  temp_c[, eval(split)]
  
  temp_c <- temp_c[, .N, by = c(word_vec)]
  
  # Move in a loop after assigning all lists?
  # setnames(temp, old = "N", new = paste0("count_", list_names[1]))
  
  setnames(temp_c, old = "N", new = "count")
  
  ngram_list[[1]] <- temp_c
  
  for (i in 2:n) {
    
    string <- sapply(i:n, \(x) paste0("(", word_vec[x], " %in% c('bosbos', 'eoseos'))"))
    string <- paste0(string, collapse = " & ")
    
    temp_c <- temp_c[!eval(str2expression(string)), .(count = sum(count)), by = c(word_vec[i:n])]
    
    ngram_list[[i]] <- temp_c
    
  }
  
  ngram_list
  
}

stupid_backoff <- function(ngram_l, smoother = .4) {
  
  ngram_lst <- copy(ngram_l)
  
  n <- length(ngram_lst)
  word_vec <- paste0("word_", 1:n)
  
  for (i in 1:(n-1)) {
    
    ngram_lst[[i]][, denom := sum(count), by = c(word_vec[i:(n-1)])][, prob := count / denom]
    
    if (i != 1) {ngram_lst[[i]][, prob := ((smoother) ^ (i - 1)) * prob]}
    
    ngram_lst[[i]] <- ngram_lst[[i]][get(word_vec[n]) != "eoseos"]
    
  }
  
  ngram_lst[[n]][, prob := (smoother^n) * count / sum(count)]
  
  ngram_lst[[n]] <- ngram_lst[[n]][get(word_vec[n]) != "eoseos"]
  
  ngram_lst
  
}

kneser_ney <- function(ngram_l) {
  
  ngram_lst <- copy(ngram_l)
  # ngram_lst <- copy(ngram_list)
  n <- length(ngram_lst) 
  word_vec <- paste0("word_", 1:n)
  ccount_after <- paste0("ccount_after_", 1:(n-1))
  ccount_after_join <- paste0("ccount_after_join_", 1:(n-1))
  ccount_before <- paste0("ccount_before_", 1:(n-1))
  ccount_before_join <- paste0("ccount_before_join_", 1:(n-1))
  pkn <- paste0("pkn_", 1:n)
  
  discount <- vector("list", n)
  
  ## unique bigrams 
  denom_1 <- nrow(ngram_lst[[(n-1)]])
  
  for (i in 1:(n - 1)) {
    
    ## Calculate discount factors
    
    n1 <- ngram_lst[[i]][count == 1, .N]
    
    n2 <- ngram_lst[[i]][count == 2, .N]
    
    discount[[i]] <- n1 / (n1 + 2 * n2)
    
    # d <- discount[[i]]
    
    ## Join to prepare for gamma calc
    
    ngram_lst[[i]][, (ccount_after[i]) := .N, by = c(word_vec[i:(n - 1)])][
      , (ccount_before[i]) := .N, by = c(word_vec[(i + 1):n])]
    
    temp <- copy(ngram_lst[[i]])
    temp <- unique(temp[, (ccount_after_join[i]) := get(ccount_after[i])][, .SD, .SDcols = c(word_vec[i:(n-1)], ccount_after_join[i])])
    
    # browser()
    
    ngram_lst[[i]] <- merge(ngram_lst[[i]], 
                            temp, 
                            by.x = word_vec[(i + 1):n],
                            by.y = word_vec[i:(n - 1)], 
                            all.x = TRUE, 
                            sort = FALSE)
    
    if (i != 1) {
      
      temp1 <- copy(ngram_lst[[i]])
      
      temp1 <- unique(temp1[, (ccount_before_join[i]) := .N, by = c(word_vec[(i+1):n])][, .SD, .SDcols = c(word_vec[(i+1):n], ccount_before_join[i])])
      # 
      # browser()
      
      ngram_lst[[i]] <- merge(ngram_lst[[i]], 
                              temp1, 
                              by.x = word_vec[i:(n-1)], 
                              by.y = word_vec[(i+1):n], 
                              all.x = TRUE,
                              sort = FALSE)
    }
    
    temp2 <- unique(ngram_lst[[i]][, .SD, .SDcols = c(word_vec[(i+1):n], ccount_before[i])])
    
    # browser()
    
    ngram_lst[[i + 1]] <- merge(ngram_lst[[i + 1]], 
                                temp2, 
                                by = word_vec[(i+1):n],
                                all.x = TRUE, 
                                sort = FALSE)
    
    
    
    # browser()
    
    ## Calculate gamma
    
    if (i == 1) {
      
      ngram_lst[[i]][, denom_count := sum(count), by = c(word_vec[i:(n - 1)])]
      
      # ngram_lst[[i]][, gamma := discount[[i]] * get(ccount_after_join[i]) / denom_count]
      ngram_lst[[i]][, gamma := discount[[i]] * get(ccount_after[i]) / denom_count]
      
    } else {
      
      # ngram_lst[[i]][, gamma := discount[[i]] * get(ccount_after_join[i]) / get(ccount_before_join[i])]
      # ngram_lst[[i]][get(word_vec[i]) == "bosbos", (ccount_before_join[i]) := 1]
      
      ngram_lst[[i]][, gamma := discount[[i]] * get(ccount_after[i]) / get(ccount_before_join[i])]
      
    }
    
    # temp1 <- copy(ngram_lst[[i + 1]])
    # temp1 <- copy(ngram_lst[[i]])
    
    
    
    # browser()
    
    if (i == 1) {
      
      ngram_lst[[i]][, first_term := pmax((count - discount[[i]]), 0) / denom_count]
      
    } else {
      
      ngram_lst[[i]][, first_term := pmax((get(ccount_before[i-1]) - discount[[i]]), 0) / get(ccount_before_join[i])]
      
    }
    
  }
  
  ## Unigram probability
  
  ngram_lst[[n]][, (pkn[n]) := get(ccount_before[n-1]) / denom_1][, prob := get(pkn[n])]
  
  ## Calculate Pkn backwards
  
  for (i in (n-1):1) {
    
    temp2 <- unique(ngram_lst[[i+1]][, .SD, .SDcols = c(word_vec[(i+1):n], pkn[i+1])])
    
    ngram_lst[[i]] <- merge(ngram_lst[[i]], 
                            temp2, 
                            by = word_vec[(i+1):n], 
                            all.x = TRUE, 
                            sort = FALSE
    )
    
    ngram_lst[[i]][, (pkn[i]) := first_term + (gamma * get(pkn[i+1]))][, prob := get(pkn[i])]
    
  }
  
  for (i in 1:(n-1)) {
    #   
    #   ngram_lst[[i]] <- ngram_lst[[i]][!is.na(prob), .SD, .SDcols = c(word_vec[i:n], "first_term", "gamma", pkn[i], "count", "prob")]
    ngram_lst[[i]] <- ngram_lst[[i]][word_5 != "eoseos"]
  }
  
  ngram_lst
  
} 

pruner <- function(ngram_l, k = NULL, f = NULL, ranking = NULL) {
  
  # if k is specified, ngrams with count equal or higher to k will be kept, rest will be discarded
  # if f is specified, ngrams of each order are sorted in descending order and the cumulative frequency is calculated. 
  # ngrams surpassing cum freq f will be discarded
  
  stopifnot("Please specify either k or f (if both, k takes precedence)" = (!is.null(k) | !is.null(f)))
  
  ngram_lst <- copy(ngram_l)
  n <- length(ngram_lst)
  word_vec <- paste0("word_", 1:n)
  
  if (!is.null(k)) {
    
    for (i in 1:n) {
      
      ngram_lst[[i]] <- ngram_lst[[i]][count >= k]
      
    }
    
    
  } else {
    
    for (i in 1:n) {
      
      setorderv(ngram_lst[[i]], cols = "count", order = -1L)
      
      ngram_lst[[i]][, freq := count / sum(count)][, freq_cum := cumsum(freq)]
      
      ngram_lst[[i]] <- ngram_lst[[i]][freq_cum < f]
      
    }
    
  }
  
  # Extra step to prune if we have more than "ranking" possible completing words for a ngram-1 combination so that we only keep as many as we want
  
  if (!is.null(ranking)) {
    
    for (i in 1:(n-1)) {
      
      # frank is slow when computing over huge number of groups, base rank is 10 times faster
      # ngram_lst[[i]][, prob_rank := frank(-prob), by = c(word_vec[i:(n-1)])] 
      ngram_lst[[i]][, prob_rank := rank(-prob), by = c(word_vec[i:(n-1)])]
      
      ngram_lst[[i]] <- ngram_lst[[i]][prob_rank <= ranking]
      
    }
    
    setorderv(ngram_lst[[n]], cols = "count", order = -1L)
    ngram_lst[[n]] <- ngram_lst[[n]][1:ranking]
    
  }
  
  ngram_lst
  
}


lookup_table <- function(ngram_l) {
  
  ngram_lst <- copy(ngram_l)
  n <- length(ngram_lst)
  word_vec <- paste0("word_", 1:n)
  
  lookup <- NULL
  
  for (i in 1:n) {
    
    words <- unique(unlist(ngram_lst[[i]][, .SD, .SDcols = c(word_vec[i:n])]))
    words <- data.table(words)
    
    lookup <- rbind(lookup, words)
    
  }
  
  lookup <- unique(lookup)
  lookup[, id := .I]
  
  lookup
  
}


lut_replace <- function(ngram_l, lookup) {
  
  ngram_lst <- copy(ngram_l)
  n <- length(ngram_lst)
  word_vec <- paste0("word_", 1:n)
  id_vec <- paste0("id_", 1:n)
  
  for (i in 1:(n - 1)) {
    
    for (j in 1:(n - i)) {
      
      ngram_lst[[i]] <- merge(ngram_lst[[i]], lookup, by.x = word_vec[i:(n - 1)][j], by.y = "words", all.x = TRUE)
      
      setnames(ngram_lst[[i]], old = "id", new = id_vec[i:(n - 1)][j])
      
    }
    
    ngram_lst[[i]] <- ngram_lst[[i]][, .SD, .SDcols = c(id_vec[i:(n - 1)], word_vec[n], "prob")]
    
    setkeyv(ngram_lst[[i]], cols = c(id_vec[i:(n - 1)]))
    
  }
  
  ngram_lst
  
}

predict_ngram <- function(sentence, ngram_l, lookup, n_words_predict = 3) {
  
  n <- length(ngram_l)
  word <- paste0("word_", n)
  
  sen <- stri_enc_toascii(sentence)
  sen <- str_replace_all(sen, "[[:punct:]]|[[:digit:]]", "")
  sen <-  str_squish(sen)
  sen <- str_to_lower(sen)
  
  char <- unlist(strsplit(sen, split = " "))
  
  if (length(char) < (n - 1)) char <- c(rep("bos", (n - 1 - length(char))), char)
  
  sen <- as.data.table(char)
  
  sen <- tail(sen, (n - 1))
  
  sen <- merge(sen, lookup, by.x = "char", by.y = "words", all.x = TRUE, sort = FALSE)[, id]
  
  pred <- NULL
  
  for (i in 1:(n - 1)) {
    
    s <- paste0(".(", paste0(sen[i:(n - 1)], collapse = ", "), ")")
    
    # filter, sort and store words and probs, maybe create a variable for the order?
    temp <- ngram_l[[i]][eval(str2expression(s)), .SD, .SDcols = c(word, "prob")][, order := (n + 1 - i)]
    
    # setorderv(temp, cols = "prob", order = -1L)
    
    if (i != 1) {temp <- temp[!(get(word) %in% resid)]}
    
    pred <- rbind(pred, temp[!is.na(get(word))])
    
    resid <- unlist(pred[, .SD, .SDcols = c(word)])
    
  }

  temp <- ngram_l[[n]][!(get(word) %in% resid)]
  temp[, order := 1]
  
  temp <- temp[, .SD, .SDcols = c(word, "prob", "order")]
  
  pred <- rbind(pred, temp[!is.na(get(word))])
  
  setorderv(pred, cols = c("prob", "order"), order = -1L)
  
  # line to run for the benchmarking only
  prediction <- unlist(pred[, .SD, .SDcols = c(word)])
  
  prediction[1:n_words_predict]
  
  # head(pred, n_words_predict)
  
}
