
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
  # prediction <- unlist(pred[, .SD, .SDcols = c(word)])
  
  # prediction[1:n_words_predict]
  
  head(pred, n_words_predict)
  
}
