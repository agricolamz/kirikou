setwd("/home/agricolamz/work/articles/2025_kirikou")
library(text.alignment)
library(tidyverse)
df <- readxl::read_xlsx("kirikou_annotations.xlsx") 

df |> 
  mutate(tx = str_remove_all(tx, "[\\+/]"),
         txenfant = str_remove_all(txenfant, "[\\+/]"),
         tx = str_remove_all(tx, "\\[.*?\\]"),
         txenfant = str_remove_all(txenfant, "\\[.*?\\]"),
         tx = str_squish(tx),
         txenfant = str_squish(txenfant),
         tx = str_replace(tx, "\\s{2,}", " "),
         txenfant = str_replace(txenfant, "\\s{2,}", " "),
         id = 1:n()) |> 
  select(id, txenfant, tx) |> 
  pivot_longer(names_to = "type", values_to = "text", -id) |> 
  mutate(text = str_replace_all(text, "á", "á"),
         text = str_replace_all(text, "á", "á"),
         text = str_replace_all(text, "à", "à"),
         text = str_replace_all(text, "à", "à"),
         text = str_replace_all(text, "á̰", "á̰"),
         text = str_replace_all(text, "á̰", "á̰"),
         text = str_replace_all(text, "à̰", "à̰"),
         text = str_replace_all(text, "à̰", "à̰"),         
         text = str_replace_all(text, "ā", "ā"),
         text = str_replace_all(text, "ā", "ā"),
         text = str_replace_all(text, "ā̰", "ā̰"),
         text = str_replace_all(text, "ā̰", "á̰"),
         text = str_replace_all(text, "a̰", "a̰"),
         text = str_replace_all(text, "é", "é"),
         text = str_replace_all(text, "é", "é"),
         text = str_replace_all(text, "è", "è"),
         text = str_replace_all(text, "è", "è"),
         text = str_replace_all(text, "ē", "ē"),
         text = str_replace_all(text, "ē", "ē"),   
         text = str_replace_all(text, "ɛ̄", "ɛ̄"), 
         text = str_replace_all(text, "ɛ̰̀", "ɛ̰̀"),
         text = str_replace_all(text, "ɛ̰̀", "ɛ̰̀"),
         text = str_replace_all(text, "ɛ̰́", "ɛ̰́"),
         text = str_replace_all(text, "ɛ̰́", "ɛ̰́"),
         text = str_replace_all(text, "ɛ̰̄", "ɛ̰̄"),  
         text = str_replace_all(text, "ɛ̰̄", "ɛ̰̄"),         
         text = str_replace_all(text, "ɛ̰̄", "ɛ̰̄"),         
         text = str_replace_all(text, "í", "í"),
         text = str_replace_all(text, "í", "í"),
         text = str_replace_all(text, "ì", "ì"),
         text = str_replace_all(text, "ì", "ì"),
         text = str_replace_all(text, "ī", "ī"),
         text = str_replace_all(text, "ī", "ī"),
         text = str_replace_all(text, "ī", "ī"),
         text = str_replace_all(text, "ḭ̄", "ḭ̄"),
         text = str_replace_all(text, "ḭ̄", "ḭ̄"),
         text = str_replace_all(text, "ḭ̄", "ḭ̄"),
         text = str_replace_all(text, "ḭ̄", "ḭ̄"),
         text = str_replace_all(text, "ḭ́", "ḭ́"),
         text = str_replace_all(text, "ḭ̀", "ḭ̀"),
         text = str_replace_all(text, "ḭ̀", "ḭ̀"),
         text = str_replace_all(text, "ɩ̀", "ì"),
         text = str_replace_all(text, "ó", "ó"),
         text = str_replace_all(text, "ó", "ó"),
         text = str_replace_all(text, "ò", "ò"),
         text = str_replace_all(text, "ò", "ò"),
         text = str_replace_all(text, "ō", "ō"),
         text = str_replace_all(text, "ō", "ō"),
         text = str_replace_all(text, "ɔ̰́", "ɔ̰́"),
         text = str_replace_all(text, "ɔ̰́", "ɔ̰́"),
         text = str_replace_all(text, "ɔ̰̀", "ɔ̰̀"),
         text = str_replace_all(text, "ɔ̰̀", "ɔ̰̀"),
         text = str_replace_all(text, "ɔ̰̄", "ɔ̰̄"),
         text = str_replace_all(text, "ɔ̰̄", "ɔ̰̄"),
         text = str_replace_all(text, "ú", "ú"),
         text = str_replace_all(text, "ú", "ú"),
         text = str_replace_all(text, "ù", "ù"),
         text = str_replace_all(text, "ù", "ù"),
         text = str_replace_all(text, "ū", "ū"),
         text = str_replace_all(text, "ū", "ū"),
         text = str_replace_all(text, "ṵ́", "ṵ́"),
         text = str_replace_all(text, "ṵ́", "ṵ́"),
         text = str_replace_all(text, "ṵ̄", "ṵ̄"),
         text = str_replace_all(text, "ṵ̄", "ṵ̄"),
         text = str_remove_all(text, "[123¹]")) |> 
  pivot_wider(names_from = type, values_from = text) ->
  selected_cols

tokenizer <- function(x){
  x |> 
    str_replace_all("([:punct:])", " \\1") |> 
    str_split(" ") |> 
    unlist()
}

map(selected_cols$id, .progress = TRUE, function(i){
  
  if(selected_cols$txenfant[i] == selected_cols$tx[i]){
    tibble(tokens_a = selected_cols$txenfant[i],
           tokens_b = selected_cols$tx[i])
  } else {
    res <- smith_waterman(selected_cols$txenfant[i], selected_cols$tx[i], type = "words",
                          tokenizer = tokenizer)
    
    tibble(tokens_a = res$a$alignment$tokens,
           id_a = cumsum(res$a$alignment$tokens %in% res$a$tokens)+res$a$alignment$from-1,
           id = seq_along(tokens_a)) |> 
      full_join(tibble(tokens_b = res$b$alignment$tokens,
                       id_b = cumsum(res$b$alignment$tokens %in% res$b$tokens)+res$b$alignment$from-1,
                       id = seq_along(tokens_b)),
                by = join_by(id)) |> 
      full_join(tibble(tokens_a = res$a$tokens,
                       id_a = seq_along(res$a$tokens)),
                by = join_by(tokens_a, id_a)) |> 
      full_join(tibble(tokens_b = res$b$tokens,
                       id_b = seq_along(res$b$tokens)),
                by = join_by(tokens_b, id_b))  |> 
      mutate(id_a = if_else(is.na(id_a), id_b, id_a),
             id_b = if_else(is.na(id_b), id_a, id_b),
             tokens_a = if_else(str_detect(tokens_a, "\\#"), NA, tokens_a),
             tokens_b = if_else(str_detect(tokens_b, "\\#"), NA, tokens_b),
             tokens_a = if_else(!is.na(tokens_a) & !is.na(tokens_b) & tokens_a == tokens_b, 
                                tokens_a, 
                                str_c("/", tokens_a, "/")),
             tokens_b = if_else(!is.na(tokens_a) & !is.na(tokens_b) & tokens_a == tokens_b, 
                                tokens_b, 
                                str_c("/", tokens_b, "/"))) |> 
      arrange(id_a, id_b) |> 
      select(tokens_a, tokens_b) |> 
      mutate(across(everything(), function(x) if_else(is.na(x), "", x)),
             tokens_a = if_else(str_detect(tokens_a, "/") & !str_detect(lead(tokens_b), "/"), 
                                str_replace_all(tokens_a, "/", "+"),
                                tokens_a),
             tokens_b = if_else(str_detect(tokens_b, "/") & !str_detect(lag(tokens_a), "/"), 
                                str_replace_all(tokens_b, "/", "+"),
                                tokens_b)) |> 
      summarize(tokens_a = str_c(tokens_a, collapse = " "),
                tokens_b = str_c(tokens_b, collapse = " ")) |> 
      mutate(tokens_a = str_replace_all(tokens_a, "/ /", " "),
             tokens_b = str_replace_all(tokens_b, "/ /", " "),
             tokens_a = str_replace_all(tokens_a, "\\+ \\+", " "),
             tokens_b = str_replace_all(tokens_b, "\\+ \\+", " "),
             tokens_a = str_replace_all(tokens_a, "\\s{1,}", " "),
             tokens_b = str_replace_all(tokens_b, "\\s{1,}", " "),
             tokens_a = str_squish(tokens_a),
             tokens_b = str_squish(tokens_b))
  }
}) |> 
  list_rbind() |> 
  rename(txenfant_automatic = tokens_a,
         tx_automatic = tokens_b) ->
  result

df |> 
  bind_cols(result) |> 
  # filter(txenfant_automatic != tx_automatic) |> View()
  writexl::write_xlsx("kirikou_annotations_with_automatic_annotation.xlsx")
