### Inits
libs <- c('tidyverse','stringr','textclean','textstem','lexicon','tidytext','stm',
          'polmineR','data.table','furrr','udpipe','scales')
new.libs <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.libs)) install.packages(new.libs)
lapply(libs, require, character.only = TRUE)

### Read press releases
df_releases <- data.table::fread('./input/doj_press_releases.csv') %>%
  dplyr::mutate(id_article = dplyr::row_number())

### Narrative processing
## Remove boiler plate language
## Remove non-informative characters
## Lemmatize text

## Boiler plate removal
df_sentences <- df_releases %>%
  dplyr::select(id_article, story) %>%
  tidytext::unnest_sentences(sentence, story)

## Count and filter sentences 
df_sentcnt <- df_sentences %>%
  dplyr::count(sentence, sort = TRUE) %>%
  dplyr::filter(n > 1) %>%
  dplyr::mutate(cnt_words = stringr::str_count(sentence, '\\S+')) %>%
  dplyr::filter(cnt_words > 1) %>%
  dplyr::filter(n >= 5)

df_sentfil <- df_sentences %>% dplyr::anti_join(df_sentcnt %>% dplyr::select(sentence))

df_reconstr <- df_sentfil %>%
  dplyr::group_by(id_article) %>%
  dplyr::mutate(story_reconstr = stringr::str_c(sentence, collapse = ' ')) %>%
  dplyr::ungroup() %>%
  dplyr::select(-sentence) %>%
  dplyr::distinct() 

### Character removal, lemmatization
tictoc::tic('Execution')
df_narratives <- df_reconstr %>%
  dplyr::select(id_article, story_reconstr) %>%
  dplyr::mutate(narrative_lemmas = textclean::replace_non_ascii(story_reconstr,'') %>%
                  stringr::str_replace_all('[^[:alnum:]]', ' ') %>% 
                  stringr::str_replace_all('[[:punct:]]','') %>% 
                  stringr::str_replace_all('[[:digit:]]','') %>%
                  stringr::str_squish() %>% 
                  textstem::lemmatize_strings(dictionary = lexicon::hash_lemmas)) %>%
  dplyr::left_join(df_releases %>% dplyr::select(id_article, story)) %>%
  dplyr::select(id_article, story, story_reconstr, narrative_lemmas)
tictoc::toc() # Execution: 2.69 sec elapsed

### N-gram extraction
func_ngrams <- function(df_input, id_col, text_col, n_words) {
  
  if (n_words == 1) {
    
    df_ngrams <- df_input %>%
      dplyr::mutate(text_col = gsub(' *\\b[[:alpha:]]{1}\\b *', ' ', {{text_col}})) %>%
      tidytext::unnest_tokens(word, text_col) %>%
      dplyr::select({{id_col}}, word) %>%
      dplyr::anti_join(tidytext::get_stopwords()) %>%
      dplyr::rename('ngram' = word)
    
  } else {
    
    df_ngrams <- df_input %>%
      dplyr::mutate(text_col = gsub(' *\\b[[:alpha:]]{1}\\b *', ' ', {{text_col}})) %>%
      tidytext::unnest_tokens(ngram, text_col, token = 'ngrams', n = n_words) %>%
      dplyr::select({{id_col}}, ngram) %>%
      dplyr::filter(!is.na(ngram)) %>%
      dplyr::mutate(ngram = gsub(' ','_',ngram))
    
  }
  
  return(df_ngrams)
}

### N-grams
df_unigrams <- func_ngrams(df_narratives, id_article, narrative_lemmas, 1)
df_bigrams <- func_ngrams(df_narratives, id_article, narrative_lemmas, 2)
df_trigrams <- func_ngrams(df_narratives, id_article, narrative_lemmas, 3)


### Parts of speech tagging

### Download and retrieve model
m_eng   <- udpipe::udpipe_download_model(language = "english-ewt")
m_eng <- udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')

### Unigram POS tagging

### Vector of narratives
narrative_vec <- df_narratives %>%
  # dplyr::filter(id_article %in% c(1:3)) %>%
  dplyr::mutate(narrative_lemmas = gsub(' *\\b[[:alpha:]]{1}\\b *', ' ', narrative_lemmas)) %>%
  dplyr::select(narrative_lemmas) %>% 
  dplyr::pull() 

### Part of Speech Tagging
tictoc::tic('Execution')
df_pos <- udpipe::udpipe_annotate(m_eng, narrative_vec) %>% dplyr::as_tibble()
tictoc::toc() # Execution: 814.23 sec elapsed

df_pos %>% View()

df_ann <- df_pos %>%
  dplyr::mutate(id_article = stringr::str_replace(doc_id,'doc','') %>% as.numeric()) %>%
  dplyr::select(id_article, token, upos, xpos)

df_ann %>% View()
df_ann %>% vroom::vroom_write('./input/df_unigram_pos.csv', delim = ',')


df_ann %>% dplyr::filter(str_detect(upos,'NOUN')) %>% View()