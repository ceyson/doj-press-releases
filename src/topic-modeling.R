







### Baseline topic model input
baseline_sparse <- df_unigrams %>%
  dplyr::count(id_article, word) %>% 
  tidytext::cast_sparse(id_article, word, n)




### parallel processing
furrr::plan(multisession)
tictoc::tic('Execution')
many_models <- tibble::tibble(K = seq(10,100,10)) %>%
  dplyr::mutate(topic_model = furrr::future_map(K, ~stm::stm(baseline_sparse, K = ., verbose = FALSE)))
tictoc::toc() # Execution: 838.69 sec elapsed

heldout <- stm::make.heldout(baseline_sparse)

tictoc::tic('Execution')
k_result <- many_models %>%
  mutate(exclusivity = purrr::map(topic_model, stm::exclusivity),
         semantic_coherence = purrr::map(topic_model, stm::semanticCoherence, baseline_sparse),
         eval_heldout = purrr::map(topic_model, stm::eval.heldout, heldout$missing),
         residual = purrr::map(topic_model, stm::checkResiduals, baseline_sparse),
         bound =  purrr::map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = purrr::map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = purrr::map_dbl(topic_model, function(x) length(x$convergence$bound)))
tictoc::toc() # Execution: 30.4 sec elapsed

k_result


k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around who the hell knows...")


topic_model <- k_result %>% 
  filter(K == 20) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model

td_beta <- tidy(topic_model)
td_beta

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(baseline_sparse))

td_gamma



library(ggthemes)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence in the DOJ Press Release corpus",
       subtitle = "With the top words that contribute to each topic")





td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")



###################################################################################


#### Bigram topic model input
bigram_sparse <- df_bigrams %>%
  dplyr::count(id_article, bigram) %>% 
  tidytext::cast_sparse(id_article, bigram, n)




### parallel processing
future::plan(multisession)
tictoc::tic('Execution')
many_models <- tibble::tibble(K = seq(10,100,10)) %>%
  dplyr::mutate(topic_model = furrr::future_map(K, ~stm::stm(bigram_sparse, K = ., verbose = FALSE)))
tictoc::toc() # Execution: 892.83 sec elapsed

heldout <- stm::make.heldout(bigram_sparse)

tictoc::tic('Execution')
k_result <- many_models %>%
  mutate(exclusivity = purrr::map(topic_model, stm::exclusivity),
         semantic_coherence = purrr::map(topic_model, stm::semanticCoherence, bigram_sparse),
         eval_heldout = purrr::map(topic_model, stm::eval.heldout, heldout$missing),
         residual = purrr::map(topic_model, stm::checkResiduals, bigram_sparse),
         bound =  purrr::map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = purrr::map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = purrr::map_dbl(topic_model, function(x) length(x$convergence$bound)))
tictoc::toc() # Execution: 264.92 sec elapsed

k_result

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be ... something...")


topic_model <- k_result %>% 
  filter(K == 20) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model

td_beta <- tidy(topic_model)
td_beta

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(bigram_sparse))

td_gamma



top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence in the DOJ Press Release corpus",
       subtitle = "With the top words that contribute to each topic")





td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")




#################################################################################



m_eng   <- udpipe::udpipe_download_model(language = "english-ewt")
m_eng <- udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')



narrative_vec <- df_narratives %>%
  dplyr::filter(id_article %in% c(1:3)) %>%
  dplyr::mutate(narrative_lemmas = gsub(' *\\b[[:alpha:]]{1}\\b *', ' ', narrative_lemmas)) %>%
  dplyr::select(narrative_lemmas) %>% 
  dplyr::pull() 

df_pos <- udpipe::udpipe_annotate(m_eng, narrative_vec) %>%
  dplyr::as_tibble()

df_pos %>% View()

df_ann <- df_pos %>%
  dplyr::group_by(doc_id) %>%
  dplyr::mutate(text_ann = paste(token,'/',xpos, collapse = ' ', sep = '')) %>%
  dplyr::ungroup() %>%
  dplyr::select(doc_id, text_ann) %>%
  dplyr::distinct()


#paste(df_pos$token, "/", df_pos$xpos, collapse = " ", sep = "")

df_ann %>% View()


































# tokenise, tag, dependency parsing
text_anndf <- udpipe::udpipe_annotate(m_eng, x = text) %>%
  as.data.frame() %>%
  dplyr::select(-sentence)
# inspect
head(text_anndf, 10)


tagged_text <- paste(text_anndf$token, "/", text_anndf$xpos, collapse = " ", sep = "")
# inspect tagged text
tagged_text



# load text
text <- readLines("https://slcladal.github.io/data/testcorpus/linguistics06.txt", skipNul = T)
# clean data
text <- text %>%
  str_squish() 













