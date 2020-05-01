musigram <- function(artist = NULL, song = NULL) {
  # Load packages
  library(tidyverse)
  library(ggraph)
  library(igraph)
  # Using the genius API to get the lyrics of a package
  genius::genius_lyrics(artist, song) %>%
    drop_na(lyric) %>%
    select(lyric) %>%
    # All of these lines are here because str_c only accepts vectors
    # Eventually, I will find a better approach
    deframe() %>%
    str_c(sep = " ", collapse = " ") %>%
    enframe(value = "lyrics") %>%
    select(lyrics) %>%
    # Creating bigrams
    tidytext::unnest_tokens(bigram, lyrics, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, sort = TRUE) %>%
    # This only keeps the word combinations that appear more than once
    filter(n > 2) %>%
    # Creating the network graph
    graph_from_data_frame() %>%
    ggraph(layout = 'fr') +
    geom_edge_link(aes(edge_alpha = n)
      , show.legend = FALSE
      , arrow = grid::arrow(type = "closed", length = unit(.15, "inches"))
      , end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}
