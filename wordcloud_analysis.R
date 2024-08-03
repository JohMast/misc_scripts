# This function make_wordcloud
# author: Johannes Mast
# date: 03.08.24
# license: MIT

library(SnowballC) # for removing stopwords
library(ggwordcloud)
library(tidytext)
library(tidyverse)

library(ggdark)
set.seed(42)



#### function to make a wordcloud #### (see example below)

comparative_wordcloud <- function(
    # the texts to compare
  text_1,text_2,
  # the two authors
  author_1,author_2,
  # shape (circle, star, heart, diamond, cardioid, square,triangle-forward,triangle-upward, pentagon)
  shape="circle",
  # color for the authors
  col1="orange",col2="cornflowerblue",
  # number of words in the wordcloud
  n_words=100,
  # whether to filter certain words
  remove_numeric=TRUE,filter_words=NULL,remove_common_first_names=FALSE,remove_stopwords=TRUE,
  # show legend or not
  show_legend=TRUE){
  
  # create a tibble with author and texts
  doc_df_sel <- 
    tibble(text=c(text_1,text_2),#
           author=c("author_1","author_2"))
  
  # optionally remove numeric values
  if(remove_numeric){
    doc_df_sel <- 
      doc_df_sel |> 
      mutate(text=gsub("[0-9]", "", text))
  }
  
  # Tokenize the text
  doc_df_sel <- doc_df_sel %>%
    unnest_tokens(word, text)
  
  # optionally remove common first names
  if(remove_common_first_names){
    # get list of baby names and take the 1000 most common
    names <- read.csv("https://raw.githubusercontent.com/hadley/data-baby-names/master/baby-names.csv") |> 
      group_by(name) |> 
      summarize(percent=mean(percent)) |> 
      slice_max(percent, n=1000)
    
    # remove these names
    doc_df_sel <- doc_df_sel %>%
      filter(!word %in% names)
  }
  
  # optionally remove stopwords
  if(remove_stopwords){
    stop_words <- 
      get_stopwords(language = "en") %>%
      mutate(word = word %>% as.character() %>% tolower())
    
    doc_df_sel <- doc_df_sel %>%
      anti_join(stop_words,by="word")
  }
  
  # Remove filter words
  if(!is.null(filter_words)){
    doc_df_sel <- doc_df_sel %>%
      filter(!word %in% filter_words)
  }
  
  # Calculate the log-likelihood ratio for each word
  frequency  <-  
    doc_df_sel %>%
    count(word, author) %>%
    group_by(word) %>%
    filter(sum(n) >= 3) %>%
    ungroup() %>%
    pivot_wider(names_from = author, values_from = n, values_fill = 0) %>%
    # mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
    mutate(author1_frac = (author_1+1) / (sum(author_1)+1),
           author2_frac = (author_2+1) / (sum(author_2)+1)) |> 
    mutate(logratio = log(author1_frac / author2_frac)) |> 
    # scale logratio to be symmetric in range on both sides) %>%
    mutate(logratio=ifelse(
      logratio>0,
      logratio/max(logratio),
      -logratio/min(logratio)
    )) |>
    arrange(desc(logratio)) 
  
  # Create a wordcloud
  # take the 100 words with the highest combined frac
  wordcloud_data <- 
    bind_rows(
      slice_max(frequency,author1_frac,n=n_words/2),
      slice_max(frequency,author2_frac,n=n_words/2))|> 
    mutate(author1_frac_adj=author1_frac/sum(author1_frac),
           author2_frac_adj=author2_frac/sum(author2_frac),
           combined_frac=author1_frac_adj+author2_frac_adj) |> 
    mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1))) |> 
    filter(!duplicated(word)) |> 
    slice_sample(prop = 1)
  
  p <- 
    ggplot(
      wordcloud_data,
      aes(
        label = word, size = combined_frac,
        color = logratio , angle = angle
      )
    ) 
  if(shape=="heart"){
    p <- p+
      geom_text_wordcloud_area(
        mask = png::readPNG(system.file("extdata/hearth.png",
                                        package = "ggwordcloud", mustWork = TRUE,show.legend=show_legend)))
  }else{
    p <- p +
      geom_text_wordcloud_area(shape = shape,area_corr = TRUE,show.legend=show_legend)
  }
  
  #add color scale and legend
  p <- p  +
    scale_size_area(max_size = 30)+
    suppressMessages(ggdark::dark_theme_minimal())  +
    guides(size="none") +
    scale_color_gradient2(low = col1,mid = "white", high = col2,breaks=c(1,0,-1),labels=c(author_1,"",author_2)) +
    theme(legend.position="bottom",legend.title = element_blank(),legend.ticks = element_blank())
  return(p)
}


#### example to make a wordcloud ####

library(gutenbergr) # for getting example texts
# download two example texts by famous authors
example_texts <- gutenbergr::gutenberg_download(c(61262,38219),meta_fields=c("title","author")) |>
  # combine texts into a single row per work
  group_by(gutenberg_id,author,title) |> 
  summarize(complete_text=paste(text,collapse=" "))

# check the example texts
head(example_texts)


# make a wordcloud that is anonymized (without legend and the same colors for both authors)
comparative_wordcloud(example_texts$complete_text[1],
                      example_texts$complete_text[2],
                      example_texts$author[1],
                      example_texts$author[2],
                      show_legend=FALSE,col1="white",col2="white")

# make a wordcloud with color and legend
comparative_wordcloud(example_texts$complete_text[1],
                      example_texts$complete_text[2],
                      example_texts$author[1],
                      example_texts$author[2])

# remove common (american) first names and specific words
comparative_wordcloud(example_texts$complete_text[2],
                      example_texts$complete_text[1],
                      example_texts$author[2],
                      example_texts$author[1],
                      filter_words = c("poirot","analysis"),
                      remove_common_first_names=TRUE)

# save the wordcloud
ggsave(paste0("wordcloud",example_texts$author[1],"_",example_texts$author[2],"_wordcloud.png"),
       width = 8, height = 8, units = "in", dpi = 300)
