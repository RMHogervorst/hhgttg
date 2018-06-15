### exploration
library(pubcrawl)
library(tidyverse)
#entirebook <- epub_to_text("data/Adams, Douglas/ultimate hitchhiker's guide to the galaxy, The/ultimate hitchhiker's guide to the galaxy, The - Douglas Adams.epub")
# entirebook %>% 
#     filter(path == "titlepage.xhtml") %>% 
#     pull(content)
# # this just contains some formatting for the title
# entirebook %>% 
#     filter(path == "index_split_000.html") %>%     
#     pull(content)
# # 000 is the index.
# entirebook %>% 
#     filter(path == "index_split_001.html") %>%     
#     pull(content)
# 

# maybe I have to use the other files. This one is not as organized as could be.

HH1 <- epub_to_text("data/Adams, Douglas/Hitchhiker's Guide to the Galaxy, The/Hitchhiker's Guide to the Galaxy, The - Douglas Adams.epub")
# much better, 73 parts. 
# better call it HH1?
HH1 %>% 
    pull(path)
HH1 %>% 
    filter(path == "OEBPS/part1.xhtml" ) %>% 
    pull(content)
# CAPITALS are headers
# chapter headings?
# "HH1 - Hitchhiker's Guide to the Galaxy\n    \n    \n      HH1 - Hitchhiker’s Guide to the Galaxy\n      By Adams, Douglas\n      The Hitchhiker’s Guide to the Galaxy\n 
# ends with Unknown\n Unknown
# May have to do some sorting
HH1 %>% 
    filter(path == "OEBPS/part2_split_000.xhtml" ) %>% 
    pull(content)
# a single line
HH1 %>% 
    filter(path == "OEBPS/part2_split_001.xhtml" ) %>% 
    pull(content)


extract_TEXT <-  . %>% 
    filter(str_detect(path, "split_001.xhtml")) %>% 
    mutate(chapter = str_extract(content, "CHAPTER [0-9]{1,3}") %>% 
               str_extract("[0-9]{1,3}") %>% 
               as.integer(),
           content = str_remove(content, "CHAPTER [0-9]{1,3}"),
           content = str_remove(content, "Unknown\n      Unknown")) %>% 
    arrange(chapter) %>% 
    select(chapter, content)
extract_text <- . %>% 
    filter(str_detect(path, "split_001.xhtml")) %>% 
    mutate(chapter = str_extract(content, "Chapter [0-9]{1,3}") %>% 
               str_extract("[0-9]{1,3}") %>% 
               as.integer(),
           content = str_remove(content, "Chapter [0-9]{1,3}"),
           content = str_remove(content, "Unknown\n      Unknown")) %>% 
    arrange(chapter) %>% 
    select(chapter, content)    

HH1 <- 
    HH1 %>% 
    extract_TEXT()
# does it work for the others too?
# 
HH2 <- epub_to_text("data/Adams, Douglas/Restaurant at the End of the Universe, The/Restaurant at the End of the Universe, The - Douglas Adams.epub") %>% 
        extract_text()
HH3 <- 
    epub_to_text("data/Adams, Douglas/Life, the Universe and Everything (Hitchhiker's Guide to the Galaxy)/Life, the Universe and Everything (Hitchhiker's Guide to the Galaxy) - Douglas Adams.epub") %>% 
    extract_text()
HH4 <- 
epub_to_text("data/Adams, Douglas/So Long, and Thanks for All the Fish (Hitchhiker's Guide to the Galaxy)/So Long, and Thanks for All the Fish (Hitchhiker's Guide to the Galaxy) - Douglas Adams.epub")  %>% 
    extract_text()    
# this book has a very different styling. Apparently no 'chapter' 
# but something like \"Mostly Harmless\"\n    \n    \n  10\n  
# Soooo \"Mostly Harmless\" followed by whitespace, line ends number

HH5 <- # mostly harmless
    epub_to_text("data/Adams, Douglas/Mostly Harmless/Mostly Harmless - Douglas Adams.epub") %>% 
    mutate(chapter = str_extract(content, "\"Mostly Harmless\"[:space:]{1,}[0-9]{1,3}") %>%  # this one took a while to find! 
               str_extract("[0-9]{1,3}") %>% 
               as.integer(),
           content = str_remove(content, "\"Mostly Harmless\"[:space:]{1,}[0-9]{1,3}"),
           content = str_remove(content, "Unknown\n      Unknown")) %>% 
    arrange(chapter)
# add part12 split to 12 split 000
text_11 <- 
    HH5 %>% 
    filter(str_detect(path, "part12_split")) %>% 
    pull(content) %>% 
    paste(collapse = "\n")
HH5$content[HH5$chapter ==11] <- text_11
HH5 <- 
    HH5 %>% 
    filter(!is.na(chapter))
## this is weird. this book should be thickest. with 22 chapters, but has only 11. 
## the ebook is indeed 11 chapters and is halfway through total size. so the other chapters are not
## detected?
HH5$content[11] %>% str_extract_all("[0-9]{1,3}")
HH5$content[11] %>% str_extract_all("[0-9]{1,3}")
pubcrawl::epub_to_text("data/Adams, Douglas/Mostly Harmless/Mostly Harmless - Douglas Adams.epub") %>% 
    filter(str_detect(path, "part12_split")) %>% 
    pull(content) %>% 
    #str_extract_all("\"Mostly Harmless\"") # just once...
    str_extract_all("[0-9]{1,3}\n") # sooo just nog chapter 12. I can probably split it again.
## and another thing



### making it tidy ------
library(tidytext)
unnestedHHGTTG<- 
    HHGTTG %>% 
    group_by(book, chapter) %>% 
    unnest_tokens(output = "word",input = content, token = "words") %>% 
    ungroup()

# use afinn and numbers
# NRC surprise, fear, joy, sadness and count
joy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")
surprise <- get_sentiments("nrc") %>% 
    filter(sentiment == "surprise")
fear <- get_sentiments("nrc") %>% 
    filter(sentiment == "fear")
sadness <- get_sentiments("nrc") %>% 
    filter(sentiment == "sadness")

unnestedHHGTTG %>% 
    inner_join(joy) %>%
    group_by(book) %>% 
    count(word, sort = TRUE) %>% 
    top_n(15, wt = n) %>% 
    ungroup() %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = book))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~book,scales = "free")+
    coord_flip()+
    labs(
        title = "Top 25 most frequent joy-words",
        subtitle = "",
        x = "", y = ""
    )

unnestedHHGTTG %>% 
    inner_join(surprise) %>%
    group_by(book) %>% 
    count(word, sort = TRUE) %>% 
    top_n(15, wt = n) %>% 
    ungroup() %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = book))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~book,scales = "free")+
    coord_flip()+
    labs(
        title = "Top 25 most frequent surprise-words",
        subtitle = "",
        x = "", y = ""
    )

unnestedHHGTTG %>% 
    inner_join(fear) %>%
    group_by(book) %>% 
    count(word, sort = TRUE) %>% 
    top_n(15, wt = n) %>% 
    ungroup() %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = book))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~book,scales = "free")+
    coord_flip()+
    labs(
        title = "Top 25 most frequent fear-words",
        subtitle = "",
        x = "", y = ""
    )

unnestedHHGTTG %>% 
    inner_join(sadness) %>%
    group_by(book) %>% 
    count(word, sort = TRUE) %>% 
    top_n(15, wt = n) %>% 
    ungroup() %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = book))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~book,scales = "free")+
    coord_flip()+
    labs(
        title = "Top 25 most frequent sad-words",
        subtitle = "",
        x = "", y = ""
    )


sentiment_chapter <- 
    unnestedHHGTTG %>% 
    inner_join(get_sentiments("bing")) %>%
    count(book, chapter, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(
        sentiment = positive - negative,
        N_sent_words = positive+negative,
        relative_sentiment = sentiment / N_sent_words
        )

sentiment_chapter %>% 
    ggplot(aes(chapter, sentiment, fill = book)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~book, ncol = 2, scales = "free_x")+
    labs(
        title = "Sentiment per chapter",
        subtitle = "Total sentiment per chapter"
    )

sentiment_chapter %>% 
    ggplot(aes(chapter, relative_sentiment, fill = book)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~book, ncol = 2, scales = "free_x")+
    labs(
        title = "Sentiment per chapter",
        subtitle = "relative sentiment per chapter"
    )

