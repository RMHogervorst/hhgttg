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


extract_text <-  . %>% 
    mutate(chapter = str_extract(content, "CHAPTER [0-9]{1,3}") %>% 
               str_extract("[0-9]{1,3}") %>% 
               as.integer(),
           content = str_remove(content, "CHAPTER [0-9]{1,3}"),
           content = str_remove(content, "Unknown\n      Unknown")) %>% 
    arrange(chapter) %>% 
    select(chapter, content)
    

HH1 <- 
    HH1 %>% 
    filter(str_detect(path, "split_001.xhtml")) %>% 
    extract_text()

# does it work for the others too?
# 
HH2 <- epub_to_text("data/Adams, Douglas/Restaurant at the End of the Universe, The/Restaurant at the End of the Universe, The - Douglas Adams.epub")
HH2 %>% 
    filter(str_detect(path, "split_001.xhtml")) %>% 
    extract_text()
# it does not work because the chapter is not in capitals.
HH2 %>% 
    filter(path == "OEBPS/part1.xhtml") %>% 
    pull(content) # well it does throw away this part ... I can accept that.
    
HH2 %>% 
    filter(str_detect(path, "split_001.xhtml")) %>% 
    mutate(chapter = str_extract(content, "Chapter [0-9]{1,3}") %>% 
               str_extract("[0-9]{1,3}") %>% 
               as.integer(),
           content = str_remove(content, "Chapter [0-9]{1,3}"),
           content = str_remove(content, "Unknown\n      Unknown")) %>% 
    arrange(chapter) %>% 
    select(chapter, content) #%>% 
    #pull(content) %>% .[1]
