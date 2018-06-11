### functions used in this project
# extracting content from epub frames ---- 
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


## other utility functions ------