#### load all data
library(tidyverse)
# I also use pubcrawl from Bob Rudis (@hrbrmstr)

source("R/utils.R")

HH1 <- 
    pubcrawl::epub_to_text("data/Adams, Douglas/Hitchhiker's Guide to the Galaxy, The/Hitchhiker's Guide to the Galaxy, The - Douglas Adams.epub") %>% 
    extract_TEXT()
HH2 <- 
    pubcrawl::epub_to_text("data/Adams, Douglas/Restaurant at the End of the Universe, The/Restaurant at the End of the Universe, The - Douglas Adams.epub") %>% 
    extract_text()

HH3 <- 
    pubcrawl::epub_to_text("data/Adams, Douglas/Life, the Universe and Everything (Hitchhiker's Guide to the Galaxy)/Life, the Universe and Everything (Hitchhiker's Guide to the Galaxy) - Douglas Adams.epub") %>% 
    extract_text()
HH4 <- 
    pubcrawl::epub_to_text("data/Adams, Douglas/So Long, and Thanks for All the Fish (Hitchhiker's Guide to the Galaxy)/So Long, and Thanks for All the Fish (Hitchhiker's Guide to the Galaxy) - Douglas Adams.epub")  %>% 
    extract_text() 

HHGTTG <- bind_rows(
    "Hitchhiker's Guide to the Galaxy" = HH1,
    "Restaurant at the End of the Universe" = HH2,
    "Life, the Universe and Everything" =HH3,
    "So Long, and Thanks for All the Fish" = HH4, 
    .id = "book"
)

rm(HH1, HH2, HH3, HH4)
