#Libraries
library(tidyverse)
library(janitor)
library(tidyr)
library(lubridate)

#Get data
books <- read_csv("bookdata.csv") %>% clean_names()

books$id <- 1:nrow(books)

#Make bookshop links
books$bookshop_link <- paste0("https://bookshop.org/a/113618/", books$isbn)

#Remove dashes from links
books$bookshop_link <- gsub("-", "", books$bookshop_link)

#Structure data for tables
bookstbls <- books[c(1:7,9:12)]
bookstbls$image <- paste0("[![", bookstbls$title,"](", bookstbls$image_url,"'",bookstbls$title,"')](", bookstbls$bookshop_link,")")
bookstbls$title <- paste0("[", bookstbls$title, "](", bookstbls$bookshop_link, ") (",bookstbls$format, ")")
bookstbls$publisher <- paste0(bookstbls$publisher, " (", bookstbls$pub_date, ")")
bookstbls <- bookstbls[c(12,1,4,8,6,2,3,10)]

#create display friendly column headings
names(bookstbls)[2] <- "Title"
names(bookstbls)[3] <- "Author"
names(bookstbls)[4] <- "Synopsis"
names(bookstbls)[5] <- "Publisher"

#make csvs for each group/subgroup table

#fiction
fiction <- bookstbls %>% filter(group == "Fiction") %>% arrange(id)
write.csv(fiction[c(1:5)], "fiction.csv", row.names = FALSE)

#non-fiction: bio and auto
biography <- bookstbls %>% filter(subgroup == "Biography & Autobiography") %>% arrange(id)
write.csv(biography[c(1:5)], "biography.csv", row.names = FALSE)

#non-fiction: self-help
selfhelp <- bookstbls %>% filter(subgroup == "Self-Help & Advice") %>% arrange(id)
write.csv(selfhelp[c(1:5)], "selfhelp.csv", row.names = FALSE)

#non-fiction: lifestyle
lifestyle <- bookstbls %>% filter(subgroup == "Lifestyle") %>% arrange(id)
write.csv(lifestyle[c(1:5)], "lifestyle.csv", row.names = FALSE)

#left, right and center
lrc <- bookstbls %>% filter(group == "Left, Right, and Center") %>% arrange(id)
write.csv(lrc[c(1:5)], "lrc.csv", row.names = FALSE)

#children's
children <- bookstbls %>% filter(group == "Children's") %>% arrange(id)
write.csv(children[c(1:5)], "children.csv", row.names = FALSE)

#young adult
ya <- bookstbls %>% filter(group == "Young Adult") %>% arrange(id)
write.csv(ya[c(1:5)], "ya.csv", row.names = FALSE)

#recommended 
recs <- bookstbls %>% filter(group == "Recommended Books" & is.na(subgroup)) %>% arrange(id)
write.csv(recs[c(1:5)], "recs.csv", row.names = FALSE)

#pick of week
pow <- bookstbls %>% filter(subgroup == "Bruce Wagner's Pick of the Week")
write.csv(pow[c(1:5)], "pow.csv", row.names = FALSE)
