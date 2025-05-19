#Libraries
library(tidyverse)
library(janitor)
library(tidyr)
library(lubridate)
library(magick)

#Get data
books <- read_csv("/Users/searley/Downloads/bookdata.csv") %>% clean_names()

books$id <- 1:nrow(books)


#Make bookshop links
books$bookshop_link <- paste0("https://bookshop.org/a/113618/", books$isbn)

#Remove dashes from links
books$bookshop_link <- gsub("-", "", books$bookshop_link)

#Format data for html
bookstbls <- books[c(1:7,9:12)]
bookstbls$title <- gsub("'", "&#39;", bookstbls$title)
bookstbls$image <- paste0("<a href='", bookstbls$bookshop_link, "' target='_blank'><img style='float: left;width: 120px;padding: 0 20px 20px 0;margin-top: 5px' alt='", bookstbls$title, "book cover image' src='", bookstbls$image_url, "'></a>" )
bookstbls$publisher <- paste0("<em>", bookstbls$publisher, " (", bookstbls$pub_date, ")</em>" )
bookstbls$title <- paste0("<a href='", bookstbls$bookshop_link, "' target='_blank' style='color:#1c2551;'>", bookstbls$title, "</a>")
bookstbls$blurb <- gsub("[\r\n]", "", bookstbls$blurb)

bookstbls <- bookstbls[c(12,1,4,8,6,2,3,10)]



#make dataframes for each group/subgroup 

#fiction
fiction <- bookstbls %>% filter(group == "Fiction") %>% arrange(id)
fiction <- fiction[c(1:5)]

#non-fiction: bio and auto
biography <- bookstbls %>% filter(subgroup == "Biography & Autobiography") %>% arrange(id)
biography <- biography[c(1:5)]

#non-fiction: self-help
selfhelp <- bookstbls %>% filter(subgroup == "Self-Help & Advice") %>% arrange(id)
selfhelp <- selfhelp[c(1:5)]

#non-fiction: lifestyle
lifestyle <- bookstbls %>% filter(subgroup == "Lifestyle") %>% arrange(id)
lifestyle <- lifestyle[c(1:5)]

#left, right and center
lrc <- bookstbls %>% filter(group == "Left, Right, and Center") %>% arrange(id)
lrc <- lrc[c(1:5)]

#children's
children <- bookstbls %>% filter(group == "Children's") %>% arrange(id)
children <- children[c(1:5)]

#young adult
ya <- bookstbls %>% filter(group == "Young Adult") %>% arrange(id)
ya <- ya[c(1:5)] 

#recommended 
recs <- bookstbls %>% filter(group == "Recommended Books" & is.na(subgroup)) %>% arrange(id)
recs <- recs[c(1:5)]

#pick of week
pow <- bookstbls %>% filter(subgroup == "Bruce Wagner's Pick of the Week")
pow <- pow[c(1:5)]



#Compile HTML




build_html <- function(topic) {
paste0(topic$image, "<strong>", topic$title, "</strong><br>", topic$author, "<br>", topic$publisher, "<p style='overflow: hidden;'>", topic$blurb,"</p><div style='clear: both'></div><hr style='margin-top:-16px;'>")
}


build_pow <- function(topic) {
  paste0(topic$image, "<strong>", topic$title, "</strong><br>", topic$author, "<br>", topic$publisher, "<div style='clear: both'></div><div style='position:relative;top:-22px;'>", topic$blurb,"</div><br><br><hr style='margin-top:-16px;'>")
}

fictionhtml <- build_html(fiction)
biohtmml <- build_html(biography)
helphtml <- build_html(selfhelp)
lifehtml <- build_html(lifestyle)
lrchtml <- build_html(lrc)
childhtml <- build_html(children)
yahtml <- build_html(ya)
powhtml <- build_pow(pow)
recshtml <- build_pow(recs)


powhtml <- gsub("<p>", "<p style='margin-bottom:16px;'>", powhtml)
powhtml <- gsub("<ul>", "<ul style='margin-bottom:16px;'>", powhtml)

recshtml <- gsub("<p>", "<p style='margin-bottom:16px;'>", recshtml)
recshtml <- gsub("<ul>", "<ul style='margin-bottom:16px;'>", recshtml)

bookshtml <- paste0("<p>The Baltimore Sun’s weekly bestseller lists of fiction, non-fiction, biography, lifestyle, political, children's and young adult books are compiled by staff members of <a href='https://www.skyhorsepublishing.com/' target='_blank' rel='noopener'>Skyhorse Publishing</a> based on publishing houses', booksellers', online retailers' and other sources' sales data covering the prior Monday through Sunday. The reviews and recommendations are compiled by Skyhorse Publishing staff. The Sun welcomes suggestions on local authors to feature via email at <a href='mailto:books@baltsun.com'>books@baltsun.com</a>.</p>
<p>As a participant in Bookshop.org's affiliate program, The Baltimore Sun earns a small commission from qualifying purchases through affiliate links on this page.</p><h3 style='color: #fff; padding: 5px 5px 5px 7px; background-color: #1c2551; font-size: .925em; font-family:","Droid Sans", "sans-serif; width: 120px; text-transform: uppercase;'>Bestsellers</h3>
                    <h4 style='border-top:3px solid #000;padding-top:10px;'>Fiction</h4>", toString(fictionhtml),
                    "<h4 style='border-top:3px solid #000;padding-top:10px;'>Non-fiction: Biography & autobiography</h4>", toString(biohtmml),
                    "<h4 style='border-top:3px solid #000;padding-top:10px;'>Non-fiction: Self-help & advice</h4>", toString(helphtml),
                    "<h4 style='border-top:3px solid #000;padding-top:10px;'>Non-fiction: Lifestyle</h4>", toString(lifehtml),
                    "<h4 style='border-top:3px solid #000;padding-top:10px;'>Left, right, and center</h4>", toString(lrchtml),
                    "<h4 style='border-top:3px solid #000;padding-top:10px;'>Children’s</h4>", toString(childhtml),
                    "<h4 style='border-top:3px solid #000;padding-top:10px;'>Young adult</h4>", toString(yahtml),
                    "<h3 style='color: #fff; padding: 5px 5px 5px 7px; width:198px!important;background-color: #1c2551; font-size: .925em; font-family:","Droid Sans", "sans-serif; text-transform: uppercase;'>Recommended Books</h3>",
                    toString(recshtml),
                    "<h4 style='border-top:3px solid #000;padding-top:10px;'>Bruce Wagner's Pick of the Week</h4>", toString(powhtml), "<br><br>"
                     )


#remove commas from toStrings and fix font style rule
bookshtml <- gsub("</div><hr style='margin-top:-16px;'>, <a", "</div><hr style='margin-top:-16px;'><a", bookshtml)
bookshtml <- gsub("<h3 style='color: #fff;", '<h3 style="color: #fff;', bookshtml)
bookshtml <- gsub("uppercase;'", 'uppercase;"', bookshtml)
bookshtml <- gsub("Droid Sans", "'Droid Sans', ", bookshtml)
bookshtml <- gsub("<hr style='margin-top:-16px;'><h4", "<h4", bookshtml)
bookshtml <- gsub("<hr style='margin-top:-16px;'><h3", "<h3", bookshtml)
bookshtml <- gsub("<h5>", "<h5 style='padding-bottom:5px;border-bottom:1px solid #000;margin-top:25px;margin-bottom:16px;'>", bookshtml)
bookshtml <- gsub("<hr style='margin-top:-16px;'>, ", "<hr style='margin-top:-16px;'>", bookshtml)
bookshtml <- gsub("<br><hr style='margin-top:-16px;'><br><br>", "<br><br>", bookshtml)
bookshtml <- gsub("<ul>", "<ul style='margin-left:15px;'>", bookshtml)

writeLines(text = bookshtml, con = "bookshtml.txt")

#Create thumbnail image

topimages <- books

topimages$groupnarrow <- paste0(topimages$group, "_", topimages$subgroup)

grouped_images <- topimages %>%
  group_by(groupnarrow)

topimages <- grouped_images %>%
  slice_min(id, n = 1) %>%
  ungroup()

image_urls <- topimages$image_url


tn1 <- image_read(image_urls[1])
tn2 <- image_read(image_urls[2])
tn3 <- image_read(image_urls[3])
tn4 <- image_read(image_urls[4])
tn5 <- image_read(image_urls[5])
tn7 <- image_read(image_urls[7])
tn8 <- image_read(image_urls[8])
tn9 <- image_read(image_urls[9])

compositeimg <- c(tn1, tn3, tn4, tn5, tn7, tn9, tn8, tn2)


input <- rep(compositeimg, 1)

# Create a panel (a montage)
p <- image_montage(input, geometry = 'x200+20+20', tile = '4x2', bg = 'black')

p <- image_convert(p, "jpg")
image_write(p, "TBS-L-BOOKSTN.jpg")



