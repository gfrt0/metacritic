setwd("D:/Dropbox (Personal)/git/metacritic")

if (!require("pacman")) install.packages("pacman")
pacman::p_load('ggplot2', 'rvest', 'tidyverse', 'foreach', 'doParallel', 'lubridate', 'stringr')

# Function to fill the fields of the metacritic table 
fields <- function(table, x) {
  dt <- table[x][[1]] %>% html_children()
  link <- strsplit(as.character(dt[4]), "href=\"|\" class=")[[1]][2]
  rest <- dt %>% html_text() %>% gsub("[\n]+|\\s+", " ", .) %>% trimws(., "both") %>% .[-c(1, 3, 8:10)] %>% c(., link)
}

# Function to read the webpage
pagetotable <- function(list, index) {
  cat("page", index, "\n" )
  url <- read_html(list[index])
  
  # table <- html_nodes(url, ".clamp-summary-wrap") %>% html_text()
  # table <- gsub("[\n]+|\\s+", " ", table)
  
  table <- html_nodes(url, ".clamp-summary-wrap")
  table <- lapply(1:length(table), function(x) fields(table, x))
  
  table.df <- bind_rows(lapply(table, function(x) data.frame(t(x))))
  names(table.df) <- c("metascore", "recordname", "recordauthor", "description", "userscore", "link")
  table.df$recordauthor <- gsub("^by ", "", table.df$recordauthor)
  table.df$date <- str_extract(table.df$recordauthor, "[A-Za-z]+ [0-9]+, [0-9]+$")
  table.df$recordauthor <- str_remove(table.df$recordauthor, table.df$date) %>% trimws(., "both")
  table.df$userscore <- str_remove(table.df$userscore, "^Metascore: [0-9]+ User Score: ")
  
  return(table.df)
}

# Opening the doParallel session 
coresno <- 3
cl <- makeCluster(coresno, outfile="")

#### Loop over album dates by score  ####
# Only albums with seven or more reviews are eligible. 
# EPs, live albums, box sets, re-issues, and compilations are also excluded.

webpages <- paste0("https://www.metacritic.com/browse/albums/score/metascore/all/filtered?sort=desc&page=", 0:132) # 132 pages as of 290622

registerDoParallel(cl)

list_mc_alltime <- 
  foreach (page = 1:(length(webpages) - 1), .packages = c("rvest", "dplyr", "stringr"), 
           .combine = list, .errorhandling='pass', 
           .multicombine = T, .maxcombine = 500) %dopar% {

           table.df <- pagetotable(list = webpages, index = page)
            
           return(table.df)
            
           Sys.sleep(1)
        }

registerDoSEQ()

# # refetch error pages iteratively
# while (length(unique(sapply(list_mc_alltime, length))) != 1) {
#     
#     toredo <- which(sapply(list_mc_alltime, length)==2)
#     cat("there are", length(toredo), "errors: pages", toredo, "\n")
#     for (page in toredo) {
#       
#       table_chop <- pagetotable(list = webpages, index = page)
#       
#       list_mc_alltime[[page]] <- table_chop 
#       }
#   }

df_mc <- bind_rows(list_mc_alltime)
df_mc$date <- as.Date(tolower(df_mc$date), "%b %d, %Y")
df_mc$metascore <- as.numeric(df_mc$metascore)
# rm(list_mc_alltime)

write.csv(df_mc, "metacritic_albums.csv")

#### Visualisations ####

#Analysing trends by month and year
df_mc$monthyear <- format(as.Date(df_mc$date, "%Y-%m-%d"), "%Y-%m")
df_mc$year <- format(df_mc$date, "%Y")
# There are only ~20 articles from 1999, too small a sample.
df_mc_old <- df_mc
df_mc <- df_mc[df_mc$year > 1999, ]

#-->> Monthly ----

monthly <- 
   df_mc %>% group_by(month=floor_date(date, "month")) %>% 
   summarise(average = mean(metascore), count = n_distinct(metascore),
             p25 = quantile(metascore, probs = .25), 
             p50 = quantile(metascore, probs = .5),
             p75 = quantile(metascore, probs = .75))

monthlycritic <- 
   ggplot(data = monthly, show.legend= F, aes(month)) + theme_bw() +
   geom_line(data=monthly, aes(x=month, y=average)) + 
   geom_ribbon(data=monthly,aes(ymin=p25,ymax=p75),alpha=0.3) +
   xlab('Month of date') + ylab('Metascore') +
   scale_x_date(date_labels="%b %y", date_minor_breaks="1 month", expand=c(0,0)) +
   scale_y_continuous(breaks=c(seq(50, 90, by=2))) +
   theme(plot.title = element_text(lineheight=.8, face="bold", hjust=.5)) +
   labs(title = "Album Metascores, by Month", 
        caption = "note: averages in black; first and third quartile in grey.")
print(monthlycritic)


#-->> Quarterly----

quarterly <- 
  df_mc %>% group_by(quarter=floor_date(date, "3 month")) %>% 
  summarise(average = mean(metascore), count = n_distinct(metascore),
            p25 = quantile(metascore, probs = .25), 
            p50 = quantile(metascore, probs = .5),
            p75 = quantile(metascore, probs = .75))

quarterlycritic <- 
   ggplot(data = quarterly, show.legend= F, aes(quarter)) + theme_bw() +
   geom_line(data=quarterly, aes(x=quarter, y=average)) + 
   geom_ribbon(data=quarterly,aes(ymin=p25,ymax=p75),alpha=0.3) +
   xlab('Quarter of date') + ylab('Metascore') +
   scale_x_date(date_labels="%b %y", date_minor_breaks="3 month", expand=c(0,0)) +
   scale_y_continuous(breaks=c(seq(50, 90, by=2))) +
   theme(plot.title = element_text(lineheight=.8, face="bold", hjust=.5)) +
   labs(title = "Album Metascores, by Quarter", 
        caption = "note: averages in black; first and third quartile in grey.")
print(quarterlycritic)

#-->> Yearly ----

yearly <- 
  df_mc %>% group_by(year=floor_date(date, "12 month")) %>% 
  summarise(average = mean(metascore), count = n_distinct(metascore),
            p25 = quantile(metascore, probs = .25), 
            p50 = quantile(metascore, probs = .5),
            p75 = quantile(metascore, probs = .75))

yearlycritic <- 
  ggplot(data = yearly, show.legend= F, aes(year)) + theme_bw() +
  geom_line(data=yearly, aes(x=year, y=average)) + 
  geom_ribbon(data=yearly,aes(ymin=p25,ymax=p75),alpha=0.3) +
  xlab('Year of date') + ylab('Metascore') +
  scale_x_date(date_labels="%b %y", date_minor_breaks="12 month", expand=c(0,0)) +
  scale_y_continuous(breaks=c(seq(50, 90, by=2))) +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=.5)) +
  labs(title = "Album Metascores, by Year", 
       caption = "note: averages in black; first and third quartile in grey.")
print(yearlycritic)

rm(monthly, quarterly, yearly)

#### Record information ####

df_mc <- read.csv("metacritic_albums.csv") %>% .[, -1]
df_mc$link <- paste0("https://www.metacritic.com", df_mc$link)

link_records <- paste0(df_mc$link, "/critic-reviews")

critic_reviews <- function(link_records, page, noisy = T) {
  if (noisy) cat("handling record", page, "of", length(link_records),"\n")
  
  critics <- read_html(link_records[page]) %>% html_nodes(".critic_reviews") %>% html_children() %>% as.character(.) %>% 
    gsub("\\s+", " ", .) %>% str_split(., " </div> </div>") %>% lapply(., function(x) x[1:3]) %>% 
    lapply(., function(x) data.frame(t(x))) %>% bind_rows(.)
  names(critics) <- c("content", "brief", "link")
  
  content <- str_split(critics$content, "</div>") %>% lapply(., function(x) data.frame(t(x))) %>% bind_rows(.) %>% .[!is.na(.[, 1]), ] %>% mutate(across(where(is.character), str_trim))
  
  content_cols <- sapply(content, gsub, pattern = "^<[a-z]+ class=\\\"([a-zA-Z_ ]+)\\\">.*", replacement = "\\1")
  cols_vars    <- lapply(1:ncol(content_cols), function(x) names(sort(table(content_cols[, x], useNA = "always"), decreasing = T))) 
  
  content <- content[, grepl("review|date", cols_vars)]
  cols_vars <- cols_vars[grepl("review|date", cols_vars)]
  
  while (sum(grepl("review_grade", cols_vars)) > 1) {
    tomerge <- which(grepl("review_grade", cols_vars))
    content[, tomerge[1]] <- paste0(content[, tomerge[1]], content[, tomerge[2]])
    content[, tomerge[2]] <- NULL
    content_cols <- sapply(content, gsub, pattern = "^<[a-z]+ class=\\\"([a-zA-Z_ ]+)\\\">.*", replacement = "\\1")
    cols_vars    <- lapply(1:ncol(content_cols), function(x) names(sort(table(content_cols[, x], useNA = "always"), decreasing = T))) 
  }
  
  names(content)[grepl("critic_review", cols_vars)] <- "critic" 
  names(content)[grepl("date", cols_vars)] <- "date" 
  names(content)[grepl("review_grade", cols_vars)] <- "score" 
  
  if ("date" %in% names(content)) {
    content$date   <- gsub("<div class=\"date\">", "", content$date) %>% trimws(., "both") %>% as.Date(., "%b %d, %Y")
  }
  
  content$score  <- content$score %>% trimws(., "both") %>% gsub("<.*>", "", .) %>% as.numeric(.)
  content$critic <- gsub("^.*\">(.*)$", "\\1", content$critic) %>% gsub("</a>$", "", .)
  
  critics$content <- NULL
  critics$brief  <- gsub("\\s+<div class=\"review_body\">\\s+", "", critics$brief)
  critics$link   <- gsub(".*class=\"external\" href=\"(.*)\">Read.*", "\\1", critics$link)
  
  critics$link[grepl("<div class=", critics$link)] <- NA 
  
  critics <- cbind(df_mc[page, 2:3], content, critics %>% .[!is.na(.$brief), ])
  
  return(critics)
}

reviews <-
  foreach (page = 1:length(link_records), .packages = c("rvest", "dplyr", "stringr"),
           .combine = list, .errorhandling='pass',
           .multicombine = T, .maxcombine = 50000) %dopar% {

           critics <- critic_reviews(link_records, page, noisy = T)

           return(critics)

           Sys.sleep(sample(.75, 1, 1.25), 1)
           if (page %% 150 == 0) Sys.sleep(30)
        }

save(file = "reviews.RData", reviews)
load("reviews.RData")

fetch.errors <- which(grepl("HTTP error|Failed to parse", reviews) == T)

while (length(fetch.errors) != 0) {
  for (page in fetch.errors) {

    cat("handling fetch error", which(page == fetch.errors), "of", length(fetch.errors), "\n")

    out <- tryCatch(critic_reviews(link_records, page, noisy = F),
                    error = function(cond) return(cond))
    
    reviews[[page]] <- out
    Sys.sleep(runif(1, 1, 2))
    
  }
  fetch.errors <- which(grepl("HTTP error|Failed to parse", reviews) == T)
}

# there are 46 remaining records for which mc links are problematic
# many of these seem to be solved once you remove the band name from the link

link_records_noartist <- data.frame(link = link_records[fetch.errors])
link_records_noartist$toremove <- gsub(".*music/[a-zA-Z-]+(/[a-zA-Z-]+)/critic-reviews$", "\\1", link_records_noartist$link)
link_records_noartist$toremove[grepl("https://", link_records_noartist$toremove)] <- ""
link_records_noartist$refetch <- sapply(1:nrow(link_records_noartist), function(x) sub(link_records_noartist$toremove[x], "", link_records_noartist$link[x]))

link_records_na <- link_records
link_records_na[fetch.errors] <- link_records_noartist$refetch

while (length(fetch.errors) != 0) {
  for (page in fetch.errors) {
    
    cat("handling fetch error", which(page == fetch.errors), "of", length(fetch.errors), "\n")
    
    out <- tryCatch(critic_reviews(link_records_na, page, noisy = F),
                    error = function(cond) return(cond))
    
    reviews[[page]] <- out
    Sys.sleep(runif(1, 1, 2))
    
  }
  fetch.errors <- which(grepl("HTTP error|Failed to parse", reviews) == T)
}

# after this step there remain 10 errors: records with numbers/non-characters as titles.

link_records_na[fetch.errors]

# fix by hand 
df_mc[fetch.errors[1], c(2:3, 6)] # can't find

df_mc[fetch.errors[2], c(2:3, 6)] # can't find

df_mc[fetch.errors[3], c(2:3, 6)] # can't find

df_mc[fetch.errors[4], c(2:3, 6)]
df_mc[fetch.errors[4], 3]     <- "Sunn O)))"
df_mc[fetch.errors[4], 6]     <- "https://www.metacritic.com/music/terrestrials/sunn-0)))"
link_records[fetch.errors[4]] <- "https://www.metacritic.com/music/terrestrials/sunn-0)))"

df_mc[fetch.errors[5], c(2:3, 6)] # can't find

df_mc[fetch.errors[6], c(2:3, 6)] # can't find

df_mc[fetch.errors[7], c(2:3, 6)] # can't find 

df_mc[fetch.errors[8], c(2:3, 6)] # can't find
df_mc[fetch.errors[8], 6]     <- "https://www.metacritic.com/music/betty-wright-the-movie/the-roots"
link_records[fetch.errors[8]] <- "https://www.metacritic.com/music/betty-wright-the-movie/the-roots"
  
df_mc[fetch.errors[9], c(2:3, 6)] # can't find

df_mc[fetch.errors[10], c(2:3, 6)] # can't find

while (length(fetch.errors) != 0) {
  for (page in fetch.errors) {
    
    cat("handling fetch error", which(page == fetch.errors), "of", length(fetch.errors), "\n")
    
    out <- tryCatch(critic_reviews(link_records, page, noisy = F),
                    error = function(cond) return(cond))
    
    reviews[[page]] <- out
    Sys.sleep(runif(1, 1, 2))
    
  }
  fetch.errors <- which(grepl("HTTP error|simpleError", reviews) == T)
}

# we end the search with 8 errors. 

# NAs

reviews.na <- unlist(lapply(reviews, function(x) all(is.na(x))))

count.reviews.na <- sum(reviews.na)

fetch.nas <- which(reviews.na == T)

while (length(fetch.nas) != 0) {
  for (page in fetch.nas) {
    
    cat("handling fetch NA", which(page == fetch.nas), "of", length(fetch.nas), "\n")
    
    out <- tryCatch(critic_reviews(link_records, page, noisy = F),
                    error = function(cond) return(cond))
    
    reviews[[page]] <- out
    Sys.sleep(runif(1, 1, 2))
    
  }
  fetch.nas <- which(unlist(lapply(reviews, function(x) all(is.na(x)))) == T)
}

# all of the NAs are cleared.

# remove 8 remaining errors. 

fetch.errors <- which(grepl("HTTP error|simpleError", reviews) == T)

for (i in length(fetch.errors):1) {
  reviews[[fetch.errors[i]]] <- NULL
}

# vctrs errors 

vctrs.errors <- grepl("vctrs", reviews) 
sum(vctrs.errors)

vctrs.errors <- which(vctrs.errors == T)

while (length(vctrs.errors) != 0) {
  for (page in vctrs.errors) {
    
    cat("handling vctrs error", which(page == vctrs.errors), "of", length(vctrs.errors), "\n")

    reviews[[page]] <- critic_reviews(link_records, page, noisy = F)
    Sys.sleep(runif(1, 1, 2))
    
  }
  vctrs.errors <-  which(grepl("vctrs", reviews) == T)
}

# make dataset.

reviews.df <- bind_rows(reviews)

write.csv(reviews.df, "reviews.csv")

sort(unique(reviews.df$score))

sort(unique(reviews.df$critic))

sort(unique(reviews.df$recordauthor))

min(df_mc$date, na.rm = T)

max(df_mc$date, na.rm = T)

full_data <- merge(x = reviews.df, y = df_mc, by = c("recordname", "recordauthor"), all = T)

full_data$date.x <- NULL
full_data$link.y <- NULL

names(full_data)[grepl("link", names(full_data))] <- "link"
names(full_data)[grepl("date", names(full_data))] <- "date"

# most of these are errors above
full_data[is.na(full_data$score), ]

full_data <- full_data[!is.na(full_data$score), ]

write.csv(full_data, "mc_critic_reviews.csv")
