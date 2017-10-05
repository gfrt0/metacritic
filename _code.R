library('ggplot2')
library('rvest')
library('tidyverse')


#Storage data.frame
df_mc = data.frame()

#### Loop over Artist By Name pages ####


webpages <- c(apply(expand.grid("http://www.metacritic.com/browse/albums/artist", 
              c("", paste0("/", letters )), "?view=condensed&page", 1:24), 1, paste, collapse=""))
webpages <- gsub(" ", "", webpages)

# url_check is used to check we are not re-scraping the same page
url_check <- ""

for (page in 1:length(webpages)) {

  url <- read_html(webpages[page])
  if (identical(url, url_check)) next
  
  table <- html_nodes(url, ".release_product") %>% html_text() %>% unlist()
  table <- gsub("\n\n\n\n\n\n\n", "", table)
  table <- gsub("\\s+", " ", table) %>% trimws()
  
  table_chop <- strsplit(table, "Release Date: |User: |Artist: ")
  table_chop <- table_chop %>% as.data.frame() %>% t() %>% data.frame()
  table_chop$rating <- ""
  q <- nchar(as.character(table_chop$X1))
  table_chop$rating <- substr(table_chop$X1,  q - 3, q) %>% trimws()
  table_chop$X1 <- table_chop$X1 %>% substr( 1, q - 4) %>% trimws()
  rownames(table_chop) <- NULL
  clnms <- c("Title", "Artist", "User Score", "Release Date", "Metascore")
  colnames(table_chop) <- clnms
  
  url_check <- url
  
  df_mc <- rbind(df_mc, table_chop)
  df_mc <- unique(df_mc)
}

#Formatting date
df_mc$date <- tolower(df_mc$`Release Date`)
df_mc$date <- gsub(",", "", df_mc$date) 
df_mc$date <- gsub(" ", "", df_mc$date)
df_mc$date[nchar(df_mc$date)==8] <- paste0(substr(df_mc$date[nchar(df_mc$date)==8],1,3), 0,
                                          substr(df_mc$date[nchar(df_mc$date)==8],4,nchar(df_mc$date[nchar(df_mc$date)==8])))
df_mc$`Release Date` <- as.Date(df_mc$date, "%b%d%Y")
df_mc$date <- NULL

save(df_mc, file = "metacritic_music.RData")


#### Loop over All Album Releases pages  ####

#Storage data.frame
df_mc_alltime = data.frame()

webpages <- c(paste0("http://www.metacritic.com/browse/albums/release-date/available/metascore?page=", 0:77))

# url_check is used to check we are not re-scraping the same page
url_check <- ""

for (page in 1:length(webpages)) {
  
  url <- read_html(webpages[page])
  if (identical(url, url_check)) next
  
  table <- html_nodes(url, ".release_product") %>% html_text() %>% unlist()
  table <- gsub("\n\n\n\n\n\n\n", "", table)
  table <- gsub("\\s+", " ", table) %>% trimws()
  
  table_chop <- strsplit(table, "Release Date: |User: |Artist: ")
  table_chop <- table_chop %>% as.data.frame() %>% t() %>% data.frame()
  table_chop$rating <- ""
  q <- nchar(as.character(table_chop$X1))
  table_chop$rating <- substr(table_chop$X1,  q - 3, q) %>% trimws()
  table_chop$X1 <- table_chop$X1 %>% substr( 1, q - 4) %>% trimws()
  rownames(table_chop) <- NULL
  clnms <- c("Title", "Artist", "User Score", "Release Date", "Metascore")
  colnames(table_chop) <- clnms
  
  url_check <- url
  
  df_mc_alltime <- rbind(df_mc_alltime, table_chop)
  df_mc_alltime <- unique(df_mc_alltime)
  
  Sys.sleep(1)
}

#Formatting date
df_mc_alltime$date <- tolower(df_mc_alltime$`Release Date`)
df_mc_alltime$date <- gsub(",", "", df_mc_alltime$date) 
df_mc_alltime$date <- gsub(" ", "", df_mc_alltime$date)
df_mc_alltime$date[nchar(df_mc_alltime$date)==8] <- paste0(substr(df_mc_alltime$date[nchar(df_mc_alltime$date)==8],1,3), 0,
                                                           substr(df_mc_alltime$date[nchar(df_mc_alltime$date)==8],4,nchar(df_mc_alltime$date[nchar(df_mc_alltime$date)==8])))
df_mc_alltime$`Release Date` <- as.Date(df_mc_alltime$date, "%b%d%Y")
df_mc_alltime$date <- NULL

df_mc_alltime <- df_mc_alltime[order(as.character(df_mc_alltime$Artist), df_mc_alltime$`Release Date`),]
rownames(df_mc_alltime) <- NULL

save(df_mc_alltime, file = "metacritic_alltime.RData")


#### Visualisations ####


#Analysing trends by month and year
df_mc_alltime$Month_Yr <- as.Date(df_mc_alltime$`Release Date`, "%Y-%m")
df_mc_alltime$Yr <- format(df_mc_alltime$`Release Date` , "%Y")
df_mc_alltime$Metascore <- as.numeric(df_mc_alltime$Metascore)
# There are only ~20 articles from 1999, too small a sample.
df_mc_cut <- df_mc_alltime[df_mc_alltime$Yr>1999,]

#-->> Monthly ----
month1 <- df_mc_cut %>% mutate(month1 = as.Date(cut(Month_Yr, breaks = "1 month"))) %>%
  group_by(month1) %>% 
  summarise(average = mean(Metascore), count = n_distinct(Metascore))

monthlycritic <- ggplot(show.legend= F) +
                   geom_line(data=month1, aes(x=month1, y=average)) + 
                   xlab('Month of Release') + ylab('Average Metascore') + 
                   scale_x_date(date_labels =  "%b %y", date_minor_breaks = "1 month", expand=c(0,0)) +
                   scale_y_continuous(breaks=c(seq(60, 80, by=1))) +
                   theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5)) + 
                   ggtitle("Average Album Metascore by Month")

print(monthlycritic)


#-->> Quarterly----
month3 <- df_mc_cut %>% mutate(month3 = as.Date(cut(Month_Yr, breaks = "3 month"))) %>%
  group_by(month3) %>% 
  summarise(average = mean(Metascore), count = n_distinct(Metascore))

quarterlycritic <- (ggplot(data=month3, aes(x=month3, y=average, width=.7)) +
                      geom_line() + 
                      xlab('Quarter of Release') + ylab('Average Metascore')) + 
  scale_x_date(date_labels =  "%b %y", date_minor_breaks = "3 month", expand=c(0,0)) +
  scale_y_continuous(breaks=c(seq(70,80,by=1))) +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5)) + 
  ggtitle("Average Album Metascore by Quarter")

print(quarterlycritic)


#-->> Quarterly with Albums Scored----
month3 <- df_mc_cut %>% mutate(month3 = as.Date(cut(Month_Yr, breaks = "3 month"))) %>%
  group_by(month3) %>% 
  summarise(average = mean(Metascore), count = n_distinct(Metascore))

quarterlyalbcritic <- (ggplot(data=month3, aes(x=month3, y=average, width=.7)) +
                       geom_line(color="red") + geom_step(data=month3, aes(x=month3, y=count)) +
                       xlab('Quarter of Release') + ylab('Average Metascore')) + 
  scale_x_date(date_labels =  "%b %y", date_minor_breaks = "3 month", expand=c(0,0)) +
  scale_y_continuous(breaks=c(seq(0,100,by=5)), sec.axis = sec_axis(~., name = "Albums Scored")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5), legend.position = c(0.8, 0.9)) + 
  ggtitle("Average Album Metascore by Quarter")

print(quarterlyalbcritic)


#-->> Yearly ----
year1 <- df_mc_cut %>% mutate(year1 = as.Date(cut(Month_Yr, breaks = "1 year"))) %>%
  group_by(year1) %>% 
  summarise(average = mean(Metascore), count = n_distinct(Metascore))

yearlycritic <- (ggplot(data=year1, aes(x=year1, y=average), show.legend= F) +
                       geom_line() +
                       xlab('Year of Release') + ylab('Average Metascore')) + 
                       scale_x_date(date_minor_breaks = "1 year") + scale_y_continuous(breaks=c(seq(70, 80,by=1))) +
                       theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5)) + 
                       ggtitle("Average Album Metascore by Year")

print(yearlycritic)

