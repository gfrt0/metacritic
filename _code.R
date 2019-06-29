setwd("/Users/gforte/Dropbox/git/metacritic")

if (!require("pacman")) install.packages("pacman")
pacman::p_load('ggplot2','rvest','tidyverse','foreach','doParallel','lubridate')

# Function to read the webpage
pagetotable <- function(list, index) {
  cat("page", index, "\n" )
  url <- read_html(list[index])
  
  table <- html_nodes(url, ".release_product") %>% html_text() %>% unlist()
  table <- gsub("\n\n\n\n\n\n\n", "", table)
  table <- gsub("\\s+", " ", table) %>% trimws()
  
  table_chop <- strsplit(table, "Release Date: |User: |Artist: ")
  table_chop <- data.frame(matrix(unlist(table_chop), nrow=length(table_chop), byrow=T))
  table_chop[,1] <- as.character(table_chop[,1])
  table_chop[,5] <- substr(table_chop[, 1], nchar(table_chop[,1])-2, nchar(table_chop[,1]))
  table_chop[,1] <- substr(table_chop[, 1], 1, nchar(table_chop[,1])-3)
  
  return(table_chop)
}

# Opening the doParallel session 
coresno <- 3
cl <- makeCluster(coresno, outfile="")

#### Loop over album releases by score  ####
# Only albums with seven or more reviews are eligible. 
# EPs, live albums, box sets, re-issues, and compilations are also excluded.

webpages <- paste0("https://www.metacritic.com/browse/albums/score/metascore/all/filtered?sort=desc&page=", 0:118)

registerDoParallel(cl)

list_mc_alltime <- foreach (page = 1:length(webpages), .packages = "rvest", .combine = list, 
                            .errorhandling='pass', .multicombine = T, .maxcombine = 500) %dopar% {
  
    table_chop <- pagetotable(list = webpages, index = page)
    
    return(table_chop)
    
    Sys.sleep(1)
  }

stopCluster(cl)

# refetch error pages iteratively
while (length(unique(sapply(list_mc_alltime, length))) != 1) {
    
    toredo <- which(sapply(list_mc_alltime, length)==2)
    cat("there are", length(toredo), "errors: pages", toredo, "\n")
    for (page in toredo) {
      
      table_chop <- pagetotable(list = webpages, index = page)
      
      list_mc_alltime[[page]] <- table_chop 
      }
  }

df_mc <- do.call("rbind",list_mc_alltime)
colnames(df_mc) <- c("title", "artist", "userscore", "release", "metascore")
df_mc$release <- as.Date(tolower(df_mc$release), "%b %d, %Y")
df_mc$metascore <- as.numeric(df_mc$metascore)
rm(list_mc, list_mc_alltime)

save(df_mc, file = "metacritic_albums.RData")

# load("metacritic_albums.RData")

#### Visualisations ####

#Analysing trends by month and year
df_mc$monthyear <- format(as.Date(df_mc$release, "%Y-%m-%d"), "%Y-%m")
df_mc$year <- format(df_mc$release, "%Y")
# There are only ~20 articles from 1999, too small a sample.
df_mc_old <- df_mc
df_mc <- df_mc[df_mc$year>1999,]

#-->> Monthly ----

monthly <- df_mc %>% group_by(month=floor_date(release, "month")) %>% 
           summarise(average = mean(metascore), count = n_distinct(metascore),
                     p25 = quantile(metascore, probs = .25), 
                     p50 = quantile(metascore, probs = .5),
                     p75 = quantile(metascore, probs = .75))

monthlycritic <- ggplot(data = monthly, show.legend= F, aes(month)) + theme_bw() +
                   geom_line(data=monthly, aes(x=month, y=average)) + 
                   geom_ribbon(data=monthly,aes(ymin=p25,ymax=p75),alpha=0.3) +
                   xlab('Month of Release') + ylab('Metascore') +
                   scale_x_date(date_labels="%b %y", date_minor_breaks="1 month", expand=c(0,0)) +
                   scale_y_continuous(breaks=c(seq(50, 90, by=2))) +
                   theme(plot.title = element_text(lineheight=.8, face="bold", hjust=.5)) +
                   labs(title = "Album Metascores, by Month", 
                        caption = "note: averages in black; first and third quartile in grey.")
print(monthlycritic)


#-->> Quarterly----

quarterly <- df_mc %>% group_by(quarter=floor_date(release, "3 month")) %>% 
             summarise(average = mean(metascore), count = n_distinct(metascore),
                        p25 = quantile(metascore, probs = .25), 
                        p50 = quantile(metascore, probs = .5),
                        p75 = quantile(metascore, probs = .75))

quarterlycritic <- ggplot(data = quarterly, show.legend= F, aes(quarter)) + theme_bw() +
                   geom_line(data=quarterly, aes(x=quarter, y=average)) + 
                   geom_ribbon(data=quarterly,aes(ymin=p25,ymax=p75),alpha=0.3) +
                   xlab('Quarter of Release') + ylab('Metascore') +
                   scale_x_date(date_labels="%b %y", date_minor_breaks="3 month", expand=c(0,0)) +
                   scale_y_continuous(breaks=c(seq(50, 90, by=2))) +
                   theme(plot.title = element_text(lineheight=.8, face="bold", hjust=.5)) +
                   labs(title = "Album Metascores, by Quarter", 
                        caption = "note: averages in black; first and third quartile in grey.")
print(quarterlycritic)

#-->> Yearly ----

yearly <- df_mc %>% group_by(year=floor_date(release, "12 month")) %>% 
          summarise(average = mean(metascore), count = n_distinct(metascore),
                    p25 = quantile(metascore, probs = .25), 
                    p50 = quantile(metascore, probs = .5),
                    p75 = quantile(metascore, probs = .75))

yearlycritic <- ggplot(data = yearly, show.legend= F, aes(year)) + theme_bw() +
                geom_line(data=yearly, aes(x=year, y=average)) + 
                geom_ribbon(data=yearly,aes(ymin=p25,ymax=p75),alpha=0.3) +
                xlab('Year of Release') + ylab('Metascore') +
                scale_x_date(date_labels="%b %y", date_minor_breaks="12 month", expand=c(0,0)) +
                scale_y_continuous(breaks=c(seq(50, 90, by=2))) +
                theme(plot.title = element_text(lineheight=.8, face="bold", hjust=.5)) +
                labs(title = "Album Metascores, by Year", 
                     caption = "note: averages in black; first and third quartile in grey.")
print(yearlycritic)
