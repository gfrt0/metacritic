# metacritic

## Purpose
1. To use Metacritic scores to analyse music review inflation, i.e. the apparent increase (decrease) in the proportion of released albums the receive a positive (negative) review.
2. To create a dataset to more broadly explore album releases.

## Inputs and Output
The web-scraped tables come from [Metacritic](http://www.metacritic.com/browse/albums/release-date/available/metascore?page=0). The code extracts information from each table-page (200~ albums in each page) by first scraping their content through rvest and then making use of regularities in the spacing to separate album information. 

For each record, the final dataset contains (where available):
* Artist
* Title
* Metascore
* Release Date
* Critic Name
* Critic Score
* Critic Review
* Review Link
 

## Limitations
The code currently does not differentiate between EPs, LPs etc., which would be nice. Also, the plan is to add more information about each album (e.g. recording label). I also wouldn'y mind adding scores by theneedledrop, as recorded in [this Google sheet](https://docs.google.com/spreadsheets/d/1GbGyWVtePH8RZCZd7N3RPDh8m-K6hgO6AyKsAHZpbeQ/edit#gid=0), to compare Anthony's marks with thosee of major publications.

## Motivation 
This is another small project I worked on to teach myself a bit of R and get into web-scraping. It was motivated by a [video](https://www.youtube.com/watch?v=wOqrhG2DTe8) on the theneedledrop channel discussing a [WSJ article](https://www.wsj.com/articles/what-happened-to-the-negative-music-review-1502535600?mg=prod/accounts-wsj) on the decline of negative reviews.
