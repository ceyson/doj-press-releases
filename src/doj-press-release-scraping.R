### Inits
libs <- c('tidyverse','stringr','rvest','tictoc')
new.libs <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.libs)) install.packages(new.libs)
lapply(libs, require, character.only = TRUE)

## Last page index
idx_last_page <- rvest::read_html('https://www.justice.gov/news?keys=medicare+medicaid&items_per_page=25&page=0') %>%
  rvest::html_nodes(".pager__item--last") %>%
  html_children() %>%
  html_attr('href') %>%
  stringr::str_locate_all('=') %>%
  unlist() %>%
  dplyr::last()


## Data frame to store results
## (1) Release date
## (2) Headline
## (3) Story
## (4) URL


## Empty data frame
df_doj <- data.frame(date = character(),
                     headline = character(),
                     story = character(),
                     url = character())


## Scraping
tictoc::tic('Execution')
for (n_page in c(0:idx_last_page)) {
  
  ## Feedback
  print(paste0('Scraping page ',n_page+1,' of ',idx_last_page+1))
  
  ## List of URLs
  urls <- rvest::read_html(paste0('https://www.justice.gov/news?keys=medicare+medicaid&items_per_page=25&page=',n_page)) %>%
    rvest::html_nodes(".views-field-title") %>%
    rvest::html_children() %>%
    rvest::html_children() %>%
    rvest::html_attr('href')
  
  for (url in urls) {
    
    ## Nice
    sample(1:2,1) %>% Sys.sleep()
    
    ## Current url
    url <- paste0('https://www.justice.gov',url)
    
    ## Read URL
    result <- rvest::read_html(url)
    #result <- rvest::read_html(curl::curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
    
    ## Release date
    date <- result %>%
      rvest::html_nodes(".field--name-field-pr-date") %>%
      rvest::html_children() %>%
      rvest::html_children() %>%
      rvest::html_children() %>%
      rvest::html_text()
    
    ## Headline
    headline <- result %>%
      rvest::html_nodes("#node-title") %>%
      rvest::html_text()
    
    ## Story
    story <- result %>%
      rvest::html_nodes(".field--name-field-pr-body") %>%
      rvest::html_children() %>%
      rvest::html_text()
    
    ## Update data frame
    df_doj <- df_doj %>%
      dplyr::add_row(date = date,
                     headline = headline,
                     story = story,
                     url = url)
    
  }
}
tictoc::toc() # Execution: 2556.79 sec elapsed


df_doj %>% dim() # [1] 1313    4

## Write initial data
df_doj %>%
  dplyr::distinct() %>%
  dplyr::mutate(date = stringr::word(date, start = 2, end = 3, sep = ',') %>%
                  stringr::str_trim() %>%
                  gsub(',','',.) %>%
                  gsub(' ','/',.) %>%
                  as.Date(date, format = "%B/%d/%Y"),
                row_id = row_number()) %>%
  dplyr::filter(row_id != 1078) %>%
  dplyr::select(-row_id) %>%
  write.csv('./input/doj_press_releases.csv', fileEncoding = 'Windows-1252', row.names = FALSE)


