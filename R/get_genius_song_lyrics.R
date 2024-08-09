#' @title Get Song Lyrics
#' @name get_genius_song_lyrics
#' @description This function gets the lyrics of the desired song.
#' @author Alberto Almuiña
#' @param song_id Genius Song ID
#' @param output 'tibble' or 'text'. Default: tibble.
#' @param access_token Genius Web API token. Defaults to Sys.getenv('GENIUS_API_TOKEN').
#' @param url If for some reason the function cannot find the url to download the letter, you can use a genius url to download it.
#' @return
#' Returns a tibble or a character vector with the lyrics
#' @details To use this function, you need to first use 'search_genius_song' to get the song ID.
#' @export
#' @examples
#' \dontrun{
#' get_genius_song_lyrics('55882', output = 'text')
#' }


get_genius_song_lyrics<-function(song_id, output = 'tibble', url = NULL,
                                 access_token = Sys.getenv('GENIUS_API_TOKEN')){

  output<-match.arg(output, c('tibble', 'text'))

  if(is.null(url)){

    url<-get_genius_song(song_id = song_id, access_token = access_token) %>% .$url

  } else {url<-url}

  # updated code using rvest 1.0.0 functions that replaced previous functions (https://rvest.tidyverse.org/reference/rename.html)
  # and made some changes so that it will work with the changes that have been made to the html on genius
  lyrics <-read_html(url) %>% 
      html_elements(xpath = "//*[contains(@class, 'ReferentFragmentdesktop')]") %>%
      html_element(xpath = "span") %>%
      html_text2() 

  removing<-purrr::partial(str_replace_all, pattern = '\\[.*?\\]', replacement = '')

  removing2<-purrr::partial(str_replace_all, pattern = '\n', replacement = ' ')

  lyrics <- lyrics %>% removing() %>% removing2() %>% .[!is.na(.) & .!=""]

  if(output == 'text'){

    # make text
    lyrics <- toString(lyrics)

  } else{

  # make tibble (basically same as code from original repo
  lyrics<- lyrics %>%
        enframe(name = NULL, value = 'Lyrics') %>% 
        as_tibble() %>% filter(Lyrics != '')

  }

  return(lyrics)

}
