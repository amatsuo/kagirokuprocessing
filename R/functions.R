#' Character count after removing non-speech sections
#'
#' @name speech_char_count
#' @param textstring speech text
#' @importFrom magrittr %>%
#' @return integer, number of characters in the speech
#' @export
#'
speech_char_count <- function(textstring) {
  return(textstring %>% stringi::stri_replace_first_regex("^○\\S+\\s", "") %>%
           stringi::stri_replace_all_regex("\\s+", "") %>%
           stringi::stri_replace_all_regex("〔.+〕", "") %>%
           stringi::stri_replace_all_regex("（.+）", "") %>%
           nchar)
}

#' Remove non-speech sections from speech
#'
#' @description To be written
#' @param textstring speech text
#' @importFrom magrittr %>%
#' @return cleaned speech
#' @export
#'


speech_clean <- function(textstring) {
  return(textstring %>% stringi::stri_replace_first_regex("^○\\S+\\s", "") %>%
           stringi::stri_replace_all_regex("\\s+", "") %>%
           stringi::stri_replace_all_regex("〔.+〕", "") %>%
           stringi::stri_replace_all_regex("（.+）", ""))
}

#' Extract speaker capacity from speech text.
#'
#' @description To be written
#' @param textstring speech text
#' @param other bool. whether unmached text will be replaced to "Other"
#' @importFrom magrittr %>%
#' @return text of speaker capacity
#' @export
#'


extract_capacity <- function(textstring, other = TRUE){
  capacity <- textstring %>%
    stringi::stri_sub(1, 20) %>%
    stringi::stri_replace_first_regex("^(○\\S+?)(\\s|\\n).+", "$1") %>%
    stringi::stri_replace_first_regex("\\n.*", "") %>%
    stringi::stri_replace_first_regex( "^.+?(参事|政府特別補佐人|内閣官房|最高裁判所長官代理者|主査|議員|副?大臣|副?議長|委員|参考人|分科員|公述人|君|説明員|参考人|公述人|事務総長|国会図書館長|法制局長(（.+）)?$)", "$1") %>%
    stringi::stri_replace_first_regex("（.+）", "") %>%
    stringi::stri_trim_both()

  if(other){
    capacity <- capacity %<>%
      stringi::stri_replace_first_regex("^○.+", "Other")

  }
  return(capacity)
}

