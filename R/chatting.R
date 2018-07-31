#get_sign
params_trans <- function(params, app_key) {
  params_run <- params[nchar(params) > 0]
  params_run <- params_run[order(names(params_run))]
  params_str <- paste0(names(params_run), '=', params_run, '&',collapse = '')
  str <- paste0(params_str, 'app_key=', app_key)
  sign_str <- toupper(digest::digest(str,algo="md5", serialize=FALSE))

  params['sign'] <- sign_str
  params_str <- paste0(names(params), '=', params, '&',collapse = '')
  params_str <- stringr::str_sub(params_str, 1, nchar(params_str) - 1)
  return(params_str)
}

#' A API to tencent AI lab's chatting robots
#'
#' \code{chatting} returns the reply of chatting robots
#' @param text The text is sent to API
#' @param app_key Your app key
#' @param app_id Your app ID
#' @return returns the reply of chatting robots
#' @author Ao Sun <\url{https://ao-sun.github.io/}
#' @export
chatting<- function(app_id = app_id, app_key = app_key, keywords) {
  timestamp_num <- function() {ceiling(as.numeric(as.POSIXct(Sys.time(), format="%Y-%m-%d %H:%M:%S")))}
  nonce_run <- function() {paste0(sample(c(letters, ceiling(runif(10, 0, 9))), 10), collapse = '')}
  params <- c(
    'app_id' = app_id,
    'nonce_str' = nonce_run(),
    'sign' = '',
    'session' = '10000',
    'question' = URLencode(enc2utf8(keywords)),
    'time_stamp' = timestamp_num()
  )
  #print(params)
  url = 'https://api.ai.qq.com/fcgi-bin/nlp/nlp_textchat'
  request_body <- params_trans(params = params, app_key = app_key)
  # print(request_body)

  curl_opts <- list(
    # httpheader = myheader,
    postfields = request_body
  )

  webpage <- RCurl::postForm(url, .opts = curl_opts)
  result <- jsonlite::fromJSON(webpage)
  return(result$data$answer)
}
