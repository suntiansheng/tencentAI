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


#' R interface to tencent AI text translation
#'
#'
#' \code{translate} returns the result of transform
#' @param text The text is sent to API
#' @param app_key Your app key
#' @param app_id Your app ID
#' @description  This function can only handle English and Chinese
#' @author Ao Sun <\url{https://ao-sun.github.io/}>
#' @examples  translate('god', app_id = '1107152791', app_key =  'OpsMj8HXPmbu9SMd')
#' @export
translate <- function(app_id = app_id, app_key = app_key, keywords) {
  timestamp_num <- function() {ceiling(as.numeric(as.POSIXct(Sys.time(), format="%Y-%m-%d %H:%M:%S")))}
  nonce_run <- function() {paste0(sample(c(letters, ceiling(runif(10, 0, 9))), 10), collapse = '')}
  params <- c(
    'app_id' = app_id,
    'nonce_str' = nonce_run(),
    'sign' = '',
    'type' = 0,
    'text' = URLencode(enc2utf8(keywords)),
    'time_stamp' = timestamp_num()
  )
  #print(params)
  url = 'https://api.ai.qq.com/fcgi-bin/nlp/nlp_texttrans'
  request_body <- params_trans(params = params, app_key = app_key)
  # print(request_body)

  curl_opts <- list(
    # httpheader = myheader,
    postfields = request_body
  )
  webpage <- RCurl::postForm(url, .opts = curl_opts)
  result <- jsonlite::fromJSON(webpage)
  return(result$data$trans_text)
}
