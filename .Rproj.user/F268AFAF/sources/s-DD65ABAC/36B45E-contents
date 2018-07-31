#get API authentication
get_sign <- function(params, app_key){
  params_run <- params[nchar(params) > 0]
  params_run[order(names(params_run))]
  params_str <- paste0(names(params_run), '=', params_run, '&', collapse = '')
  str <- paste0(params_str, 'app_key=', app_key)
  sign_str <- toupper(digest::digest(str,algo="md5", serialize=FALSE))
  return(sign_str)
}
