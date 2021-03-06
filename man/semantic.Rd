% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semantic.R
\name{semantic}
\alias{semantic}
\title{R interface to tencent AI lab's semantic analysis}
\usage{
semantic(text, app_key, app_id)
}
\arguments{
\item{text}{The text is sent to API}

\item{app_key}{Your app key}

\item{app_id}{Your app ID}
}
\value{
the meaning of return code shown in this page <\url{https://ai.qq.com/doc/nlpsem.shtml}>
}
\description{
text can only be Chinese, this function performs poorly in English
}
\details{
\code{semantic} returns the result of semantic analysis
}
\examples{
 semantic('今天和明天的天气怎样', app_id = '1107152791', app_key =  'OpsMj8HXPmbu9SMd')
}
\author{
Ao Sun <\url{https://ao-sun.github.io/}>
}
