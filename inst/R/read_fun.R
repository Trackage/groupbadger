read_fun <- function() {
  library(dplyr)
  load("data/EAnt_SES.RData")
  d <- head(ses_tracks, 100)
  return(d)
  d <- ses_tracks #`[sort(sample(nrow(ses_tracks), 10000)), ]
  d <- d[order(d$ref, d$gmt), ]
  d <- d %>% distinct(ref, gmt, .keep_all = TRUE)
  d
}