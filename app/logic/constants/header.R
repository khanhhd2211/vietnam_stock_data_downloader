box::use(httr[add_headers])

#' @export
entrade_headers <- add_headers(
  authority = "services.entrade.com.vn",
  accept = "application/json, text/plain, */*",
  "accept-language" = "en-US,en;q=0.9",
  dnt = "1",
  "origin" = "https://banggia.dnse.com.vn",
  "referer" = "https://banggia.dnse.com.vn/",
  "sec-ch-ua" = "\"Edge\";v=\"114\", \"Chromium\";v=\"114\", \"Not=A?Brand\";v=\"24\"",
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = "\"Windows\"",
  "sec-fetch-dest" = "empty",
  "sec-fetch-mode" = "cors",
  "sec-fetch-site" = "cross-site",
  "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1788.0" # nolint
)

# API request config for SSI API endpoints

#' @export
ssi_headers <- add_headers(
  "Connection" = "keep-alive",
  "sec-ch-ua" = "\"Not A;Brand\";v=\"99\", \"Chromium\";v=\"98\", \"Google Chrome\";v=\"98\"",
  "DNT" = "1",
  "sec-ch-ua-mobile" = "?0",
  "X-Fiin-Key" = "KEY",
  "Content-Type" = "application/json",
  "Accept" = "application/json",
  "X-Fiin-User-ID" = "ID",
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/98.0.4758.102 Safari/537.36", # nolint
  "X-Fiin-Seed" = "SEED",
  "sec-ch-ua-platform" = "Windows",
  "Origin" = "https://iboard.ssi.com.vn",
  "Sec-Fetch-Site" = "same-site",
  "Sec-Fetch-Mode" = "cors",
  "Sec-Fetch-Dest" = "empty",
  "Referer" = "https://iboard.ssi.com.vn/",
  "Accept-Language" = "en-US,en;q=0.9,vi-VN;q=0.8,vi;q=0.7"
)