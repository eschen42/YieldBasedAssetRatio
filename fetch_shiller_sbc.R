local_file <- "ie_data.xls"
if (!file.exists(local_file))
  tryCatch(
    download.file(
      url = "https://web.archive.org/web/20250402184825/https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/34a1781b-f073-448f-b9f9-6230becb2e49/ie_data.xls",
      #url = "https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/34a1781b-f073-448f-b9f9-6230becb2e49/ie_data.xls",
      #url = "https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/ie_data.xls",
      destfile = local_file,
      method = "libcurl"
      ),
    error = function(e) { }
  )

if (!file.exists(local_file))
  tryCatch(
    download.file(
      url = "https://web.archive.org/web/20250402184825/https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/34a1781b-f073-448f-b9f9-6230becb2e49/ie_data.xls",
      #url = "https://web.archive.org/web/20240419164520/https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/ie_data.xls",
      destfile = local_file,
      method = "libcurl"
      ),
    error = function(e) { }
  )

if (!file.exists(local_file)) {
  cat(
    "fetch from shillerdata.com failed; falling back to yale.edu\n",
    file = stderr()
  )
}

if (!file.exists(local_file))
  tryCatch(
    download.file(
      url = "https://web.archive.org/web/20231223160615/http://www.econ.yale.edu/~shiller/data/ie_data.xls",
      destfile = local_file,
      method = "libcurl"
      ),
    error = function(e) { }
  )


if (!file.exists(local_file))
  tryCatch(
    download.file(
      url = "http://www.econ.yale.edu/~shiller/data/ie_data.xls",
      destfile = local_file,
      method = "libcurl"
      ),
    error = function(e) { }
  )

if (!file.exists(local_file))
  stop("Error: Failed to download Robert Shiller's ie_data.xls")
