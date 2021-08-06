temp <- tempfile()

download.file("https://outbreak.info/location-reports?loc=USA_US-MN", temp)

read_json(temp)

haha <- jsonlite::fromJSON("~/Downloads/outbreakinfo_resources_metadata_2021-08-05.json")
