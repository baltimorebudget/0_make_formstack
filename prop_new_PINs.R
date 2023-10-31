.libPaths("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Documents/r_library")
library(rio)
library(tidyverse)
library(magrittr)
library(openxlsx)
library(readxl)
library(httr)

devtools::load_all("G:/Analyst Folders/Sara Brumfield/_packages/bbmR")

##workday cost center hierarchy from Extract Cost Centers report
cc_hier <- read_xlsx("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/_Code/_ref/Cost Center Hierarchy.xlsx") %>%
  select(`Reference ID`, `Cost Center`, Name, Agency, Service, Inactive, `Capital Project`) %>%
  filter(`Capital Project` == "No" & is.na(Inactive))

cc_hier <- cc_hier %>%
  group_by(Agency, Service, `Cost Center`) %>%
  summarise(n()) %>%
  ungroup() %>%
  select(-`n()`)

agencies <- unique(cc_hier$`Agency`)

services <- unique(cc_hier$Service)

cost_centers <- unique(cc_hier$`Cost Center`)

export_excel(data.frame(agencies), "Agencies", "Agencies.xlsx")

export_excel(data.frame(services), "Services", "Services.xlsx")


##push drop-down values to Formstack via API ============
api <- bbmR::connect_fs_api()
form_id = 5508140
field_id = 153957687
key = "7cf3e390462e4e0882dcc5df52a73d69"
url <- paste0("https://www.formstack.com/api/v2/form/", form_id, "/field.json")

api2 <- GET(paste0("https://www.formstack.com/api/v2/form.json?oauth_token=", key))
http_status(api2)
headers(api2)
# content(api2, "text")


for (a in agencies) {
  df <- cc_hier %>% 
    filter(`Agency` == a) 
  
  # response <- VERB("POST", url, body = list(id = form_id,
  #                                           field_type = "select",
  #                                           label = paste(a, ": Service-Activity"),
  #                                           options = as.list(c(NA, sort(unique(df$`Service-Activity`)))),
  #                                           required = 1,
  #                                           logic = list(
  #                                             action = "show",
  #                                             conditional = "all",
  #                                             checks = list(list(
  #                                               condition = "equals",
  #                                               field = field_id,
  #                                               option = a)))),
  #                  content_type("application/json"), accept("application/json"), encode = "json")
  # 
  # print(content(response, "text"))
  
  POST(url,
       add_headers(Authorization = paste("Bearer", #Sys.getenv("FS_API_KEY"), sep = " ")),
                                         key, sep = " ")),
       content_type("application/json"),
       accept("application/json"),
       body = list(id = form_id,
                   field_type = "select",
                   label = paste(a, ": Cost Center"),
                   options = as.list(c(NA, sort(unique(df$`Cost Center`)))),
                   required = 0,
                   logic = list(
                     action = "show",
                     conditional = "all",
                     checks = list(list(
                       condition = "equals",
                       field = field_id,
                       option = a))
                   )
       ),
       encode = "json")
}
