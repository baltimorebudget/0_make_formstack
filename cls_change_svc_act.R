.libPaths("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Documents/r_library")
library(rio)
library(tidyverse)
library(magrittr)
library(openxlsx)
library(readxl)
library(httr)

devtools::load_all("G:/Analyst Folders/Sara Brumfield/bbmR")

##get updated lists of agencies and analysts ===============
analysts <- import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx") %>%
  filter(Operational == TRUE) %>%
  select(`Agency ID`, `Agency Name`, `Agency Name - Cleaned`)  %>%
  mutate(`Agency ID` = as.character(`Agency ID`)) 

agencies <- analysts %>%
  select(`Agency Name`) %>%
  unique() %>%
  arrange(`Agency Name`) %>%
  filter(!is.na(`Agency Name`))

agencies %>%
  export_excel("FY23 Agency", "outputs/List of FY24 Agencies.xlsx")


##get updated services ================
planning <- read_xlsx("G:/Fiscal Years/Fiscal 2024/Planning Year/1. CLS/1. Line Item Reports/line_items_2022-09-16_CLS_After_186.xlsx",
                      sheet = "Details")

svc <- planning %>%
  mutate(`Service` = paste0(`Program ID`, "-", `Program Name`)) %>%
  select(`Agency Name`, `Service`) %>%
  unique() %>%
  arrange(`Agency Name`, `Service`) %>%
  filter(!is.na(`Agency Name`))

export_excel(svc, "FY24 Services", "FY24 Service List.xlsx")

act <- planning %>%
  mutate(`Activity` = paste0(`Activity ID`, "-", `Activity Name`),
         `Service` = paste0(`Program ID`, "-", `Program Name`)) %>%
  select(`Agency Name`, `Service`, `Activity`) %>%
  unique() %>%
  arrange(`Agency Name`, `Service`, `Activity`) %>%
  filter(!is.na(`Agency Name`))

##create drop-down values ================
expend <- read_xlsx("G:/Fiscal Years/Fiscal 2022/Projections Year/2. Monthly Expenditure Data/Month 12_June Projections/Expenditure 2022-06_Run7.xlsx") %>%
  distinct(`Agency Name`, `Program ID`, `Program Name`,
           `Activity ID`, `Activity Name`) %>%
  mutate(`Service` = paste0(`Program ID`, "-", `Program Name`),
         `Activity ID` = str_pad(`Activity ID`, width = 2, "left", "0"),
         `Activity` = paste0(`Activity ID`, "-", `Activity Name`)) %>%
  select(`Agency Name`, `Service`, `Activity`) %>%
  distinct()

##need to filter out non-operational agencies
data <- right_join(expend, agencies) %>% filter(!is.na(Service))

agencies2 <- unique(data$`Agency Name`)

services <- unique(data$Service)

##push drop-down values to Formstack via API ============
api <- bbmR::connect_fs_api()
form_id = 5008440
field_id = 132361438
key = "7cf3e390462e4e0882dcc5df52a73d69"
url <- paste0("https://www.formstack.com/api/v2/form/", form_id, "/field.json")

api2 <- GET(paste0("https://www.formstack.com/api/v2/form.json?oauth_token=", key))
http_status(api2)
headers(api2)
content(api2, "text")


for (a in agencies2) {
  df <- data %>% 
    filter(`Agency Name` == a) 
  
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
                   label = paste(a, ": Service to remove, rename or convert"),
                   options = as.list(c(NA, sort(unique(df$`Service`)))),
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

field_id2 = 132366979
for (s in services) {
  df <- data %>% 
    filter(`Service` == s) 
  
  POST(url,
       add_headers(Authorization = paste("Bearer", #Sys.getenv("FS_API_KEY"), sep = " ")),
                                         key, sep = " ")),
       content_type("application/json"),
       accept("application/json"),
       body = list(id = form_id,
                   field_type = "select",
                   label = paste(s, ": Activity to remove, rename or convert"),
                   options = as.list(c(NA, sort(unique(df$`Activity`)))),
                   required = 0,
                   logic = list(
                     action = "show",
                     conditional = "any",
                     checks = list(list(
                       condition = "equals",
                       field = field_id2,
                       option = s))
                   )
       ),
       encode = "json")
}

##roll-up results

flat_adj <- import("inputs/FlatAdjustmentFY23.xlsx") %>%
  unite("Service-Activity", contains("Service-Activity")) %>%
  mutate(`Service-Activity` = gsub("NA_|_NA", "", `Service-Activity`)) %>%
  select(-(Browser:Location)) %>%
  separate(`Service-Activity`, c("SA ID", "SA Name"), sep = ": ") %>%
  separate(`SA ID`, c("Service ID", "Activity ID"), sep = "-") %>%
  separate(`SA Name`, c("Service Name", "Activity Name"), sep = "-", extra = "merge") %>%
  separate(Fund, c("Fund ID", "Fund Name"), sep = " | ", extra = "merge") %>%
  relocate(`Service Name`, .after = "Service ID") %>%
  rename(`Agency ID` = Agency) %>%
  left_join(analysts) %>%
  relocate(`Agency Name`, .after = "Agency ID") %>%
  mutate(`Analyst Rec (Yes/No)` = NA,
         `Analyst Partial Rec (Amount)` = NA,
         `Analyst Comments` = NA) %>%
  relocate(`Analyst Rec (Yes/No)`:`Analyst Comments`) %>%
  # in FY23, Fire was asked to resubmit because their first round of submissions was invalid.
  # this filters out the first round of Fire submissions.
  filter(!(`Agency Name` == "Fire" & Time < lubridate::ymd("2021-09-22")))

export_excel(flat_adj, "FY23", "outputs/FY23 Flat Adjustments.xlsx" ,"new")

# split by agency
x <- unique(flat_adj$`Agency Name`)

map("M-R: Office of Information and Technology", function(x) {
  
  agencies <- list(info = analysts %>%
                     filter(`Agency Name` == x[1]))
  
  agencies$agency <- agencies$info %>%
    extract2("Agency Name - Cleaned")
  
  agencies$output_file <- paste0("G:/Agencies/", agencies$agency, "/File Distribution/FY23 Flat Adj - ", agencies$agency, ".xlsx") 
  
  agencies$df <- flat_adj %>%
    filter(`Agency Name` == x) %>%
    arrange(`Fund ID`, `Service ID`, `Activity ID`) %>%
    select(-`Agency Name - Cleaned`)
  
  style <- list(cell.bg = createStyle(fgFill = "lightcyan", border = "TopBottomLeftRight",
                                      borderColour = "black", textDecoration = "bold"))
  
  excel <- export_excel(agencies$df, "Flat Adjustments", agencies$output_file, "new",
                        col_width = c(rep(15, 17), 40, 15, 40), save = FALSE)
  
  addStyle(excel, 1, style$cell.bg, rows = 1,
           gridExpand = TRUE, stack = FALSE,
           cols = 1:3)
  
  saveWorkbook(excel, agencies$output_file, overwrite = TRUE)
  
})