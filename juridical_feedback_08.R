library(tidyr)
library(dplyr)
library(rvest)
library(xml2)
library(data.table)
library(stringi)
library(RCurl)
library(stringr)
library(xlsx)

##### Негативні висновки Головного юридичного управління (ГЮУ) ####

##### Детальний опис законодавчої активності ####
bills_main_skl8 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl8/bills_main-skl8.csv", 
                            fileEncoding = "UTF-8")

##### Головні виконавці законів ####
bills_executives <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl8/bills_executives-skl8.csv", 
                             fileEncoding = "UTF-8" ) %>%
  filter(type=="mainExecutive") %>% 
  select(bill_id, department)

##### Чинні закони і інформація про них з виконавцями ####
bills_acts08 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl8/bills_acts-skl8.csv", 
                         fileEncoding = "UTF-8" )%>%
  left_join(bills_main_skl8, by=c("bill_id"="bill_id"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))

##### Законодавчі ініціативи, які мають зауваження ГЮУ ####

bills_documents_gyur <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl8/bills_documents-skl8.csv", 
                                 fileEncoding = "UTF-8" )%>%
  filter(document_type=="Зауваження Головного юридичного управління")%>%
  group_by(bill_id) %>% 
  mutate(order = seq_along(bill_id))

##### Відокремлення  ####
bi_gyur <- bills_documents_gyur%>%
  mutate(n_id = stri_extract_last_regex(document_url, "\\d{6}")) # Відокремлюємо останні шість символів в посиланні

# Поєднання зауважень із законодавчою активністю
bi_gyur_z <- bi_gyur%>%
  #left_join(bills_acts08, by=c("bill_id"="bill_id"))%>% # це якщо чинні акти лише треба
  #filter(!is.na(act_number))%>%
  left_join(bills_main_skl8, by=c("bill_id"="bill_id"))%>%
  filter(!is.na(n_id))

##### Скачування висновків  ####  

# Робимо список з необхідних айдішників
bi_gyur_list <- as.list(bi_gyur_z$n_id)

# Завантажуємо дані

get_data <- function(){
  
  for(i in bi_gyur_list) {
    
    url <- paste0("https://w1.c1.rada.gov.ua/pls/zweb2/webproc77_1?pf35401=", i)
    
    page <- read_html(url)
    
    if(url.exists(url) == TRUE){
      
      name <- page%>%
        html_nodes("p")%>% 
        html_text()%>%
        data.frame()
      
      df <- data.frame(name, url)
    }
    
    message(paste( 'завантажено'))
    
    write.table(df, paste0("concl_jurid_08", "_12_02_2020", "_full.csv"), # Change date
                append = TRUE, col.names = FALSE, row.names = FALSE, sep = ';')
    
    Sys.sleep(0.8)
    
  }
}

##### Функція для активації скачування   ####  
results_juridicials <- get_data()

##### Завантаження скачаних висновків  ####  
library(readr)

# Відкрити скачаний файл
conclusion_read <- read_delim("concl_jurid_08_12_02_2020_full.csv", # Змінюємо назву відповідно до 70 рядка
                              ";", escape_double = FALSE, col_names = FALSE, 
                              locale = locale(encoding = "Windows-1251"), trim_ws = TRUE)

# Вилучитит в колонці n_id_extracted останні шість символів 
conclusion_fixed_id <- conclusion_read %>%
  mutate(n_id_extracted = stri_extract_last_regex(X2, "\\d{6}"))

# Приєднати до новоствореної колонки з 6-ма символами датафрейм bi_gyur_z (зауваження із законодавчою активністю)
conclusion_fixed <- conclusion_fixed_id %>%
  left_join(bi_gyur_z, by=c("n_id_extracted"="n_id"))%>%
  separate(X1, c("text", "status_conc"), sep = ":")%>%
  mutate(status=str_trim(status_conc))

# Розширюємо файл
conclusion_extended <- conclusion_fixed %>%
  left_join(bills_main_skl8, by=c("bill_id"="bill_id"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))

# Записуємо файл розширених висновців 
write.xlsx(as.data.frame(conclusion_extended),  
           file=paste0("висновки_ГЮУ_08R_", "06_02_2020", ".xlsx"), #Change date
           sheetName="conclusion_extended", row.names=FALSE, append = FALSE)
