
library(dplyr)
library(stringr)
library(vwr)
library(rJava)
library(RJDBC)





#'#####################################################################################
#' intitialise settings
# Sys.setenv(JAVA_HOME='/usr/lib/jvm/java-8-openjdk-amd64')
# options(java.parameters="-Xmx2g")
# 
# .jinit()
# 
# jdbcDriver <- JDBC(driverClass = "oracle.jdbc.OracleDriver",
#                    classPath = "/usr/lib/oracle/12.1/client64/lib/ojdbc6.jar")
# 
# jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//ukkinORA03x.wkuk.net:1521/WKUKDW",
#                             "REPO", "INTERVENTION5")

#'#####################################################################################

#' current customers query
live_cust_query <- "WITH LIVE_CUST_DETAILS AS (
    SELECT 
CT.SOLDTOID__C AS CONTRACT_SOLDTOID, 
A.ID AS ACCOUNT_SFDC_ID, A.SAP_CLIENT_NUMBER__C AS ACCOUNT_SAP_ID, 
(A.TOTALCCHINFORMATIONVALUE + A.TOTALCRONERINFORMATIONVALUE) AS ACCOUNT_INFO_VALUE,
soldto.CCH_BUSINESS_TYPE, soldto.BUSINESS_TYPE, A.NUMBEROFEMPLOYEES, A.ANNUALREVENUE, A.INDUSTRY,
REPLACE(soldto.GUO_NAME,',',';') AS SOLDTO_GUO_NAME,
REPLACE(soldto.ADDRESS,',',';') AS SOLDTO_ADDRESS, 
REPLACE(soldto.TOWN,',',';') AS SOLDTO_TOWN, 
REPLACE(soldto.POST_CODE,',',';') AS SOLDTO_POSTCODE,
soldto.EMAIL AS SOLDTO_EMAIL, soldto.TELEPHONE SOLDTO_PHONE,
REPLACE(shipto.FIRST_NAME,',',';') AS SHIPTO_FIRST_NAME,
REPLACE(shipto.LAST_NAME,',',';') AS SHIPTO_LAST_NAME,
REPLACE(shipto.ADDRESS,',',';') AS SHIPTO_ADDRESS, 
REPLACE(shipto.TOWN,',',';') AS SHIPTO_TOWN, 
REPLACE(shipto.POST_CODE,',',';') AS SHIPTO_POSTCODE,
shipto.EMAIL AS SHIPTO_EMAIL, shipto.TELEPHONE SHIPTO_PHONE,
A.TOTALCCHINFORMATIONVALUE AS CCH_INFO_VALUE,
A.TOTALCRONERINFORMATIONVALUE AS CRONER_INFO_VALUE
FROM ODS.SFDC_CONTRACT CT, ODS.SFDC_ACCOUNT A,
BUSINESS_ANALYSIS.CUSTOMER_D SOLDTO, 
BUSINESS_ANALYSIS.CUSTOMER_D SHIPTO
WHERE UPPER(CT.CATEGORY__C) = 'INFORMATION'
AND CT.ACCOUNTID__C = A.ID
AND CT.ENDDATE__C > SYSDATE
/*AND UPPER(CT.STATUS__C) = 'ACTIVE'
AND (A.TOTALCCHINFORMATIONVALUE + A.TOTALCRONERINFORMATIONVALUE) > 0*/
AND SOLDTO.BP_ID = CT.SOLDTOID__C
AND SHIPTO.BP_ID = CT.SHIPTOID__C
)
SELECT DISTINCT SOLDTO_GUO_NAME, SOLDTO_POSTCODE
FROM LIVE_CUST_DETAILS LCD"


live_cust_query <- gsub("\n|\t"," ",live_cust_query)



name.website.df <- NULL

#' set maximum size to 30 MB
options(shiny.maxRequestSize=30*1024^2)

options(stringsAsFactors = FALSE)

na.patterns <- "#REF!|#N/A|\\(blank\\)"

special.chars <- c("\\.",
                   ";",
                   "\\(.+\\)")

company.name.stop.words <- c("\\blimited\\b",
                             "\\bltd\\b",
                             "\\bllc\\b",
                             "\\bplc\\b",
                             "\\bllc\\b",
                             "\\bllp\\b",
                             "\\bUK\\b",
                             "\\bLONDON\\b",
                             "\\bcounty council\\b",
                             "\\bcouncil\\b",
                             "\\bcompany\\b",
                             "& Co",
                             "& son",
                             "& sons",
                             "\\bpartners\\b",
                             "\\bpartner\\b",
                             "\\bthe\\b",
                             "\\bgroup\\b",
                             "\\bholdings\\b",
                             "\\bholding\\b",
                             "\\bhousing\\b",
                             "\\btrading\\b",
                             "\\bschool\\b",
                             "\\bcollege\\b",
                             "\\bhospital\\b",
                             "\\bmedical\\b",
                             "\\bpublic\\b",
                             "\\bservices\\b",
                             "\\bservice\\b",
                             "\\bsystems\\b",
                             "\\bproducts\\b",
                             "\\bassociation\\b",
                             "\\bassociates\\b",
                             "\\bhotel\\b",
                             "\\bhotels\\b",
                             "\\bproductions\\b",
                             "\\bfood\\b",
                             "\\bfoods\\b",
                             "\\bhome\\b",
                             "\\bnursing\\b",
                             "\\bconsultants\\b",
                             "\\bconsultancy\\b",
                             "\\bchartered\\b",
                             "\\binternational\\b",
                             "\\binvestment\\b",
                             "\\binvestments\\b",
                             "\\btransport\\b",
                             "\\baudit\\b",
                             "\\baccounting\\b",
                             "\\baccountancy\\b",
                             "\\baccountants\\b",
                             "\\bproperty\\b",
                             "\\bestate\\b",
                             "\\btechnology\\b",
                             "\\btravel\\b",
                             "\\bdistribution\\b",
                             "\\bmanagement\\b",
                             "\\bleisure\\b",
                             "\\bpayroll\\b",
                             "\\bscotland\\b",
                             "\\bengineering\\b",
                             "\\bpartnerships\\b",
                             "\\bpartnership\\b",
                             "\\bindustries\\b",
                             "\\bcommunity\\b",
                             "\\bbusiness\\b",
                             "\\bconstruction\\b",
                             "\\btrusts\\b",
                             "\\btrust\\b",
                             "\\bbank\\b")

url.prefix <- c("http://",
                  "https://",
                  "www.uk.",
                  "www.")

url.prefix2 <- c("\\buk.")

generic.domains <- c("gmail","hotmail","yahoo","googlemail","aol","btinternet","ymail","icloud")


computeShortName <- function(df, acNameCol){
  
  #df$Account.Name <- stringr::str_replace_all(df$Account.Name,"\\s+", " ")
  
  df[[acNameCol]] <- stringr::str_replace_all(df[[acNameCol]],"\\s+", " ")
  
  special.char.pattern <- paste(special.chars,collapse = "|")
  
  df$Account.Name.Short <- unlist(lapply(df[[acNameCol]], function(x) gsub(x,pattern = special.char.pattern, replacement = "")))
  
  stop.words.pattern <- paste(company.name.stop.words, collapse = "|")
  
  df$Account.Name.Short <- unlist(lapply(df$Account.Name.Short, function(x) gsub(x,pattern = stop.words.pattern, 
                                                                                 replacement = "", ignore.case = TRUE)))
  
  two.words <- stringr::str_extract_all(df$Account.Name.Short, "[[:alnum:]][[:alnum:]]+\\s+[[:alnum:]][[:alnum:]]+")
  two.words <- unlist(lapply(two.words, function(x) ifelse(length(x) < 1 ,NA,x[[1]])))
  
  df$Account.two.words <- two.words
  df$Account.two.words[is.na(df$Account.two.words)] <- df$Account.Name.Short[is.na(df$Account.two.words)]
  
  #' Keep sole person records as is
  sole.person.index <- grep("(MR|MRS|MISS|MS).+",df$Account.two.words, ignore.case = TRUE)
  df$Account.two.words[sole.person.index] <- df$Account.Name.Short[sole.person.index]
  
  #' Keep University records as is
  univ.index <- index <- grep("UNIVERSITY OF",df$Account.two.words, ignore.case = TRUE)
  df$Account.two.words[univ.index] <- df$Account.Name.Short[univ.index]
  
  df$Account.Name.Short <- df$Account.two.words
  df$Account.Name.Short <- str_trim(df$Account.two.words)
  
  df$Account.two.words <- NULL
  
  return(df)
}

matchAccountNames <- function(df1,df2){
    
  #' convert both vectors to uppercase
  df1$Account.Name.Short <- stringi::stri_trim(toupper(df1$Account.Name.Short))
  df2$Account.Name.Short <- stringi::stri_trim(toupper(df2$Account.Name.Short))
  
  targets <- unique(df2$Account.Name.Short)
  
  df1$Levenshtein.Match <- ""
  df1$Levenshtein.Dist <- Inf
  df1$Levenshtein.Dist.Norm <- 0
  
  # df1$Hamming.Match <- ""
  # df1$Hamming.Dist <- Inf
  
  df1$Levenshtein.Damerau.Match <- ""
  df1$Levenshtein.Damerau.Dist <- Inf
  df1$Levenshtein.Damerau.Norm <- 0
  
  for(i in 1:nrow(df1)){
    #' keep only targets that have first 3 characters common with the string to match
    sub.targets <- targets[substr(df1$Account.Name.Short[i],1,3) == substr(targets,1,3)]
    
    if(length(sub.targets) < 1){
      
      #df1$Levenshtein.Match[i] <- df1$Hamming.Match[i] <- df1$Levenshtein.Damerau.Match[i] <- "NO MATCH"
      df1$Levenshtein.Match[i] <- df1$Levenshtein.Damerau.Match[i] <- "NO MATCH"
      
    }else{
      
      #'check if exact match exists in sub.targets
      exact_matches <- sub.targets[grep(df1$Account.Name.Short[i],sub.targets,ignore.case = TRUE)]
      if(length(exact_matches) > 0){
        
        exact_match <- exact_matches[1]
        
        df1$Levenshtein.Match[i] <- exact_match
        df1$Levenshtein.Dist[i] <- 0
        
        df1$Levenshtein.Damerau.Match[i] <- exact_match
        df1$Levenshtein.Damerau.Dist[i] <- 0
      }
      #' exact match not found search for closest matches
      else{
        
        #' get Levenshtein matches
        lev.match <- unlist(vwr::levenshtein.neighbors(df1$Account.Name.Short[i], sub.targets))[1]
        
        if(length(lev.match) < 1) {
          df1$Levenshtein.Match[i] <- "NO MATCH"
        }else{
          df1$Levenshtein.Match[i] <- lev.match
          df1$Levenshtein.Dist[i] <- vwr::levenshtein.distance(df1$Account.Name.Short[i], df1$Levenshtein.Match[i])
        }
        
        #' get Hamming matches
        # ham.match <- unlist(vwr::hamming.neighbors(df1$Account.Name.Short[i], sub.targets))[1]
        # if(length(ham.match) < 1) {
        #   df1$Hamming.Match[i] <- "NO MATCH"
        # }else{
        #   df1$Hamming.Match[i] <- ham.match
        #   df1$Hamming.Dist[i] <- vwr::hamming.distance(df1$Account.Name.Short[i], df1$Hamming.Match[i])
        # }
        
        #' get Levenshtein Damerau matches
        # lev.dam.match <- unlist(vwr::levenshtein.damerau.neighbors(df1$Account.Name.Short[i], sub.targets))[1]
        # if(length(lev.dam.match) < 1){
        #   df1$Levenshtein.Damerau.Match[i] <- "NO MATCH"
        # }else {
        #   
        #   df1$Levenshtein.Damerau.Match[i] <- lev.dam.match
        #   df1$Levenshtein.Damerau.Dist[i] <- vwr::levenshtein.damerau.distance(df1$Account.Name.Short[i], df1$Levenshtein.Damerau.Match[i])
        #   
        # }
        
      }
      
      
    }
    
    
  }
  
  #' normalize lev distance by string length
  df1$Levenshtein.Dist.Norm <- df1$Levenshtein.Dist / nchar(df1$Account.Name.Short)
  
  return(df1)
}