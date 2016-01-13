# -- -------------------------------------------------------------------------------------------- #
# -- Initial Developer: FranciscoME ------------------------------------------------------------- #
# -- GitHub Repossitory: https://github.com/IFFranciscoME/ROandaAPI ----------------------------- #
# -- License: GNU General Public License -------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- List of available instruments -------------------------------------------------------------- #

InstrumentsList <- function(AccountType,Token,AccountID){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/instruments?accountId=",sep="")
  QueryInst  <- paste(Queryhttp,AccountID,sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Actual Price Request ----------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

ActualPrice <- function(AccountType,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth         <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp    <- paste(httpaccount,"/v1/prices?instruments=",sep="")
  QueryPrec    <- paste(Queryhttp,Instrument,sep="")
  InstPrec     <- getURL(QueryPrec,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstPrecjson <- fromJSON(InstPrec, simplifyDataFrame = TRUE)
  DateTime     <- as.POSIXct(substr(InstPrecjson[[1]]$time,12,19),
                  format = "%H:%M:%S")
  Date <- as.character(substr(DateTime,1,10))
  Time <- as.character(substr(DateTime,12,19))
  DataJSON    <- data.frame(paste(Date,Time,sep=" "),InstPrecjson[[1]]$bid,InstPrecjson[[1]]$ask)
  colnames(DataJSON) <- c("TimeStamp","Bid","Ask")
  DataJSON$TimeStamp <- as.POSIXct(DataJSON$TimeStamp,origin="1970-01-01")
  return(DataJSON)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Historical Prices Request ------------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

HisPrices  <- function(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,Start,End){
  if(AccountType == "practice"){
    httpaccount  <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount  <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  #ifelse(Count = is.NULL, qcount  <- paste("count=",Count,sep=""), break)
  
  qstart <- paste("start=",Start,sep="")
  qend   <- paste("end=",End,sep="")
  
  qcandleFormat  <- "candleFormat=midpoint"
  qgranularity   <- paste("granularity=",Granularity,sep="")
  qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
  qalignmentTimezone <- paste("alignmentTimezone=",TimeAlign,sep="")
  
  auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
  QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
  QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
  QueryHistPrec2 <- paste(QueryHistPrec1,qstart,qend,qcandleFormat,qgranularity,
                    qdailyalignment,qalignmentTimezone,sep="&")
  InstHistP <- getURL(QueryHistPrec2,cainfo=system.file("CurlSSL","cacert.pem",
               package="RCurl"),httpheader=auth)
  InstHistPjson <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
  Prices        <- data.frame(InstHistPjson[[3]])
  Prices$time <- paste(substr(Prices$time,1,10),substr(Prices$time,12,19), sep=" ")
  colnames(Prices) <- c("TimeStamp","Open","High","Low","Close","TickVolume","Complete")
  Prices$TimeStamp <- as.POSIXct(Prices$TimeStamp,"%d/%m/%y %H:%M:%S",origin="1970-01-01")
  return(Prices)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Accounts per given username  --------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

UsersAccounts <- function(AccountType,Token,UserName){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth         <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/accounts?username=",sep="")
  QueryInst  <- paste(Queryhttp,UserName,sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Account Information  ----------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

AccountInfo   <- function(AccountType,AccountID,Token){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  QueryInfo   <- paste(Queryhttp,AccountID,sep="")
  CtaInfo     <- getURL(QueryInfo,cainfo=system.file("CurlSSL",
                 "cacert.pem",package="RCurl"),httpheader=auth)
  CtaInfoJson <- fromJSON(CtaInfo, simplifyDataFrame = TRUE)
  
  CtaNombre <- CtaInfoJson$accountName     # Nombre de la cta que se esta consultando
  CtaBalanc <- CtaInfoJson$balance         # Balance de la cta
  Ctaunreal <- CtaInfoJson$unrealizedPl    # Ganancia/Perdida sin tomar
  Ctareal   <- CtaInfoJson$realizedPl      # Ganancia/Perdida ya tomada
  Ctamgenut <- CtaInfoJson$marginUsed      # Margen utilizado
  Ctamgendi <- CtaInfoJson$marginAvail     # Margen disponible
  Ctamgenrt <- CtaInfoJson$marginRate      # Tasa de margen utilizada
  CtaOperac <- CtaInfoJson$openTrades      # Operaciones abiertas
  CtaOrdens <- CtaInfoJson$openOrders      # Ordenes abiertas
  datos     <- data.frame(CtaNombre,CtaBalanc,Ctaunreal,Ctareal,Ctamgenut,
               Ctamgendi,Ctamgenrt,CtaOperac,CtaOrdens)
  return(datos)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Actual order in the account ---------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

AccountOrders  <- function(AccountType,AccountID,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/orders?instrument=",sep="")
  Querythttp3  <- paste(Querythttp2,Instrument,sep="")
  Querythttp4  <- paste(Querythttp3,"&count=2",sep="")
  QueryInst1   <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Place a new order -------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

NewOrder <- function(AccountType,Token,Instrument,AccountID,Count,Side,OrderType){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/accounts/",sep="")
  Queryhttp1 <- paste(Queryhttp,AccountID,sep="")
  Queryhttp2 <- paste(Queryhttp1,"/orders",sep="")
  
  postForm(Queryhttp2,style="POST",.params=c(instrument=Instrument,units=Count,upperBound=slipUp,
  lowerBound=slipDown,takeProfit=takeProfit,stopLoss=stopLoss,side=Side,type=OrderType),
  .opts=list(httpheader=auth,ssl.verifypeer = FALSE))
}

# -- -------------------------------------------------------------------------------------------- #
# -- Information about a particular order ------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

OrderInfo  <- function(AccountType,AccountID,Token,OrderNum){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/orders/",sep="")
  Querythttp3  <- paste(Querythttp2,OrderNum,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- List of open trades ------------------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

OpenTrades  <- function(AccountType,AccountID,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/trades?instrument=",sep="")
  Querythttp3  <- paste(Querythttp2,Instrument,sep="")
  Querythttp4  <- paste(Querythttp3,"&count=100",sep="")
  QueryInst1   <- getURL(Querythttp4,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- A particular trade's Information  ---------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

TradeInfo  <- function(AccountType,AccountID,Token,TradeNumber){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/trades/",sep="")
  Querythttp3  <- paste(Querythttp2,TradeNumber,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Account's Open Positions List -------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

AccountPositions  <- function(AccountType,AccountID,Token){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/positions",sep="")
  QueryInst1   <- getURL(Querythttp2,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Position respect a particular instrument --------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

InstrumentPositions  <- function(AccountType,AccountID,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/positions/",sep="")
  Querythttp3  <- paste(Querythttp2,Instrument,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Historical of transactions ----------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

AccountHistTransactions  <- function(AccountType,AccountID,Token,Instrument,Count){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/transactions?instrument=",sep="")
  Querythttp3  <- paste(Querythttp2,Instrument,sep="")
  Querythttp4  <- paste(Querythttp3,"&count=",sep="")
  Querythttp5  <- paste(Querythttp4,Count,sep="")
  QueryInst1   <- getURL(Querythttp5,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- A particular transaction info  ------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

InfoTransaction  <- function(AccountType,AccountID,Token,TransactionNum){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/transactions/",sep="")
  Querythttp3  <- paste(Querythttp2,TransactionNum,sep="")
  QueryInst1   <- getURL(Querythttp3,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}

# -- -------------------------------------------------------------------------------------------- #
# -- General Info about all transactions of the account ----------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

AccountTransactions  <- function(AccountType,AccountID,Token){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth        <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp   <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1  <- paste(Queryhttp,AccountID,sep="")
  Querythttp2  <- paste(Querythttp1,"/alltransactions",sep="")
  QueryInst1   <- getURL(Querythttp2,cainfo=system.file("CurlSSL","cacert.pem",
                  package="RCurl"),httpheader=auth)
  # InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(QueryInst1)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Economic Calendar -------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

EconomicCalendar <- function(AccountType,Token,Instrument,Period){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/calendar?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")  
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")
  
  CalenH  <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
            "cacert.pem",package="RCurl"),httpheader=auth)
  Calend  <- fromJSON(CalenH, simplifyDataFrame = TRUE)
  Calend  <- subset(Calend, select = -c(currency,region,impact))
  Calend  <- Calend[complete.cases(Calend[,]),]
  Calend$timestamp <- as.POSIXct(Calend$timestamp,origin = "1970-01-01")
  return(Calend)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Historical posistion ratios in OANDA ------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

RatiosPosturas <- function(AccountType,Token,Instrument,Period){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/historical_position_ratios?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")                   
  
  ratios     <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
                "cacert.pem",package="RCurl"),httpheader=auth)
  ratios     <- data.frame(fromJSON(ratios))
  ratios[,2] <- as.POSIXct(ratios[,2],origin = "1970-01-01")
  return(ratios)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Current OANDA's Clients Spreads ------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

Spreads <- function(AccountType,Token,Instrument,Period){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/spreads?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")
  
  spread <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
            "cacert.pem",package="RCurl"),httpheader=auth)
  spread <- fromJSON(spread)
  return(spread)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Commitment Of Traders ---------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

COT <- function(AccountType,Token,Instrument){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/historical_position_ratios?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Cot        <- getURL(Queryhttp,cainfo=system.file("CurlSSL",
                "cacert.pem",package="RCurl"),httpheader=auth)
  return(Cot)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Order Book --------------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

OrderBook <- function(AccountType,Token,Instrument,Period){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ") 
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/orderbook_data?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")  
  
  orderbook  <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
                "cacert.pem",package="RCurl"),httpheader=auth)
  orderbook  <- fromJSON(orderbook)
  return(orderbook)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Autochartist "Our Favorites" Signals ------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

Autochartist <- function(AccountType,Token,Instrument,Period,Type){
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth  <- c(Authorization = paste("Bearer",Token,sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste(httpaccount,"/labs/v1/signal/autochartist?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")
  Queryhttp4 <- paste(Queryhttp3,"type=",sep="")
  Queryhttp5 <- paste(Queryhttp4,Type,sep="")
  
  Autochart  <- getURL(Queryhttp5,cainfo=system.file("CurlSSL",
                "cacert.pem",package="RCurl"),httpheader=auth)
  Autochart  <- fromJSON(Autochart)
  return(Autochart)
}

# -- PENDING Close an order --------------------------------------------------------------------- #

# -- PENDING Close a trade ---------------------------------------------------------------------- #

# -- PENDING Close existing position ------------------------------------------------------------ #

# -- PENDING Modify parameters of an order ------------------------------------------------------ #

# -- PENDING Modify parameters of a trade ------------------------------------------------------- #

# Function GetHistoryOanda
# Gets X candles of history
# Author - yokinfx
GetHistoryOanda <- function(Token, sim1, timeframe, velas) {
  library(RCurl)
  library(rjson)
  library(parsedate)
  detach_package(jsonlite)
  #token <- YOUR TOKEN
  token <- Token
  simbolo <- sim1; 
  granularity <- timeframe
  numVelas <- velas
  
  auth <- c(Authorization=paste("Bearer",token))
  query <- paste("https://api-fxpractice.oanda.com/v1/candles?instrument=",simbolo,"&count=",numVelas,
                 "&granularity=",granularity,sep="")
  
  getData <- getURL(query, httpheader = auth, .opts = list(ssl.verifypeer = FALSE))
  jsonData <- fromJSON(getData)
  
  x<-data.frame(time=strptime(jsonData$candles[[numVelas-1]]$time,format="%Y-%m-%dT%H:%M"),close=jsonData$candles[[numVelas-1]]$closeBid)
  # (1)
  #If you want seconds precission, comment previous line and uncomment following:
  #  x<-data.frame(time=strptime(jsonData$candles[[numVelas-1]]$time,format="%Y-%m-%dT%T"),close=jsonData$candles[[numVelas-1]]$closeBid)

  
  for (i in (numVelas-2):1)
  {
    newRow<-data.frame(time=strptime(jsonData$candles[[i]]$time,format="%Y-%m-%dT%H:%M"),close=jsonData$candles[[i]]$closeBid)
    # If you want seconds precission, read (1),  comment previous line, and uncomment the following:
    #newRow<-data.frame(time=strptime(jsonData$candles[[i]]$time,format="%Y-%m-%dT%T"),close=jsonData$candles[[i]]$closeBid)

    x<-rbind(newRow,x)
  }
  
  x
  #Lo pasamos a xts
  #s1 <- xts(x[,-1], order.by=x[,1])
}

# Function GetHistoryOanda2
# Gets history using from and to, in format "YYYY-MM-DD"
# Not limited to Oanda's limit of 5000 candles per request
# Author - yokinfx
GetHistoryOanda2 <- function(Token,sim1, timeframe, from, to) {
  #In the next line, use your own path if you want to store your own history in files
  nombreFichero<-paste("C:/Users/jpoudereux/Documents/R/hist/",sim1,".",timeframe,".",from,".",to,".rdata",sep="")
  if (file.exists(nombreFichero)) {
    load(nombreFichero)
    return(s1)
  }
  library(RCurl)
  library(rjson)
  library(parsedate)
  token <- Token
    simbolo <- sim1; 
  granularity <- timeframe
  numVelas<-1000
  
  diferencia<-difftime(strptime(to,format="%Y-%m-%d"), strptime(from,format="%Y-%m-%d"))
  stopifnot(diferencia > 0)
  
  lapse<-0
  if(timeframe=="M1")       lapse <- 3
  else if(timeframe=="M5")  lapse <- 15
  else if(timeframe=="M15") lapse <- 50
  else if(timeframe=="M30") lapse <- 100
  else if(timeframe=="H1")  lapse <- 200
  else if(timeframe=="H4")  lapse <- 800
  else if(timeframe=="D")   lapse <- 5000
  
  startDate<-paste(toString(from),"T00:00:00Z",sep="")
  endDate<-to
  if (diferencia > lapse) endDate<-as.Date(from)+lapse  
  endDate=paste(toString(endDate),"T23:59:59Z",sep="")
  
  x<-0
  Result<-c()
  diferencia <- difftime(strptime(endDate,format="%Y-%m-%dT%T"), strptime(startDate,format="%Y-%m-%dT%T"))
  cat(from," ",startDate," ",endDate," ",to,"\n")
  startDate = as.Date(startDate)
  endDate = as.Date(endDate)
  iter<-1
  
  while(!is.na(diferencia) & diferencia >= 0) {
    auth <- c(Authorization=paste("Bearer",token))
    query <- paste("https://api-fxpractice.oanda.com/v1/candles?instrument=",simbolo,"&start=", gsub("\\:","%3A",startDate),
                   "&end=",gsub("\\:","%3A",endDate),"&granularity=",granularity,sep="")      
    getData <- getURL(query, httpheader = auth, .opts = list(ssl.verifypeer = FALSE))
    jsonData <- fromJSON(getData)
    
    i<-2
    x=data.frame(time=strptime(jsonData$candles[[1]]$time,format="%Y-%m-%dT%T"),close=jsonData$candles[[1]]$closeBid)
    
    while(i<=length(jsonData$candles)) {
      newRow<-data.frame(time=strptime(jsonData$candles[[i]]$time,format="%Y-%m-%dT%T"),close=jsonData$candles[[i]]$closeBid)
      x<-rbind(newRow,x)
      i=i+1
    }
    startDate = endDate
    endDate   = as.Date(endDate,"%Y-%m-%d") + lapse
    diferencia = as.Date(endDate) - as.Date(startDate)
    cat("[",iter,"] from: ",as.Date(startDate)," to: ",as.Date(to)," , endDate: ",as.Date(endDate)," ,startDate: ",startDate,"\n")
    Result=rbind(x,Result)
    iter = iter+1
    if (as.Date(endDate) > as.Date(to)) break
  }
  #print(x)
  #Lo pasamos a xts
  s1 <- xts(Result[,-1], order.by=Result[,1])
  save(s1,file=nombreFichero)
  s1
}
