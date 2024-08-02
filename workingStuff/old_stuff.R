

### ======================    F U N C T I O N S   =============================

STDATE = format(Sys.Date() - 90,format = "%Y-%m-%d") # date a 3 month ago

apiUpdateTokens = function() {
  # == 1 == Read data
  consumerKey = readRDS('./data/ck.rds')
  authCode = readRDS('./data/ac.rds')
  refreshToken = readRDS('./data/t90.rds')
  rtime = refreshToken$refreshExpire

  # == 2 == Reset Refresh Token
  # The Refresh Token should be reset before it expires after 90 days.
  # TD indicates they do look for frequent Refresh Token generation.
  # This should be used conservatively!!!
  ctime = Sys.time()
  if (difftime(ctime,rtime,units = 'days') > 90) {
    # Callback URL is not required
    refreshToken = rameritrade::td_auth_refreshToken(consumerKey, codeToken = refreshToken)
    # Save the Refresh Token to a safe location so it can be retrieved as needed.
    # It will be valid for 90 days.
    rtime = Sys.time()
    saveRDS(refreshToken,'./data/t90.rds')
    saveRDS(rtime,'./data/t90date.rds')
    print(paste0("The Refresh Token is updated at ",rtime,"."))
    print(paste0("The Refresh Token is valid till ",refreshToken$refreshExpire,"."))
  } else {
    print(paste0("The Refresh Token is valid till ",refreshToken$refreshExpire,"."))
  } # endif

  # == 3 == Check/Update the Access Token
  accessToken = readRDS('./data/at.rds')
  if (accessToken$expireTime < Sys.time()) {
    # accessToken is expired => get a new one
    accessToken = rameritrade::td_auth_accessToken(consumerKey, refreshToken)
    saveRDS(accessToken,'./data/at.rds')
    print("A new Access Token is created.")
    print(paste0("The Access Token is valid till ",accessToken$expireTime,"."))
  } else {
    print(paste0("The Access Token is valid till ",accessToken$expireTime ,"."))
  } #endif
} # end apiUpdateTokens

#' Get all transactions for accounts from sDate to the current date
#'
#' Gets transactions for specified accounts
#' @param acIds The vector of accounts' Ids (numbers)
#' @param sDate The start date (as a string "yyy-mm-dd")
#' @return The data frame with the following columns
#'         "accountId","type","description", "date","symbol","underSymbol",
#'         "cusip","assetType","expirationDate","amount","price","cost",
#'         "netAmount","instruction","positionEffect","fees"
#' @examples
#' acIds = c(111222333,444555666)
#' STDATE = format(Sys.Date() - 90,format = "%Y-%m-%d")
#' TRANS =  apiGetTransactions = function(acIds,STDATE)
#' @export
apiGetTransactions = function(acIds,sDate) {
  TRANS = NULL
  colNamesList = c("accountId","type","description", "date",
                   "symbol","underSymbol","cusip","assetType","expirationDate",
                   "amount","price","cost","netAmount","instruction","positionEffect","fees")
  # print(paste("Accounts: ",paste(acIds,collapse = " | ")))
  for (i in 1: length(acIds)) {
    acId = acIds[i]
    # print(paste0("Account: ",acId))
    trans = td_transactSearch(accountNumber = acId,startDate = sDate,endDate = Sys.Date())
    trans = as.data.frame(trans)
    # print(paste0("Number of transactions: ",nrow(trans)))
    if (nrow(trans) > 0) {
      F1 = as.data.frame(trans$transactionItem)
      F2 = as.data.frame(F1$instrument)
      Fees = rowSums(trans$fees)
      FFF = cbind(F1$accountId,trans$type,trans$description,trans$transactionDate,
                  F2$symbol,F2$underlyingSymbol,F2$cusip,F2$assetType,F2$optionExpirationDate,
                  F1$amount,F1$price,F1$cost,trans$netAmount,F1$instruction,F1$positionEffect,Fees)
      if (ncol(FFF) == 16) {
        colnames(FFF) = colNamesList
        TRANS = rbind(TRANS,FFF)
      } #endif
    } #endif
  } # end for acId
  TRANS = as.data.frame(TRANS)
  row.names(TRANS) = c(1:nrow(TRANS))
  return(TRANS)
} #end  apiGetTransactions

#' Splits TD Ameritrade transaction date
#'
#' Gets the date part of transaction date
#' @param dd The date like  "2023-08-07T18:15:32+0000"
#' @return The date part ("2023-08-07")
#' @examples
#' trDate = splitTransDate("2023-08-07T18:15:32+0000")
splitTransDate = function (dd){
  day = strsplit(dd,'T',fixed = TRUE)[[1]][1];
  return(day)
} # end splitTransDate

#' Splits TD Ameritrade transaction date
#'
#' Gets the time part of transaction date
#' @param dd The date like  "2023-08-07T18:15:32+0000"
#' @return The time part ("18:15:32+0000")
#' @examples
#' trTime = splitTransTime("2023-08-07T18:15:32+0000")
splitTransTime = function (dd){
  time = strsplit(dd,'T',fixed = TRUE)[[1]][2];
  return(time)
} # end splitTransTime


#' Splits date/time data
#'
#' Gets the date part of the date
#' @param dd The date like  "2023-10-19 14:32:30"
#' @return The date part ("2023-10-19")
#' @examples
#' trDate = getTradeDate("2023-10-19 14:32:30")
getTradeDate = function(dd) {
  aDate = strsplit(dd,' ',fixed = TRUE)[[1]][1];
  return(aDate)
} # end getTradeDate

#' Splits date data
#'
#' Gets the year part of the date
#' @param dd The date like  "2023-10-19"
#' @return The year part ("2023")
#' @examples
#' trYear = getTradeDateYear("2023-10-19")
getTradeDateYear = function(dd) {
  allDate = strsplit(dd,' ',fixed = TRUE)[[1]][1];
  tt = strsplit(allDate,'-',fixed = TRUE)[[1]]
  dYear = tt[1]
  return(dYear)
} # end getTradeDateYear

#' Splits date data
#'
#' Gets the month part of the date
#' @param dd The date like  "2023-10-19"
#' @return The month part ("10")
#' @examples
#' trMonth = getTradeDateMonth("2023-10-19")
getTradeDateMonth = function(dd) {
  allDate = strsplit(dd,' ',fixed = TRUE)[[1]][1];
  tt = strsplit(allDate,'-',fixed = TRUE)[[1]]
  dMonth = tt[2]
  return(dMonth)
} # end getTradeDateMonth


#' Gets trade statistics
#'
#' Gets statistics for a set of trades
#' @param TRADES The data frame of trades
#' @params cap The initial capital
#' @return The list of statististic that includes:
#' total P or L,total commision,yield,yield percentage,number of trades,
#' number of winners, winners percentage,number of contracts,maximal win and lost,
#' average capital at risk,average ROI,average trade duration,
#' longest and shortest trade durations
#' @examples
#' trMonth = getStatistics(TRADES,cap)
#' @export
getStatistics = function(TRADES,cap) {
  plTotal = sum(TRADES$pl)
  commisTotal = sum(TRADES$entryCommis) + sum(TRADES$exitCommis)
  yield =  plTotal - commisTotal
  pcYield = round(yield/cap*100,2)
  winners = sum(TRADES$winner == 1)
  pcWinners = round(winners/nrow(TRADES)*100,2)
  contracts = sum(TRADES$amount)
  maxWin = max(TRADES$pl)
  maxLoss = min(TRADES$pl)
  avgCapRisk = round(mean(TRADES$capAtRisk),2)
  avgRoi = round(mean(TRADES$roi),2)
  avgDuration = round(mean(TRADES$tradeDur),2)
  maxDuration = max(TRADES$tradeDur)
  minDuration = min(TRADES$tradeDur)
  return (c(plTotal,commisTotal,yield,pcYield,nrow(TRADES),winners,pcWinners,contracts,maxWin,maxLoss,
            avgCapRisk,avgRoi,avgDuration,maxDuration,minDuration))
} # end getStatistics


#' Gets day trades
#'
#'Processes transactions to get option day trades
#' @param TRANS The data frame of transactions
#' @return The data frame with the following columns:
#' "accountId","entryDate","exitDate","underSymbol","symbol",
#' "entryPrice","exitPrice","amount","entryCommis","exitCommis","pl",
#' "winner","capAtRisk","roi","tradeDur","year","month","day"
#' @examples
#' TRADES = processOneDayTrades(TRANS)
#' @export
processOneDayTrades = function(TRANS) {
  TRADES = data.frame(NULL)
  isFirst = TRUE
  TRANSOP = TRANS[(TRANS$type == 'TRADE') & (TRANS$assetType == 'OPTION'),]
  TrDay = unlist(sapply(TRANSOP$date,splitTransDate,simplify = F))
  TrTime = unlist(sapply(TRANSOP$date,splitTransTime,simplify = F))
  TRANSOP$day = TrDay
  TRANSOP$time = TrTime
  uDays = unique(TRANSOP$day)

  for (d in uDays){ # Loop for every day
    TR = TRANSOP[(TRANSOP$day == d),]
    if (nrow(TR) > 1) {
      uCS = unique(TR$cusip)
      for (cs in uCS) { # Loop for every symbol
        TR1 = TR[TR$cusip == cs,]
        if (nrow(TR1) > 1){
          TR1 = TR1[order(TR1$time),]
          entryDate = exitDate = ''
          amount = pl = entryPrice = exitPrice = 0;
          entryCommis = exitCommis = 0;
          entered = 0; exited = 0
          for (k in 1:nrow(TR1)){ # Loop for every trade
            tr = TR1[k,]
            #print(tr)
            if (tr$positionEffect == 'OPENING') {
              if (entered == 0) {
                entryDate = sub('+0000','',sub('T',' ',tr$date,fixed=T),fixed = T)
                entryYear = getTradeDateYear(entryDate)
                entryMonth = getTradeDateMonth(entryDate)
                entryPrice = as.numeric(tr$price)
                entered = 1
              } else {
                trprice = as.numeric(tr$price)
                tramount = as.numeric(tr$amount)
                entryPrice = (amount*entryPrice + tramount*trprice)/(amount + tramount)
              }#endif
              amount = amount + as.numeric(tr$amount)
              pl = pl + as.numeric(tr$cost)
              entryCommis = entryCommis + as.numeric(tr$fees)
            } #endif
            if ((tr$positionEffect == 'CLOSING') & (entered == 1)){
              if (exited == 0) {
                exitPrice = as.numeric(tr$price)
                examount = as.numeric(tr$amount)
                exited = 1
              } else {
                trprice = as.numeric(tr$price)
                tramount = as.numeric(tr$amount)
                exitPrice = (examount*exitPrice + tramount*trprice)/(examount + tramount)
                examount = examount + tramount
              } #endif
              pl = pl + as.numeric(tr$cost)
              exitCommis = exitCommis + as.numeric(tr$fees)
              if (amount - examount == 0) {
                entered = 0
                exited = 0
                exitDate = sub('+0000','',sub('T',' ',tr$date,fixed=T),fixed = T)
                winner = ifelse(pl > 0,1,0)
                capAtRisk = entryPrice*amount*100
                tradeDur = as.numeric(difftime(exitDate,entryDate,units="secs"))
                # Save the record
                eDate = getTradeDate(entryDate)
                roi = pl/capAtRisk*100
                rec = c(tr$accountId,entryDate,exitDate,tr$underSymbol,tr$symbol,entryPrice,exitPrice,amount,entryCommis,exitCommis,pl,winner,capAtRisk,roi,tradeDur,entryYear,entryMonth,eDate)
                TRADES = rbind(TRADES,rec)
                entryDate = exitDate = ''
                amount = pl = entryPrice = exitPrice = 0;
                entryCommis = exitCommis = 0;
              } #endif
            } #endif
          } # end for k
        } #endif
      } #end for cs
    } #endif
  } #end for d
  TRADES = data.frame(TRADES)
  colnames(TRADES) = c("accountId","entryDate","exitDate","underSymbol","symbol","entryPrice","exitPrice","amount","entryCommis","exitCommis","pl","winner","capAtRisk","roi","tradeDur","year","month","day")
  TRADES$entryPrice = as.numeric(TRADES$entryPrice)
  TRADES$exitPrice = as.numeric(TRADES$exitPrice)
  TRADES$amount = as.numeric(TRADES$amount)
  TRADES$entryCommis = as.numeric(TRADES$entryCommis)
  TRADES$exitCommis  = as.numeric(TRADES$exitCommis)
  TRADES$pl  = as.numeric(TRADES$pl)
  TRADES$winner  = as.numeric(TRADES$winner)
  TRADES$capAtRisk  = as.numeric(TRADES$capAtRisk)
  TRADES$roi  = as.numeric(TRADES$roi)
  TRADES$tradeDur  = as.numeric(TRADES$tradeDur)
  return(TRADES)
} # end processOneDayTrades

#' Generates report for day trades
#'
#'Processes transactions to get option day trades
#' @param TRANS The data frame of transactions
#' @return The data frame with the following columns:
#' "accountId","entryDate","exitDate","underSymbol","symbol",
#' "entryPrice","exitPrice","amount","entryCommis","exitCommis","pl",
#' "winner","capAtRisk","roi","tradeDur","year","month","day"
#' @examples
#' TRADES = processOneDayTrades(TRANS)
#' @export
generateOneDayReport = function(TRADES,header = TRUE) {

  # == 1 == Output the header
  RES = data.frame(NULL)
  header1 = c("Action","Trade Date","Stock","","Option","Quantity","Price","Stop","Target","Gross Entry/Exit","# contracts",
              "Commission","Gross P or L","Winner","Loser","Capital At Risk","Porfolio Size","","ROI","Win %","Loss %","Duration Of Signal","Win $","Loss $")
  emptyRow = rep("",length(header1))
  if (header) {
    RES = rbind(RES,emptyRow)
    colnames(RES) = header1
  } # endif

  # == 2 == For each trade generate two lines of data
  for (k in 1:nrow(TRADES)) {
    tr = TRADES[k,]

    # == 2.1 == Output the first line
    entryPrice = as.numeric(tr$entryPrice)
    stop = entryPrice*0.9; target = entryPrice *1.2
    rec1 = c("Entry",tr$entryDate,tr$underSymbol,'',tr$symbol,tr$amount,tr$entryPrice,stop,target,tr$entryPrice,tr$amount,tr$entryCommis,'','','','','','','','','','','','')
    RES = rbind(RES,rec1)

    # == 2.2 == Output the second line and empty line
    winner = tr$winner
    roi = tr$roi
    pl = tr$pl
    winInd = ifelse(winner == 1,1,'')
    loseInd = ifelse(winner == 1,'',1)
    pcwin = ifelse(winner == 1,roi,'')
    pclose = ifelse(winner == 1,'',roi)
    winUSD = ifelse(winner == 1,pl,'')
    lostUSD = ifelse(winner == 1,'',pl)
    rec2 = c("Exit",tr$exitDate,'','','',paste0("-",tr$amount),tr$exitPrice,'','',tr$exitPrice,tr$amount,tr$exitCommis,pl,winInd,loseInd,tr$capAtRisk,'','',tr$roi,pcwin,pclose,tr$tradeDur,winUSD,lostUSD)
    RES = rbind(RES,rec2)
    RES = rbind(RES,emptyRow)
  } # end for k

  return(RES)
} #end generateOneDayReport

getTradingStatistics = function(TRADES,cap) {
  monthNum2String = c("January","February","March","April","May","June","July","August","September","October","November","December")
  STATS = data.frame(NULL)
  statsHeader = c("year","month","day","pl","commis","yield","pcYield","trades","winners","pcWinners","contracts","maxWin","maxLoss",
                  "avgCapRisk","avgRoi","avgDuration","maxDuration","minDuration")
  # == 1 == Total statistics
  total = getStatistics(TRADES,cap)
  rec =  c("Total","","",total)
  STATS = rbind(STATS,rec)
  colnames(STATS) = statsHeader

  # == 2 == Monthly statistics
  uYear = sort(unique(TRADES$year))
  for (y in uYear) {
    YTRADES = TRADES[TRADES$year == y,]
    uMonth = sort(unique(YTRADES$month))
    for (m in uMonth) {
      MTRADES = YTRADES[YTRADES$month == m,]
      if (nrow(MTRADES) > 0) {
        MTRADES = MTRADES[order(MTRADES$entryDate),]
        mo = monthNum2String[as.numeric(m)]
        rec = c(y,mo,"",getStatistics(MTRADES,cap))
        STATS = rbind(STATS,rec)
      } #endif
    } #end for m
  } #end for y

  # == 3 == Daily statistics
  uYear = sort(unique(TRADES$year))
  for (y in uYear) {
    YTRADES = TRADES[TRADES$year == y,]
    uMonth = sort(unique(YTRADES$month))
    for (m in uMonth) {
      MTRADES = YTRADES[YTRADES$month == m,]
      if (nrow(MTRADES) > 0) {
        MTRADES = MTRADES[order(MTRADES$entryDate),]
        uDays = unique(MTRADES$day)
        for (d in uDays) {
          DTRADES = MTRADES[MTRADES$day == d,]
          mo = monthNum2String[as.numeric(m)]
          rec = c(y,mo,d,getStatistics(DTRADES,cap))
          STATS = rbind(STATS,rec)
        } # end for d
      } #endif
    } #end for m
  } #end for y

  return(STATS)
} # end getTradingStatistics

# return(c(symb,nTrades,nWins,pcWins,yield,nCalls,nCallsWins,pcCallsWins,nPuts,nPutsWins,pcPutsWins))

add_ticker_statistics= function(tStat){
  txt = paste("Symbol |Trades  |Wins    |Wins %  |Yield   |Calls   |Call Wins |Call Wins % |Puts    |Put Wins |Put Wins %",
              "-------|--------|--------|--------|--------|--------|----------|------------|--------|---------|----------",sep="n")
  for (k in 1:nrow(tStat)) {
    curStat = tStat[k,]
    txt1 = paste(curStat[1],curStat[2],curStat[3],paste0(curStat[4],"%"),toUSD(as.numeric(curStat[5])),curStat[6],curStat[7],paste0(curStat[8],"%"),curStat[9],curStat[10],paste0(curStat[11],"%"),sep="|")
    txt = paste(txt,txt1,sep="\n")
  } #end for k
  return(txt)
} # add_ticker_statistics


### ================   E N D      F U N C T I O N S   =========================
