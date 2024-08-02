# =============================================================================
# oneDayTradesReport.r processes one-day option trade data
#                      with TDAm API using R package 
#                      TDAmeritrade tools project
# -----------------------------------------------------------------------------
# Programmer : Valery Petrushin
# Creation Date: October 12, 2023
# Modification Date: February 7, 2024
# =============================================================================

library(rameritrade)
library(lubridate)

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

apiGetTransactions = function(acIds,sDate = STDATE) {
  # Get all transactions for accounts from sDate to the current date
  # Parameters: 
  #  acIds is a list of accounts' IDs (numbers)
  #  sDate is the start date as a string "yyy-mm-dd"
  
  TRANS = NULL
  colNamesList = c("accountId","type","description", "date",
                   "symbol","underSymbol","cusip","assetType","expirationDate",
                   "amount","price","cost","netAmount","instruction","positionEffect","fees")           
  print(paste("Accounts: ",paste(acIds,collapse = " | ")))
  for (i in 1: length(acIds)) {
    acId = acIds[i]
    print(paste0("Account: ",acId))
    trans = td_transactSearch(accountNumber = acId,startDate = sDate,endDate = Sys.Date())
    trans = as.data.frame(trans)
    print(paste0("Number of transactions: ",nrow(trans)))
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


splitTransDate = function (dd){
  day = strsplit(dd,'T',fixed = TRUE)[[1]][1];
  return(day)
} # end splitTransDate

splitTransTime = function (dd){
  time = strsplit(dd,'T',fixed = TRUE)[[1]][2];
  return(time)
} # end splitTransTime

getTradeDate = function(dd) {
  aDate = strsplit(dd,' ',fixed = TRUE)[[1]][1];
  return(aDate)
} # end getTradeDate

getTradeDateYear = function(dd) {
  allDate = strsplit(dd,' ',fixed = TRUE)[[1]][1];
  tt = strsplit(allDate,'-',fixed = TRUE)[[1]]
  dYear = tt[1]
  return(dYear)
} # end getTradeDateYear

getTradeDateMonth = function(dd) {
  allDate = strsplit(dd,' ',fixed = TRUE)[[1]][1];
  tt = strsplit(allDate,'-',fixed = TRUE)[[1]]
  dMonth = tt[2]
  return(dMonth)
} # end getTradeDateMonth

getStatistics = function(TRADES,cap) {
  
  # == 1 == Profit & Loss
  plTotal = sum(TRADES$pl)
  commisTotal = sum(TRADES$entryCommis) + sum(TRADES$exitCommis)
  yield =  plTotal - commisTotal
  pcYield = round(yield/cap*100,2)
  avgRoi = round(mean(TRADES$roi),2)
  
  # == 2 == Number of trades and wins
  winners = sum(TRADES$winner == 1)
  nTrades = nrow(TRADES)
  pcWinners = round(winners/nTrades*100,2)
  maxWin = max(TRADES$pl)
  maxLoss = min(TRADES$pl)
  if (maxLoss >= 0) maxLoss = 0
  
  # == 3 == Number of contracts and capital at risk  
  contracts = sum(TRADES$amount)
  contrTrade = round(contracts/nTrades,2)
  avgCapRisk = round(mean(TRADES$capAtRisk),2)
  
  # == 4 == Trade durations
  avgDuration = round(mean(TRADES$tradeDur),2)
  maxDuration = max(TRADES$tradeDur)
  minDuration = min(TRADES$tradeDur)
  
  # == 5 == Trading days used
  sDate = getTradeDate(TRADES$entryDate[1])
  eDate = getTradeDate(TRADES$entryDate[nTrades])
  dRange = WD$workDates[which(WD$dates == sDate):which(WD$dates == eDate)]
  trDays = sum(dRange)
  trDaysUsed = length(unique(TRADES$day))
  pctrDaysUsed = round(trDaysUsed/trDays*100,2)
  
  stats = c(plTotal,commisTotal,yield,pcYield,nTrades,winners,pcWinners,contracts,contrTrade,maxWin,maxLoss,
            avgCapRisk,avgRoi,avgDuration,maxDuration,minDuration,trDays,trDaysUsed,pctrDaysUsed)
  return(stats)
} # end getStatistics

getTickerStatistics = function(TRADES){
  # == 1 == Symbols used
  symTable = table(TRADES$underSymbol)
  symbs = attr(symTable,"dimnames")[[1]]
  tickStats = NULL
  for (symb in symbs) {
    rec = countCallsPuts(TRADES,symb)
    tickStats = rbind(tickStats,rec)
  } #end for symb
  tickStats = data.frame(tickStats)
  colnames(tickStats) = c('symbol','nTrades','nWins','pcWins','nCalls','nCallsWins','pcCallsWins','nPuts','nPutsWins','pcPutsWins')
  return(tickStats)
} # end getTickerStatistics

getTimeSeries = function (TRADES,cap,level = 'total') {
  shortMonth = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  # == 1 == Time series
  if (level == 'total') {
    # == 1.1 == Aggregate to monthly
    uYears = sort(unique(TRADES$year))
    value = cap
    trSeries = NULL
    for (y in uYears) {
      YTRADES = TRADES[TRADES$year == y,]
      uMonths = sort(unique(YTRADES$month))
      for (mo in uMonths) {
        MTRADES = YTRADES[YTRADES$month == mo,]
        mStats = getStatistics(MTRADES,value)
        mTime = paste0(shortMonth[as.numeric(mo)],'-',substr(y,3,4))
        value = value + mStats[3]
        rec = c(mTime,value,mStats[3],mStats[5:7],mStats[9],mStats[13:14])
        trSeries = rbind(trSeries,rec)
      } #end for mo
    } #end for y
  } else {
    # == 1.2 == Aggregate to daily
    uYears = sort(unique(TRADES$year))
    value = cap
    trSeries = NULL
    DD = sapply(sort(TRADES$entryDate),getTradeDate)
    uDates = unique(DD)
    value = cap
    trSeries = NULL
    for (dd in uDates) {
      DTRADES = TRADES[DD == dd,]
      dStats = getStatistics(DTRADES,value)
      dTime = dd
      value = value + dStats[3]
      rec = c(dTime,value,dStats[3],dStats[5:7],dStats[9],dStats[13:14])
      trSeries = rbind(trSeries,rec)
    } #end for dd  
  }#endif
  trSeries = data.frame(trSeries)
  colnames(trSeries) = c('time','value','yield','ntrades','winners','pcwinners','contrtrade','roi','duration')
  return(trSeries)
} # end getTimeSeries

#.             1.       2.       3.     4.      5.      6.       7.        8.        9.       10.    11
# stats = c(plTotal,commisTotal,yield,pcYield,nTrades,winners,pcWinners,contracts,contrTrade,maxWin,maxLoss,
#.            12.       13.     14.          15.         16.       17.     18.         19
#          avgCapRisk,avgRoi,avgDuration,maxDuration,minDuration,trDays,trDaysUsed,pctrDaysUsed)


countCallsPuts = function(TRADES,symb) {
  symOpt = TRADES[TRADES$underSymbol == symb,c('symbol','winner')]
  optList = NULL
  for (ss in symOpt$symbol) {
    optList = c(optList,substr(strsplit(ss,'_',fixed = T)[[1]][2],7,7))
  } # end for ss
  symOpt$optList = optList
  nTrades = nrow(symOpt)
  nWins = sum(symOpt$winner)
  pcWins = round(nWins/nTrades*100,2)
  calls = symOpt[symOpt$optList == 'C',]
  nCalls = nrow(calls)
  nCallsWins = sum(calls$winner)
  if (nCalls > 0) { 
    pcCallsWins = round(nCallsWins/nCalls*100,2)
  } else {
    pcCallsWins = 0
  } #endif
  puts = symOpt[symOpt$optList == 'P',]
  nPuts = nrow(puts)
  nPutsWins = sum(puts$winner)
  if (nPuts > 0) {
    pcPutsWins = round(nPutsWins/nPuts*100,2)
  } else {
    pcPutsWins = 0
  } # endif
  return(c(symb,nTrades,nWins,pcWins,nCalls,nCallsWins,pcCallsWins,nPuts,nPutsWins,pcPutsWins))
} # end countCallsPuts


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

### ================   E N D      F U N C T I O N S   =========================


### ===================    M A I N.    P R O G R A M   ========================
cap = 25000  # initial capital
curDate = format(Sys.Date(),format = "%Y-%m-%d")  # date in a format yyyy-mm-dd

# === Working dates 2023 - 2024
marketHolidays = c('2023-01-02','2023-01-16','2023-02-20','2023-04-07','2023-05-29',
                   '2023-06-19','2023-07-04','2023-09-04','2023-11-23','2023-12-25',
                   '2024-01-01','2024-01-15','2024-02-19','2024-03-29','2024-05-27',
                   '2024-06-19','2024-07-04','2024-09-02','2024-11-28','2024-12-25')
dates = seq(ymd('2023-01-01'),ymd('2024-12-31'), by = 'day')
dayOfWeek = weekdays(dates)
workDates = rep(1,length(dates))
workDates[dayOfWeek == "Saturday"] = 0
workDates[dayOfWeek == "Sunday"] = 0
for (dd in marketHolidays) {
  workDates[dates == dd] = 0
} # end for dd
WD = data.frame(dates,dayOfWeek,workDates)

# == 1 == Create folder for transactions and reports
wDir = paste0(getwd(),"/",curDate)
if (!dir.exists(wDir)) {dir.create(wDir)} #endif
transFile = paste0(wDir,"/Transactions_",curDate,".csv")
tradeFile = paste0(wDir,"/TradesHistory.csv")

# == 2 == Update API tokens
apiUpdateTokens()

# == 3  == Get transactions data for all accounts
# == 3.1 ==  Get accounts' data
actDF = td_accountData()
acBal = actDF$balances
# == 3.2 == Get transactions
acIds = sort(acBal$accountId)
TRANS = apiGetTransactions(acIds,STDATE)
TRANS = TRANS[order(TRANS$accountId,TRANS$date),]
write.csv(TRANS,transFile,row.names = F)
print(paste("Saved transaction file",transFile))

# == 4 == Create trade data and reports for each account
TRADES = processOneDayTrades(TRANS)
write.csv(TRADES,tradeFile,row.names = F)
print(paste("Saved trade history file",tradeFile))

uAcc = unique(TRADES$accountId)
for (ac in uAcc) {
  TRADESAC = TRADES[TRADES$accountId == ac,]
  REPORT = generateOneDayReport(TRADESAC)
  repFile = paste0(wDir,"/Report_",ac,"_",curDate,".csv")
  write.csv(REPORT,repFile,row.names = F)
  
  acStats = getStatistics(TRADESAC,cap)  
  acTicStats = getTickerStatistics(TRADESAC)
  acTimeSeries = getTimeSeries(TRADESAC,cap,level = 'total')
  print(paste('Total statistics for account',ac))
  print(acStats)
  print(acTicStats)
  print(acTimeSeries)
  
  uYear = sort(unique(TRADESAC$year))
  value = cap
  for (y in uYear) {
    YTRADES = TRADESAC[TRADESAC$year == y,]
    uMonth = sort(unique(YTRADES$month))
    for (m in uMonth) {
      MTRADES = YTRADES[YTRADES$month == m,]
      if (nrow(MTRADES) > 0) {
        MTRADES = MTRADES[order(MTRADES$entryDate),]
        dStats = getStatistics(MTRADES,value) 
        dTicStats = getTickerStatistics(MTRADES)
        dTimeSeries = getTimeSeries(MTRADES,value,level = 'daily')
        value = value + dStats[3]
        print(paste('Monthly statistics for account',ac,'and month',m))
        print(dStats)
        print(dTicStats)
        print(dTimeSeries)
      } #endif
    } #end for m
  } #end for y
  
  
  # STATS = getTradingStatistics(TRADESAC,cap)
  # statFile = paste0(wDir,"/Statistics_",ac,"_",curDate,".csv")
  # write.csv(STATS,statFile,row.names = F)
} # end for ac


