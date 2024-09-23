# =============================================================================
# daytrading.R  R-package for processing one-day option trade data
#               and generating reports
#               This is the second step after getting trade data using
#               Schwab's API (a Python package)
# -----------------------------------------------------------------------------
# Programmer : Valery Petrushin
# Creation Date: July 23, 2024
# Modification Date: August 16, 2024
# =============================================================================

### ======================    F U N C T I O N S   =============================

STDATE = format(Sys.Date() - 252,format = "%Y-%m-%d") # date a 3 month ago

#' Converts a number to US dollar format
#'
#' @param money a number
#'
#' @return a string that represent number as a dollar value
#' @export
#'
#' @examples
#' toUSD(109245.65)  # "$109,245.65"
#' toUSD(-10924.6)   # "-$10,924.60"
toUSD = function(money){
  scales::dollar(money,largest_with_cents = 1e+10)
} #end toUSD

#' Gets the last name
#'
#' @param name  is a string that has a name separated by "."
#'
#' @return the substring after the last "."
#'
#' @export
#'
#' @examples
#' getLastName("aaa.bbb.ccc")  # returns "ccc"
#'
getLastName = function(name) {
  arr = strsplit(name,'.',fixed = T)[[1]]
  return(arr[length(arr)])
} #end getLastName


#' Splits Schwab's transaction date and gets the date part of it
#'
#' @param dd The date like  "2023-08-07T18:15:32+0000"
#' @return The date part ("2023-08-07")
#'
splitTransDate = function (dd){
  day = strsplit(dd,'T',fixed = TRUE)[[1]][1];
  return(day)
} # end splitTransDate

#' Splits Schwab's transaction date and gets the time part of it
#'
#' @param dd The date like  "2023-08-07T18:15:32+0000"
#' @return The time part ("18:15:32+0000")
#'
splitTransTime = function (dd){
  time = strsplit(dd,'T',fixed = TRUE)[[1]][2];
  return(time)
} # end splitTransTime

#' Splits date/time data
#'
#' Gets the date part of the date
#' @param dd The date like  "2023-10-19 14:32:30"
#' @return The date part ("2023-10-19")
#'
getTradeDate = function(dd) {
  aDate = strsplit(dd,' ',fixed = TRUE)[[1]][1];
  return(aDate)
} # end getTradeDate

#' Splits date data
#'
#' Gets the year part of the date
#' @param dd The date like  "2023-10-19"
#' @return The year part ("2023")
#'
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
#'
getTradeDateMonth = function(dd) {
  allDate = strsplit(dd,' ',fixed = TRUE)[[1]][1];
  tt = strsplit(allDate,'-',fixed = TRUE)[[1]]
  dMonth = tt[2]
  return(dMonth)
} # end getTradeDateMonth

#' Read the params.yml file that specifies parameters
#'
#'The YAML file params.yml may have parameters for one or more accounts.
#'The multiple accounts should be linked.
#'It has the following structure:
#'---
#'- account:
#'    number: '99999999'
#'    initValue: 25000
#'    startDate: 2024-01-01
#'   . . .
#'- account:
#'    number: '88888888'
#'    initValue: 30000
#'    startDate: 2024-01-01
#' @return The data frame with the following fields:
#'         "account.number", "account.initValue", "account.startDate"
#' @export
#'
getParams = function() {
  params = yaml::read_yaml("params.yml")
  PARS = NULL
  for (k in 1:length(params)){
    PARS = rbind(PARS,as.data.frame(params[[k]]))
  } # end for k
  return(PARS)
} # end getParams

#' Analyze and select account numbers
#'
#' @param accNumbers a set of numbers from Schwab account
#' @param parNumbers a set of numbers from parameter file
#'
#' @return a data frame with the following fields
#'         'accountNumber','initValue','startDate','hashValue'
#' @export
select_accountNumbers = function(accNumbers,parNumbers) {
  # == 1 == Analyze account numbers
  # Check if the numbers from parameter file is a subset of Schwab account numbers
  if (!setequal(intersect(parNumbers,accNumbers), parNumbers)) {
    diff = setdiff(parNumbers,accNumbers)
    print(paste0("No data available for account(s) ",diff))
    parNumbers = intersect(parNumbers,accNumbers)
  } #endif

  # == 2 == Select account numbers and add hash values
  isFirst = TRUE
  for (num in parNumbers){
    hashValue = accNums[accNums$accountNumber == num,"hashValue"]
    ROW = unlist(c(PARS[PARS$account.number == num,],hashValue))
    if (isFirst) {
      SEL = ROW
      isFirst = FALSE
    } else {
      SEL = rbind(SEL,ROW)
    }#endif
  } #end for num
  SEL = as.data.frame(SEL)
  colnames(SEL) = c('accountNumber','initValue','startDate','hashValue')
  return(SEL)
} # end select_accountNumbers


#' Gets trade transaction for the specified account
#'
#' @param tokens are access and refresh tokens for account
#' @param accHash is a hash value for specific account number
#' @param startDate is the start date for transactions
#' @param endDate is the end date for transactions
#' @param assetType is the assete type ('OPTION' (default),'FUTURE' or 'EQUITY')
#'
#' @return a data frame with transactions
#' @export
getTransactions = function(tokens,accHash,startDate,endDate,assetType = 'OPTION') {

  transRaw = charlesschwabapi::get_transactions(tokens,accHash,startDate,endDate,types = 'TRADE')
  isFirst = TRUE
  TRANS = NULL
  for (k in 1:nrow(transRaw)) {
    curRec = transRaw[k,]
    trandf = as.data.frame(curRec$transferItems)
    if (trandf$instrument.assetType == assetType) {
      curRow =cbind(curRec[1:10],trandf)
      if (isFirst) {
        TRANS = curRow
        isFirst = FALSE
      } else {
        TRANS = dplyr::bind_rows(TRANS,curRow)
      } #endif
    } #endif
  } #end for k
  TRANS = TRANS[order(TRANS$time),]
  return(TRANS)
} #end getTransactions


#' Get statistics for all option trades for underlying symbol
#'
#' @param TRADES The data frame of trades with the following fields:
#'         "accountId","entryDate","exitDate","underSymbol","symbol",
#'         "entryPrice","exitPrice","amount","entryCommis","exitCommis","pl",
#'         "winner","capAtRisk","roi","tradeDur","year","month","day"
#' @return The data frame with the following fields:
#'   'symbol','nTrades','nWins','pcWins','yield',
#'   'nCalls','nCallsWins','pcCallsWins',
#'   'nPuts','nPutsWins','pcPutsWins'
#' @export
#'
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
  colnames(tickStats) = c('symbol','nTrades','nWins','pcWins','yield','nCalls','nCallsWins','pcCallsWins','nPuts','nPutsWins','pcPutsWins')
  tickStats$nTrades = as.integer(tickStats$nTrades)
  tickStats$nWins = as.integer(tickStats$nWins)
  tickStats$pcWins = as.numeric(tickStats$pcWins)
  tickStats$yield = as.numeric(tickStats$yield)
  tickStats$nCalls = as.integer(tickStats$nCalls)
  tickStats$nCallsWins = as.integer(tickStats$nCallsWins)
  tickStats$pcCallsWins = as.numeric(tickStats$pcCallsWins)
  tickStats$nPuts = as.integer(tickStats$nPuts)
  tickStats$nPutsWins = as.integer(tickStats$nPutsWins)
  tickStats$pcPutsWins = as.numeric(tickStats$pcPutsWins)
  return(tickStats)
} # end getTickerStatistics

#' Gets trade statistics
#'
#' Gets statistics for a set of trades
#'
#' @param TRADES The data frame of trades
#' @param WD is the frame of market working dates with the following fields:
#'           "dates","dayOfWeek","workDates"
#' @param startDate is the first global trade date
#' @param endDate is the last global trade date
#' @param level  = 'global', 'year' or 'month'
#' @param cap The initial capital
#'
#' @return The list of statistics that includes:
#' total P or L,total commision,yield,yield percentage,number of trades,
#' number of winners, winners percentage,number of contracts,maximal win and lost,
#' average capital at risk,average ROI,average trade duration,
#' longest and shortest trade durations, number of trading days,
#' number of trading days used, percentage of trading days used
getStatistics = function(TRADES,WD,cap,startDate,endDate,level = 'global') {

  lastDayOfMonth = function(WD,year,month) {
    getYearMonth = function (dd) {
      ss = strsplit(dd,'-',fixed = T)[[1]]
      return(paste0(ss[1],'-',ss[2]))
    } # end getYearMonth
    ym = sapply(WD$dates,getYearMonth)
    k = max(which(ym == paste0(year,'-',month)))
    return(WD$dates[k])
  } #end lastDayOfMonth

  # == 1 == Profit & Loss
  plTotal = sum(TRADES$pl) # ,na.rm = T
  commisTotal = sum(TRADES$entryCommis) + sum(TRADES$exitCommis)
  yield =  plTotal - commisTotal
  pcYield = yield/cap
  avgRoi = mean(TRADES$roi)

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
  sDate = TRADES$day[1]
  eDate = TRADES$day[nTrades]
  year = TRADES$year[1]
  month = TRADES$month[1]
  if (month < 10) {
    month = paste0('0',month)
  } else {
    month = as.character(month)
  } #endif
  # print(paste0("sDate= ",sDate))
  # print(paste0("eDate= ",eDate))
  # print(paste0("year= ",year))
  # print(paste0("month= ",month))

  if (level == 'global') {
    firstD = startDate; lastD = endDate
    if (endDate < curDate) { lastD = curDate}
  } #endif
  if (level == 'year') {
    if (sDate == startDate) {
      firstD = startDate
    } else {
      firstD = paste0(year,"-01-01") # the first day of the year
    } #endif
    if (eDate == endDate) {
      lastD = curDate
    } else {
      lastD = paste0(year,"-12-31") # the last day of the year
    } #endif
  } #endif
  if (level == 'month') {
    if (sDate == startDate) {
      firstD = startDate
    } else {
      firstD = paste0(year,"-",month,"-01") # the first day of month
    } #endif
    if (eDate == endDate) {
      lastD = curDate
    } else {
      lastD = lastDayOfMonth(WD,year,month)
    } #endif
  } #endif

  # print(paste0("firstD= ",firstD," index= ",which(WD$dates == firstD)))
  # print(paste0("lastD= ",lastD," index= ",which(WD$dates == lastD)))
  dRange = WD$workDates[which(WD$dates == firstD):which(WD$dates == lastD)]
  trDays = sum(dRange)
  trDaysUsed = length(unique(TRADES$day))
  # print(paste0("trDays= ",trDays))
  # print(paste0("trDaysUsed= ",trDaysUsed))
  pctrDaysUsed = round(trDaysUsed/trDays*100,2)

  stats = c(plTotal,commisTotal,yield,pcYield,nTrades,winners,pcWinners,contracts,contrTrade,maxWin,maxLoss,
            avgCapRisk,avgRoi,avgDuration,maxDuration,minDuration,trDays,trDaysUsed,pctrDaysUsed)

  return(stats)
} # end getStatistics

#' Create trade time series data frame on monthly or daily level
#'
#' @param TRADES The data frame of trades with the following fields:
#'         "accountId","entryDate","exitDate","underSymbol","symbol",
#'         "entryPrice","exitPrice","amount","entryCommis","exitCommis","pl",
#'         "winner","capAtRisk","roi","tradeDur","year","month","day"
#' @param WD is the frame of market working dates with the following fields:
#'           "dates","dayOfWeek","workDates"
#' @param cap is the portfolio value (capital)
#' @param startDate is the first global trade date
#' @param endDate is the last global trade date
#' @param level if = 'total' then data is aggregated to monthly otherwise to daily
#'
#' @return the time series data frame with the following fields:
#'         'time','value','yield','ntrades','winners','pcwinners',
#'         'contrtrade','roi','duration'
#' @export
#'
getTimeSeries = function (TRADES,WD,cap,startDate,endDate,level = 'total') {
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
        mStats = getStatistics(MTRADES,WD,value,startDate,endDate,level = 'month')
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
      dStats = getStatistics(DTRADES,WD,value,startDate,endDate,level = 'month')
      dTime = dd
      value = value + dStats[3]
      rec = c(dTime,value,dStats[3],dStats[5:7],dStats[9],dStats[13:14])
      trSeries = rbind(trSeries,rec)
    } #end for dd
  }#endif
  trSeries = data.frame(trSeries)
  colnames(trSeries) = c('time','value','yield','ntrades','winners','pcwinners','contrtrade','roi','duration')
  trSeries$value = as.numeric(trSeries$value)
  trSeries$yield = as.numeric(trSeries$yield)
  trSeries$ntrades = as.integer(trSeries$ntrades)
  trSeries$winners = as.integer(trSeries$winners)
  trSeries$pcwinners = as.numeric(trSeries$pcwinners)
  trSeries$contrtrade = as.numeric(trSeries$contrtrade)
  trSeries$roi = as.numeric(trSeries$roi)
  trSeries$duration = as.numeric(trSeries$duration)
  return(trSeries)
} # end getTimeSeries

#' Counts option trades for an underlying symbol
#'
#' @param TRADES The data frame of trades with the following fields:
#'         "accountId","entryDate","exitDate","underSymbol","symbol",
#'         "entryPrice","exitPrice","amount","entryCommis","exitCommis","pl",
#'         "winner","capAtRisk","roi","tradeDur","year","month","day"
#' @param symb is an underlying symbol
#'
#' @return The data frame with the following fields:
#'   "symb","nTrades","nWins","pcWins","yield",
#'   "nCalls","nCallsWins","pcCallsWins",
#'   "nPuts","nPutsWins","pcPutsWins"
#'
countCallsPuts = function(TRADES,symb) {
  symOpt = TRADES[TRADES$underSymbol == symb,c('symbol','winner','pl','entryCommis','exitCommis')]
  optList = NULL
  for (ss in symOpt$symbol) {
    if (grepl('_',ss)) { # TDAm's Option symbol: META_101323C325
      optList = c(optList,substr(strsplit(ss,'_',fixed = T)[[1]][2],7,7))
    } else { # Schwab's option symbol: QQQ   240604C00452000
      ss = gsub("\\s+"," ",ss)
      optList = c(optList,substr(strsplit(ss,' ',fixed = T)[[1]][2],7,7))
    } #endif
  } # end for ss
  symOpt$optList = optList
  yield = sum(symOpt$pl) - sum(symOpt$entryCommis) - sum(symOpt$exitCommis)
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
  return(c(symb,nTrades,nWins,pcWins,yield,nCalls,nCallsWins,pcCallsWins,nPuts,nPutsWins,pcPutsWins))
} # end countCallsPuts


#' Gets day trades
#'
#'Processes transactions to get option day trades
#' @param TRANS The data frame of transactions with the following fields:
#' ,index,activityId,time,accountNumber,type,status,subAccount,tradeDate,orderId,
#' netAmount,positionId,transferItems.amount,transferItems.cost,transferItems.price,
#' transferItems.instrument.assetType,transferItems.instrument.status,transferItems.
#' instrument.symbol,transferItems.instrument.description,transferItems.instrument.instrumentId,
#' transferItems.instrument.expirationDate,transferItems.instrument.optionDeliverables,
#' transferItems.instrument.optionPremiumMultiplier,transferItems.instrument.putCall,
#' transferItems.instrument.strikePrice,transferItems.instrument.type,transferItems.
#' instrument.underlyingSymbol,transferItems.instrument.underlyingCusip,transferItems.feeType,
#' transferItems.instrument.closingPrice,transferItems.instrument.activeContract,
#' transferItems.instrument.lastTradingDate,transferItems.instrument.multiplier,
#' transferItems.instrument.futureType,transferItems.positionEffect
#' @return The data frame with the following columns:
#'         "accountId","entryDate","exitDate","underSymbol","symbol",
#'         "entryPrice","exitPrice","amount","entryCommis","exitCommis","pl",
#'         "winner","capAtRisk","roi","tradeDur","year","month","day"
#' @export
#'
processOneDayTrades = function(TRANS) {

  TRADES = data.frame(NULL)
  isFirst = TRUE
  TRANSOP = TRANS[(TRANS$type == 'TRADE') & (TRANS$assetType == 'OPTION'),]
  TrDay = unlist(sapply(TRANSOP$time,splitTransDate,simplify = F))
  TrTime = unlist(sapply(TRANSOP$time,splitTransTime,simplify = F))
  TRANSOP$day = TrDay
  TRANSOP$trTime = TrTime
  TRANSOP$fees = TRANSOP$cost - TRANSOP$netAmount
  for (k in 1:nrow(TRANSOP)) {
    if (TRANSOP$positionEffect[k] == '') {
      TRANSOP$positionEffect[k] = ifelse(TRANSOP$amount[k] > 0, "OPENING", "CLOSING")
    } #endif
  } #end for k

  uDays = unique(TRANSOP$day)

  for (d in uDays){ # Loop for every day
    TR = TRANSOP[(TRANSOP$day == d),]
    if (nrow(TR) > 1) {
      uSYMB = unique(TR$symbol)
      for (symb in uSYMB) { # Loop for every symbol
        TR1 = TR[TR$symbol == symb,]
        if (nrow(TR1) > 1){
          TR1 = TR1[order(TR1$trTime),]
          entryDate = exitDate = ''
          amount = pl = entryPrice = exitPrice = 0;
          entryCommis = exitCommis = 0;
          entered = 0; exited = 0
          for (k in 1:nrow(TR1)){ # Loop for every trade
            tr = TR1[k,]
            # print(tr)
            if (tr$positionEffect == 'OPENING') {
              if (entered == 0) {
                entryDate = sub('+0000','',sub('T',' ',tr$time,fixed=T),fixed = T)
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
              if (amount + examount == 0) {
                entered = 0
                exited = 0
                exitDate = sub('+0000','',sub('T',' ',tr$time,fixed=T),fixed = T)
                winner = ifelse(pl > 0,1,0)
                capAtRisk = entryPrice*amount*100
                tradeDur = as.numeric(difftime(exitDate,entryDate,units="secs"))
                # Save the record
                eDate = getTradeDate(entryDate)
                roi = pl/capAtRisk*100
                rec = c(tr$accountNumber,entryDate,exitDate,tr$underlyingSymbol,tr$symbol,entryPrice,exitPrice,amount,entryCommis,exitCommis,pl,winner,capAtRisk,roi,tradeDur,entryYear,entryMonth,eDate)
                TRADES = rbind(TRADES,rec)
                entryDate = exitDate = ''
                amount = pl = entryPrice = exitPrice = 0;
                entryCommis = exitCommis = 0;
              } #endif
            } #endif
          } # end for k
        } #endif
      } #end for symb
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


#' Generates low level report for option day trades as a csv file
#'
#'Processes trades to get option day report for each trade
#' @param TRADES The data frame of trades with the following fields:
#'         "accountId","entryDate","exitDate","underSymbol","symbol",
#'         "entryPrice","exitPrice","amount","entryCommis","exitCommis","pl",
#'         "winner","capAtRisk","roi","tradeDur","year","month","day"
#' @param header if = TRUE outputs the header (default is TRUE)
#' @return The data frame with the following columns:
#'         "Action","Trade Date","Stock","","Option","Quantity","Price","Stop",
#'         "Target","Gross Entry/Exit","# contracts","Commission","Gross P or L",
#'         "Winner","Loser","Capital At Risk","Porfolio Size","","ROI","Win %",
#'         "Loss %","Duration Of Signal","Win $","Loss $"
#' @export
#'
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

#' Calculate statistics for option daily trades on all levels
#' total - monthly - daily
#'
#' @param TRADES The data frame of trades with the following fields:
#'         "accountId","entryDate","exitDate","underSymbol","symbol",
#'         "entryPrice","exitPrice","amount","entryCommis","exitCommis","pl",
#'         "winner","capAtRisk","roi","tradeDur","year","month","day"
#' @param WD is the frame of market working dates with the following fields:
#'           "dates","dayOfWeek","workDates"
#' @param cap is the portfolio value (capital)
#' @param startDate is the first global trade date
#' @param endDate is the last global trade date
#'
#' @export
#' @return The statistics data frame with the following fields:
#'   year,month,day,plTotal,commisTotal,yield,pcYield,nTrades,winners,pcWinners,
#'   contracts,contrTrade,maxWin,maxLoss,avgCapRisk,avgRoi,
#'   avgDuration,maxDuration,minDuration,trDays,trDaysUsed,pctrDaysUsed
#'
getTradingStatistics = function(TRADES,WD,cap,startDate,endDate) {
  monthNum2String = c("January","February","March","April","May","June","July","August","September","October","November","December")
  STATS = data.frame(NULL)
  statsHeader = c("year","month","day","pl","commis","yield","pcYield","trades","winners","pcWinners","contracts","maxWin","maxLoss",
                  "avgCapRisk","avgRoi","avgDuration","maxDuration","minDuration","trDays","trDaysUsed","pctrDaysUsed")
  # == 1 == Total statistics
  total = getStatistics(TRADES,WD,cap,startDate,endDate,level = 'global')
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
        rec = c(y,mo,"",getStatistics(MTRADES,WD,cap,startDate,endDate,level = 'month'))
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
          rec = c(y,mo,d,getStatistics(DTRADES,WD,cap,startDate,endDate,level = 'month'))
          STATS = rbind(STATS,rec)
        } # end for d
      } #endif
    } #end for m
  } #end for y

  return(STATS)
} # end getTradingStatistics

#' Create an R Markdown code for the document header
#'
#' @param title is the title of the R Markdown file
#' @param author is the author of R Markdown file
#' @param date is the date of release
#'
#' @return The text for R Markdown title (YAML fragment)
#'
add_header = function(title,author,date) {
  txt = paste("---",paste0("title: \"",title,"\""),paste0("author: \"",author,"\""),
              paste0("date: \"",date,"\""),"output:","  html_document:","    css: \"./www/style.css\"",
              "---","",sep="\n")
  return(txt)
} # end add_header

#' Include an R code into R Markdown file
#'
#' @return text of R Markdown code
add_prefix1 = function() {
  txt = paste("```{r setup, include=FALSE}",
              "knitr::opts_chunk$set(echo = TRUE)",
              "```","",sep = "\n")
  return(txt)
} # end add_prefix1

#' Include an R code into R Markdown file
#'
#' @return text of R Markdown code
add_prefix2 = function() {
  txt = paste("```{r echo = FALSE, warning = FALSE,message=FALSE}",
              "library(daytrading)",
              "```","",sep = "\n")
  return(txt)
} # end add_prefix2

#' Include an R code into R Markdown file
#'
#' @param cap is initial value of portfolio (capital)
#' @param tradesFile is the name of csv file with option day trade data
#'
#' @return text of R Markdown code
add_prefix3 = function(cap,tradesFile) {
  txt = paste("```{r echo = FALSE}",
              paste0("cap = ",cap),
              "curDate = format(Sys.Date(),format = \"%Y-%m-%d\")  # date in a format yyyy-mm-dd",
              "WD = getTradingDates()",
              paste0("tradesFile = \"",tradesFile,"\""),
              "TRADESAC = read.csv(tradesFile,header = T)",
              "```",sep = "\n")
  return(txt)
} # end add_prefix3

#' Add R Markdown code for n-level header
#'
#' @param text is the header's name
#' @param n is the header's level (1-5)
#' @param tabset if =TRUE adds tabset parameters to the header
#'
#' @return text of R Markdown code
add_title_n = function(text,n,tabset = FALSE) {
  if (n < 1) { n = 1}
  if (n > 5) { n = 5}
  pref = paste(rep("#",n),collapse = "")
  text = paste0("\n",pref," ",text)
  if (tabset) {
    text = paste0(text," {.tabset .tabset-pills}")
  } #endif
  res = paste(text,"",sep="\n")
  return (res)
} #end add_title_n


# === Statistics Output Functions ===
#' Add R Markdown code for general statistics
#'
#' @param cap is the initial value
#' @param gStat is the vector that has the following data:
#'   plTotal,commisTotal,yield,pcYield,nTrades,winners,pcWinners,contracts,
#'   contrTrade,maxWin,maxLoss,avgCapRisk,avgRoi,
#'   avgDuration,maxDuration,minDuration,trDays,trDaysUsed,pctrDaysUsed
#'
#' @return text of R Markdown code
#'
add_general_statistics = function(cap,gStat) {
  txt = paste0("**Initial Value:** ",toUSD(cap))
  txt = paste(txt,paste0("**Current Value:** ",toUSD(cap + gStat[3])),sep="<br>\n")
  txt = paste(txt,paste0("**Total Yield:** ",toUSD(gStat[3])),sep="<br>\n")
  txt = paste(txt,paste0("**Total % Yield:** ",round(gStat[4]*100,2),"%"),sep="<br>\n")
  txt = paste(txt,paste0("**Number of Trades:** ",as.integer(gStat[5])),sep="<br>\n")
  txt = paste(txt,paste0("**Number of Winners:** ",as.integer(gStat[6])),sep="<br>\n")
  txt = paste(txt,paste0("**Winners %:** ",gStat[7],"%"),sep="<br>\n")
  txt = paste(txt,paste0("**Number of Contracts:** ",as.integer(gStat[8])),sep="<br>\n")
  txt = paste(txt,paste0("**Number of Contracts per Trade:** ",round(gStat[9],2)),sep="<br>\n")
  txt = paste(txt,paste0("**Average ROI:** ",round(gStat[13],2),"%"),sep="<br>\n")
  txt = paste(txt,paste0("**Average Trade Duration (sec):** ",round(gStat[14],2)),sep="<br>\n")
  txt = paste(txt,paste0("**Minimal Trade Duration (sec):** ",as.integer(gStat[16])),sep="<br>\n")
  txt = paste(txt,paste0("**Maximal Trade Duration (sec):** ",as.integer(gStat[15])),sep="<br>\n")
  txt = paste(txt,paste0("**Number of Trading Days Used:** ",as.integer(gStat[18])," of ",as.integer(gStat[17])),sep="<br>\n")
  txt = paste(txt,paste0("**Trading Days Used %:** ",gStat[19],"%"),sep="<br>\n")
  return(txt)
} # end add_general_statistics

#' Create plotly chart for monthly time series data
#'
#' @param cap is the value of portfolio
#' @param tmData is monthly option day trading data frame with the following fields:
#'   'time','value','yield','ntrades','winners','pcwinners','contrtrade','roi','duration'
#' @export
#'
#' @return plotly picture
add_charts_monthly = function(cap,tmData) {
  chType = 'bar'
  tmData$count = c(1:nrow(tmData))
  # Value chart
  fig1 = plotly::plot_ly(tmData,x =~count, y = ~value, type = chType,name = 'Value (USD)')
  fig1 = fig1 %>% plotly::layout(title = "Value",yaxis = list(title = 'USD'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))

  # Yield chart
  avgYield = mean(tmData$yield)
  fig2 = plotly::plot_ly(tmData,x =~count, y = ~yield, type = chType,name = 'Yield (USD)')
  # colors=RColorBrewer::brewer.pal(12, "Set3"), color = 'orange'
  fig2 = plotly::add_trace(fig2,y = rep(avgYield,nrow(tmData)),name = 'Avg Yield',
                           type='scatter',mode='lines',
                           line = list(color = 'red',width = 3,dash = 'dash'))
  fig2 = fig2 %>% plotly::layout(title = "Yield",yaxis = list(title = 'USD'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  # ROI chart
  avgROI = mean(tmData$roi)
  fig3 = plotly::plot_ly(tmData,x =~count, y = ~roi, type = chType,name = 'ROI %')
  fig3 = plotly::add_trace(fig3,y = rep(avgROI,nrow(tmData)),name = 'Avg ROI',
                           type='scatter',mode='lines',
                           line = list(color = 'pink',width = 3,dash = 'dash'))
  fig3 = fig3 %>% plotly::layout(title = "ROI",yaxis = list(title = '%'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  # Number of Trades chart
  avgTrades = mean(tmData$ntrades)
  fig4 = plotly::plot_ly(tmData,x =~count, y = ~ntrades, type = chType,name = '#Trades')
  fig4 = plotly::add_trace(fig4,y = rep(avgTrades,nrow(tmData)),name = 'Avg #trades',
                           type='scatter',mode='lines',
                           line = list(color = 'blue',width = 3,dash = 'dash'))
  fig4 = fig4 %>% plotly::layout(title = "Number of Trades",yaxis = list(title = 'Count'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  # Contracts per trade chart
  avgContr = mean(tmData$contrtrade)
  fig5 = plotly::plot_ly(tmData,x =~count, y = ~contrtrade, type = chType, name = 'Contracts/Trade')
  fig5 = plotly::add_trace(fig5,y = rep(avgContr,nrow(tmData)),name = 'Avg contracts/trade',
                           type='scatter',mode='lines',
                           line = list(color = 'darkgreen',width = 3,dash = 'dash'))
  fig5 = fig5 %>% plotly::layout(title = "Contracts per Trade",yaxis = list(title = 'Contracts/Trade'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  # Trade duration chart
  avgDur = mean(tmData$duration)
  fig6 = plotly::plot_ly(tmData,x =~count, y = ~duration, type = chType, name = 'Duration (Sec)')
  fig6 = plotly::add_trace(fig6,y = rep(avgDur,nrow(tmData)),name = 'Avg trade duration',
                           type='scatter',mode='lines',
                           line = list(color = 'violet',width = 3,dash = 'dash'))
  fig6 = fig6 %>% plotly::layout(title = "Trade Duration",yaxis = list(title = 'Seconds'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  fig = plotly::subplot(fig1,fig2,fig3,fig4,fig5,fig6,nrows = 3) %>%
    plotly::layout(title = "Performance Charts")
  return(fig)
} # end add_charts_monthly

#' Create plotly chart for daily time series data
#'
#' @param cap is the value of portfolio
#' @param tmData is daily option day trading data frame with the following fields:
#'   'time','value','yield','ntrades','winners','pcwinners','contrtrade','roi','duration'
#'
#' @return plotly picture
add_charts_daily = function(cap,tmData) {
  chType = 'scatter'
  chMode = 'lines'
  tmData$count = c(1:nrow(tmData))
  # Value chart
  fig1 = plotly::plot_ly(tmData,x =~count, y = ~value, type = chType,mode = chMode,name = 'Value (USD)')
  fig1 = fig1 %>% plotly::layout(title = "Value",yaxis = list(title = 'USD'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))

  # Yield chart
  avgYield = mean(tmData$yield)
  fig2 = plotly::plot_ly(tmData,x =~count, y = ~yield, type = chType,mode = chMode,name = 'Yield (USD)')
  # colors=RColorBrewer::brewer.pal(12, "Set3"), color = 'orange'
  fig2 = plotly::add_trace(fig2,y = rep(avgYield,nrow(tmData)),name = 'Avg Yield',
                           type='scatter',mode='lines',
                           line = list(color = 'red',width = 3,dash = 'dash'))
  fig2 = fig2 %>% plotly::layout(title = "Yield",yaxis = list(title = 'USD'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  # ROI chart
  avgROI = mean(tmData$roi)
  fig3 = plotly::plot_ly(tmData,x =~count, y = ~roi, type = chType,mode = chMode,name = 'ROI %')
  fig3 = plotly::add_trace(fig3,y = rep(avgROI,nrow(tmData)),name = 'Avg ROI',
                           type='scatter',mode='lines',
                           line = list(color = 'pink',width = 3,dash = 'dash'))
  fig3 = fig3 %>% plotly::layout(title = "ROI",yaxis = list(title = '%'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  # Number of Trades chart
  avgTrades = mean(tmData$ntrades)
  fig4 = plotly::plot_ly(tmData,x =~count, y = ~ntrades, type = chType,mode = chMode,name = '#Trades')
  fig4 = plotly::add_trace(fig4,y = rep(avgTrades,nrow(tmData)),name = 'Avg #trades',
                           type='scatter',mode='lines',
                           line = list(color = 'blue',width = 3,dash = 'dash'))
  fig4 = fig4 %>% plotly::layout(title = "Number of Trades",yaxis = list(title = 'Count'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  # Contracts per trade chart
  avgContr = mean(tmData$contrtrade)
  fig5 = plotly::plot_ly(tmData,x =~count, y = ~contrtrade, type = chType,mode = chMode, name = 'Contracts/Trade')
  fig5 = plotly::add_trace(fig5,y = rep(avgContr,nrow(tmData)),name = 'Avg contracts/trade',
                           type='scatter',mode='lines',
                           line = list(color = 'darkgreen',width = 3,dash = 'dash'))
  fig5 = fig5 %>% plotly::layout(title = "Contracts per Trade",yaxis = list(title = 'Contracts/Trade'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  # Trade duration chart
  avgDur = mean(tmData$duration)
  fig6 = plotly::plot_ly(tmData,x =~count, y = ~duration, type = chType,mode = chMode, name = 'Duration (Sec)')
  fig6 = plotly::add_trace(fig6,y = rep(avgDur,nrow(tmData)),name = 'Avg trade duration',
                           type='scatter',mode='lines',
                           line = list(color = 'violet',width = 3,dash = 'dash'))
  fig6 = fig6 %>% plotly::layout(title = "Trade Duration",yaxis = list(title = 'Seconds'),
                                 xaxis = list(tickmode = "array",tickvals = tmData$count,
                                              ticktext = tmData$time,title = 'Time'))
  fig = plotly::subplot(fig1,fig2,fig3,fig4,fig5,fig6,nrows = 3) %>%
    plotly::layout(title = "Performance Charts")
  return(fig)
} # end add_charts_daily

#' Create R Markdown code for monthly option day trade time series
#'
#' @param ac is account number
#' @param cap is the value of portfolio
#'
#' @return text of R Markdown code
add_charts = function(ac,cap) {
  txt = paste("","```{r fig.width = 12,fig.height=10,echo = FALSE}",
              paste0("TRADESAC = TRADESAC[order(TRADESAC$entryDate),]"),
              paste0("startDate = TRADESAC$day[1]"),
              paste0("endDate = TRADESAC$day[nrow(TRADESAC)]"),
              paste0("acTimeSeries = getTimeSeries(TRADESAC,WD,",cap,",startDate,endDate,level = 'total')"),
              paste0("add_charts_monthly(",cap,",acTimeSeries)"),
              "```",sep="\n")
  txt = paste(txt,"<br>","<br>",sep="\n")
  return(txt)
} #end add_charts

#' Create R Markdown code for ticker statistics as a table
#'
#' @return text of R Markdown code
add_table = function() {
  txt = paste("","```{r  echo = FALSE}",
              "acTicStats = getTickerStatistics(TRADESAC)",
              "acTicStats = acTicStats[order(-acTicStats$nTrades),]",
              "colnames(acTicStats) = c('Symbol','Trades','Wins','Wins%','Yield','Calls','CWins','CWins%','Puts','PWins','PWins%')",
              "row.names(acTicStats) = c(1:nrow(acTicStats))",
              "tab = knitr::kable(acTicStats,\"html\", table.attr = \"class=\\\"striped\\\"\",",
              "caption = \"<span  style=\\\"color: darkblue; text-align:center\\\">Performance by Symbol</span>\",",
              "align = \"lrrrrrrrrrr\")",
              "txt = paste0(\"<div class=\\\"scroll\\\">\",tab,\"</div>\")",
              "```","`r txt`",sep="\n")
  txt = paste(txt,"<br>","<br>",sep="\n")
  return(txt)
} # end add_table

#' Create R Markdown code for monthly charts for specific year
#'
#' @param y is the year number (2024)
#' @param value is the value of portfolio
#'
#' @return text of R Markdown code
add_charts_year = function(y,value) {
  txt = paste("","```{r fig.width = 12,fig.height=10,echo = FALSE}",
              paste0("YTRADES = TRADESAC[TRADESAC$year == ",y,",]"),
              paste0("yTimeSeries = getTimeSeries(YTRADES,WD,",value,",startDate,endDate,level = 'total')"),
              paste0("add_charts_monthly(",value,",yTimeSeries)"),
              "```",sep ="\n")
  txt = paste(txt,"<br>","<br>",sep="\n")
  return(txt)
} # end add_charts_year

#' Create R Markdown code for ticker statistics for specific year as a table
#'
#' @return text of R Markdown code
add_table_year = function() {
  txt = paste("","```{r  echo = FALSE}",
              "yTicStats = getTickerStatistics(YTRADES)",
              "yTicStats = yTicStats[order(-yTicStats$nTrades),]",
              "colnames(yTicStats) = c(\"Symbol\",\"Trades\",\"Wins\",\"Wins%\",\"Yield\",\"Calls\",\"CWins\",\"CWins%\",\"Puts\",\"PWins\",\"PWins%\")",
              "row.names(yTicStats) = c(1:nrow(yTicStats))",
              "tab = knitr::kable(yTicStats,\"html\", table.attr = \"class=\\\"striped\\\"\",",
              "caption = \"<span  style=\\\"color: darkblue; text-align:center\\\">Performance by Symbol</span>\",",
              "align = \"lrrrrrrrrrr\")",
              "txt = paste0(\"<div class=\\\"scroll\\\">\",tab,\"</div>\")",
              "```",
              "`r txt`",sep = "\n")
  txt = paste(txt,"<br>","<br>",sep="\n")
  return(txt)
} # end add_table_year

#' Create R Markdown code for daily charts for specific month
#'
#' @param m is the month number (1-12)
#' @param value is the value of portfolio
#'
#' @return text of R Markdown code
add_charts_month = function(m,value) {
  txt = paste("","```{r fig.width = 12,fig.height=10,echo = FALSE}",
              paste0("MTRADES = YTRADES[YTRADES$month == ",m,",]"),
              "MTRADES = MTRADES[order(MTRADES$entryDate),]",
              paste0("dTimeSeries = getTimeSeries(MTRADES,WD,",value,",startDate,endDate,level = 'daily')"),
              paste0("add_charts_monthly(",value,",dTimeSeries)"),
              "```",sep ="\n")
  txt = paste(txt,"<br>","<br>",sep="\n")
  return(txt)
} # end

#' Create R Markdown code for ticker statistics for specific month as a table
#'
#' @return text of R Markdown code
add_table_month = function() {
  txt = paste("", "```{r  echo = FALSE}",
              "dTicStats = getTickerStatistics(MTRADES)",
              "dTicStats = dTicStats[order(-dTicStats$nTrades),]",
              "colnames(dTicStats) = c(\"Symbol\",\"Trades\",\"Wins\",\"Wins%\",\"Yield\",\"Calls\",\"CWins\",\"CWins%\",\"Puts\",\"PWins\",\"PWins%\")",
              "row.names(dTicStats) = c(1:nrow(dTicStats))",
              "tab = knitr::kable(dTicStats,\"html\", table.attr = \"class=\\\"striped\\\"\",",
              "caption = \"<span  style=\\\"color: darkblue; text-align:center\\\">Performance by Symbol</span>\",",
              "align = \"lrrrrrrrrrr\")",
              "txt = paste0(\"<div class=\\\"scroll\\\">\",tab,\"</div>\")",
              "```","`r txt`",sep="\n")
  txt = paste(txt,"<br>","<br>",sep="\n")
  return(txt)
} # end add_table_month


#' Create the data frame of market working dates for 2018-2024
#'
#' @return The data frame that has the folloeing fields:
#'         dates is date in the formay 'yyy-mm-dd'
#'         dayOfWeek is the corresponding day of week Sunday - Saturday
#'         workDates = 1 if it is market work day or = 0 otherwise
#' @export
#'
#' @examples
#'       WD = getTradingDates()
getTradingDates = function() {
  # === Working dates 2018 - 2024
  marketHolidays = c('2018-01-01','2018-01-15','2018-02-19','2018-03-30','2018-05-28',
                     '2018-07-04','2018-09-03','2018-11-22','2018-12-25',
                     '2019-01-01','2019-01-21','2019-02-18','2019-04-19','2019-05-27',
                     '2019-07-04','2019-09-02','2019-11-28','2019-12-25',
                     '2020-01-01','2020-01-20','2020-02-17','2020-04-10','2020-05-25',
                     '2020-07-03','2020-09-07','2020-11-26','2020-12-25',
                     '2021-01-02','2021-01-18','2021-02-15','2021-04-02','2021-05-31',
                     '2021-07-05','2021-09-06','2021-11-25','2021-12-24',
                     '2022-01-17','2022-02-21','2022-04-15','2022-05-30',
                     '2022-06-20','2022-07-04','2022-09-05','2022-11-24','2022-12-26',
                     '2023-01-02','2023-01-16','2023-02-20','2023-04-07','2023-05-29',
                     '2023-06-19','2023-07-04','2023-09-04','2023-11-23','2023-12-25',
                     '2024-01-01','2024-01-15','2024-02-19','2024-03-29','2024-05-27',
                     '2024-06-19','2024-07-04','2024-09-02','2024-11-28','2024-12-25')
  dates = seq(lubridate::ymd('2018-01-01'),lubridate::ymd('2024-12-31'), by = 'day')
  dayOfWeek = weekdays(dates)
  workDates = rep(1,length(dates))
  workDates[dayOfWeek == "Saturday"] = 0
  workDates[dayOfWeek == "Sunday"] = 0
  for (dd in marketHolidays) {
    workDates[dates == dd] = 0
  } # end for dd
  WD = data.frame(dates,dayOfWeek,workDates)
  WD$dates = as.character(WD$dates)
  return(WD)
} # end getTradingDates

#' Generate R Markdown code for an account's option day trading
#'
#' @param tradesFile is the file name of one day option trades
#' @param TRADES is the frame of trade data with the following fields:
#'               "accountId","entryDate","exitDate","underSymbol","symbol","entryPrice","exitPrice","amount",
#'               "entryCommis","exitCommis","pl","winner","capAtRisk","roi","tradeDur","year","month","day"
#' @param ac is the account number
#' @param curDate is the current date (format yyyy-mm-dd)
#' @param WD is the frame of market working dates with the following fields:
#'           "dates","dayOfWeek","workDates"
#' @param cap is the value of account at the beginning
#' @param startDate is the first global trade date
#' @param endDate is the last global trade date
#' @param isTest prints additional info if TRUE (default is FALSE)
#'
#' @return a string of R Markdown code for the account's trades
#' @export
#'
generateAccountCode = function(tradesFile,TRADES,ac,curDate,WD,cap,startDate,endDate,isTest = FALSE) {
  title = "Option Day Trading Report"
  author = paste0("Account: ",ac)
  date = paste0("Date: ",curDate)
  txt = add_header(title,author,date)
  txt = paste(txt,add_prefix1(),sep = "\n")
  txt = paste(txt,add_prefix2(),sep = "\n")
  txt = paste(txt,add_prefix3(cap,tradesFile),sep = "\n")
  acStats = getStatistics(TRADES,WD,cap,startDate,endDate,level = 'global')
  if (isTest) {
    print(paste('Total statistics for account',ac))
    print(acStats)
    acTicStats = getTickerStatistics(TRADES)
    acTicStats = acTicStats[order(-acTicStats$nTrades),]
    acTimeSeries = getTimeSeries(TRADES,WD,cap,startDate,endDate,level = 'total')
    print(acTicStats)
    print(acTimeSeries)
  } #endif
  title1 = '<span  style="color: darkblue;">Performance Statistics </span>'
  txt = paste(txt, add_title_n(title1,2,tabset = T),sep = "\n")
  txt = paste(txt,add_title_n("Summary",3),sep = "\n")
  txt = paste(txt,add_general_statistics(cap,acStats),sep = "\n")
  txt = paste(txt,add_charts(ac,cap))
  txt = paste(txt,add_table())
  return(txt)
} # end generateAccountCode

#' Generate R Markdown code for yearly option day trading
#'
#' @param TRADES is the frame of trade data with the following fields:
#'               "accountId","entryDate","exitDate","underSymbol","symbol","entryPrice","exitPrice","amount",
#'               "entryCommis","exitCommis","pl","winner","capAtRisk","roi","tradeDur","year","month","day"
#' @param y is the number of the year (example: 2024)
#' @param WD is the frame of market working dates with the following fields:
#'           "dates","dayOfWeek","workDates"
#' @param value is the value of portfolio at the beginning of the year
#' @param startDate is the first global trade date
#' @param endDate is the last global trade date
#' @param isTest prints additional info if TRUE (default is FALSE)
#'
#' @return a string of R Markdown code for the yearly trades
#' @export
#'
generateYearCode = function(TRADES,y,WD,value,startDate,endDate,isTest = FALSE) {
  yStats = getStatistics(TRADES,WD,value,startDate,endDate,level = 'year')
  if (isTest) {
    print(paste('Yearly statistics for year',y))
    print(yStats)
    yTicStats = getTickerStatistics(TRADES)
    yTimeSeries = getTimeSeries(TRADES,WD,value,startDate,endDate,level = 'total')
    print(yTicStats)
    print(yTimeSeries)
  } #endif
  txt = add_title_n(y,3,tabset = T)
  txt = paste(txt,add_title_n("General Statistics",4),sep = "\n")
  txt = paste(txt,add_general_statistics(value,yStats),sep = "\n")
  txt = paste(txt,add_charts_year(y,value))
  txt = paste(txt,add_table_year())
  return(txt)
} # end generateYearCode

#' Generate R Markdown code for monthly option day trading
#'
#' @param TRADES is the frame of trade data with the following fields:
#'               "accountId","entryDate","exitDate","underSymbol","symbol","entryPrice","exitPrice","amount",
#'               "entryCommis","exitCommis","pl","winner","capAtRisk","roi","tradeDur","year","month","day"
#' @param m is the number of the month (1-12)
#' @param WD is the frame of market working dates with the following fields:
#'           "dates","dayOfWeek","workDates"
#' @param value is the value of portfolio at the beginning of the month
#' @param startDate is the first global trade date
#' @param endDate is the last global trade date
#' @param isTest prints additional info if TRUE (default is FALSE)
#'
#' @return a structure with two fields: $txt is R Markdown code and $value is portfolio value at the end of month
#' @export
#'
generateMonthCode = function(TRADES,m,WD,value,startDate,endDate,isTest = FALSE) {
  res = NULL
  monthName = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  TRADES = TRADES[order(TRADES$entryDate),]
  dStats = getStatistics(TRADES,WD,value,startDate,endDate,level = 'month')
  m = as.integer(m)
  if (isTest) {
    print(paste('Monthly statistics for month',m))
    print(dStats)
    dTicStats = getTickerStatistics(TRADES)
    dTimeSeries = getTimeSeries(TRADES,WD,value,startDate,endDate,level = 'daily')
    print(dTicStats)
    print(dTimeSeries)
  } #endif
  txt = add_title_n(monthName[m],4)
  txt = paste(txt,add_general_statistics(value,dStats),sep = "\n")
  txt = paste(txt,add_charts_month(m,value))
  txt = paste(txt,add_table_month())
  res$txt = txt
  res$value = value + dStats[3]
  return(res)
} # end generateMonthCode


### ================   E N D      F U N C T I O N S   =========================

