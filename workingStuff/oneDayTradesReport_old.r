# =============================================================================
# oneDayTradesReport.r processes one-day option trade data
#                      with TDAm API using R package
#                      TDAmeritrade tools project
# -----------------------------------------------------------------------------
# Programmer : Valery Petrushin
# Creation Date: October 12, 2023
# Modification Date: October 16, 2023
# =============================================================================

library(rameritrade)

### ===================    M A I N.    P R O G R A M   ========================
cap = 75289.97  # initial capital
curDate = format(Sys.Date(),format = "%Y-%m-%d")  # date in a format yyyy-mm-dd

# == 1 == Create folder for transactions and reports
wDir = paste0(getwd(),"/",curDate)
if (!dir.exists(wDir)) {dir.create(wDir)} #endif
transFile = paste0(wDir,"/Transactions_",curDate,".csv")

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
uAcc = unique(TRADES$accountId)
for (ac in uAcc) {
  TRADESAC = TRADES[TRADES$accountId == ac,]
  REPORT = generateOneDayReport(TRADESAC)
  repFile = paste0(wDir,"/Report_",ac,"_",curDate,".csv")
  write.csv(REPORT,repFile,row.names = F)
  STATS = getTradingStatistics(TRADESAC,cap)
  statFile = paste0(wDir,"/Statistics_",ac,"_",curDate,".csv")
  write.csv(STATS,statFile,row.names = F)
} # end for ac


