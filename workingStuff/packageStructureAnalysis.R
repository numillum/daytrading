# ====================================================================================
# packageStructureAnalysis.r processes an R package and creates its function hierarchy
#                            Trading tools project
# ------------------------------------------------------------------------------------
# Programmer : Valery Petrushin
# Creation Date: February 14, 2025
# Modification Date: February 14, 2025
# ====================================================================================
# Link to pkgnet usage https://uptake.github.io/pkgnet/articles/pkgnet-intro.html
library(pkgnet)
pkgName = "daytrading"  # the package must be installed

buildHierarchy = function(fnm,fDepNames,fDEP,level = '    ') {
  # print(paste("fname:",paste(fnm,collapse = ',')))
  if (fnm %in% fDepNames) {
    dfnm = fDEP[fDEP$fNames == fnm,'fDepend'][[1]]
    # print(paste("dependent fnames:",paste(dfnm,collapse = ',')))
    txt = fnm
    for (i in 1:length(dfnm)) {
      nm = dfnm[i]
      # print(paste("name:",nm))
      txt = paste(txt,paste0(level,buildHierarchy(nm,fDepNames,fDEP,paste0(level,'    '))),sep='\n')
    } #end for i
  } else {
    # print(paste("returns:",fnm))
    return(fnm)
  } #endif
  return(txt)
} #end buildHierarchy

# == Get package structure report
pkgReport = CreatePackageReport(pkg_name = pkgName)
# == Get nodes info
funcNodes = pkgReport$FunctionReporter$nodes
# == Get dependencies
fGraph = pkgReport$FunctionReporter$pkg_graph
fEdges = fGraph$edges
uFrom = unique(fEdges$SOURCE)
fCount = sort(table(fEdges$SOURCE),decreasing = T)

fNames = attr(fCount,"dimnames")[[1]]
fDepend = {}
for (k in 1:length(fNames)) {
  nm = fNames[k]
  fDeps = fEdges[fEdges$SOURCE == nm,]
  dList = {}
  for (dnm in fDeps$TARGET) {
    dList = c(dList,dnm)
  } # end for dnm
  fDepend[[k]] = dList
} # end for nm
fDEP = data.frame(cbind(fNames,fDepend))

# == Get hierarchy for top level functions
topFunc = unlist(funcNodes[(funcNodes$outDegree > 0) & (funcNodes$inDegree == 0),'node'])
fDepNames = unlist(fDEP$fNames)
fHierarchy = ''
for (fnm in topFunc) {
  fHierarchy = paste(fHierarchy,buildHierarchy(fnm,fDepNames,fDEP),sep='\n')
} # end for fnm
cat(fHierarchy)

# == Save hierarchy to a text file
outFile = paste0(pkgName,"Hierarchy.txt")
sink(outFile)
cat(fHierarchy)
sink()

# == Save nodes info
outFile = paste0(pkgName,"FunctionInfo.csv")
write.csv(funcNodes,outFile,row.names = F)
