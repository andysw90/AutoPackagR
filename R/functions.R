removeBlanks <- function(inputstring){
  if(length(which(inputstring==""))){
    inputstring <- inputstring[-which(inputstring=="")]
  }
}

AutoPackagR <- function(fname){
  specialChars <- c("\\{","\\}","\\[","\\]","<","-","%",">","\\+","\\\\","\\/","%","\\:")
  allPackagesAndFunctions <- collidr::CRAN_packages_and_functions()

  fileContents <- readLines(fname)

  hasFunction_indx <- sapply(fileContents,function(x) length(grep(pattern = "\\(",x = x)))
  hasFunction <- fileContents[as.logical(hasFunction_indx)]
  hasFunction_pre <- sapply(hasFunction,function(x) unlist(strsplit(x,split="\\("))[1])
  hasFunction_pre2 <-
    sapply(hasFunction_pre,function(x) {
      if(length(grep("<-",x))>0){
        return(tail(unlist(strsplit(x,split="<-")),n=1))
      }else{
        return(x)
      }
    })
  hasFunction_pre3 <- sapply(hasFunction_pre2,function(x) {
    containsSpecial <- sapply(specialChars,function(thisChar) grep(x = x,pattern = thisChar))
    if(length(unlist(containsSpecial))>0){
      specials <- names(unlist(containsSpecial))
      return(tail(unlist(strsplit(x,split=specials)),n=1))
    }else{
      return(x)
    }
  })
  hasFunction_pre4 <- sapply(hasFunction_pre3,function(x) gsub(pattern = " ",replacement = "", x = x))

  stockPackages <- sort(unique(c(ls("package:utils"),
                                 ls("package:base"),
                                 ls("package:graphics"),
                                 builtins(),
                                 "AutoPackagR")))
  nonBaseFunctions <- setdiff(hasFunction_pre4,stockPackages)

  installedPackages <- installed.packages()[,1]
  installedPackages_FunctionsList <- lapply(installedPackages,function(x) allPackagesAndFunctions$function_names[which(allPackagesAndFunctions$package_names==x)])
  installedPackages_Functions <- unlist(installedPackages_FunctionsList)


  notAvailableFunctions <- sapply(nonBaseFunctions,function(x) length(which(installedPackages_Functions==x)))
  availableFunctions <- names(which(notAvailableFunctions==1))
  packagesToLoad <- sapply(availableFunctions,function(x) installedPackages[grep(x,installedPackages_FunctionsList)])
  ## Add catcher if function maps to more than 1 package, ask user which
  l <- lapply(unlist(packagesToLoad),library,character.only = TRUE)

  notAvailableFunctions <- names(which(notAvailableFunctions==0))

  packagesToInstall <- lapply(notAvailableFunctions,function(x) allPackagesAndFunctions[which(allPackagesAndFunctions$function_names==x),])
  packageOutput <- rep("",length(packagesToInstall))
  for(i in 1:length(packagesToInstall)){
    nPackages <- dim(packagesToInstall[[i]])[1]

    if(nPackages==0){
      warning(paste0(notAvailableFunctions[i]," function not found in CRAN.\n"))
    }else{
      packages <- packagesToInstall[[i]]$package_names
      cat(paste0(notAvailableFunctions[i]," function found in ",nPackages," packages:\n",
                 paste0(1:length(packages),". ",packages,collapse="\n")))
      answer <- readline(prompt = "Enter package name or index:\n")
      if(answer==as.numeric(answer)){
        if(as.numeric(answer)<=length(packages)){
          packageOutput[i] <- packages[as.numeric(answer)]
        }else{
          answer <- readline(prompt = "Number of index invalid, try another index:\n")
          packageOutput[i] <- packages[as.numeric(answer)]
          if(is.null(packageOutput[i])){
            warning(paste0("PACKAGE NOT LOADED FOR FUNCTION ",nnotAvailableFunctions[i]))
          }
        }
      }else{
        matchedIndx <- which(packages==answer)
        if(length(matchedIndx)==0){
          answer <- readline(prompt = "Package name not found in list of possibilities, enter index or name:\n")
          matchedIndx <- which(packages==answer)
          if(length(matchedIndx)==0){
            warning(paste0("PACKAGE NOT LOADED FOR FUNCTION ",nnotAvailableFunctions[i]))
          }else{
            packageOutput[i] <- answer
          }
        }else{
          packageOutput[i] <- answer
        }
      }
    }
  }
  packageOutput <- removeBlanks(packageOutput)


  a <- sapply(packageOutput,function(x) install.packages(x))
  installedPackages <- removeBlanks(names(a))

  installedPackages

}
