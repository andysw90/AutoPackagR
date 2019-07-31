removeBlanks <- function(inputstring){
  if(length(which(inputstring==""))){
    inputstring <- inputstring[-which(inputstring=="")]
  }
}

getFunctionLines <- function(fileContents){
  hasFunction_indx <- sapply(fileContents,function(x) length(grep(pattern = "\\(",x = x))>0)
  hasFunction <- fileContents[hasFunction_indx]
  hasFunction_func <- sapply(hasFunction,function(x) unlist(strsplit(x,split="\\("))[1])
  hasFunction_funcNoArrow <-
    sapply(hasFunction_func,function(x) {
      if(length(grep("<-",x))>0){
        return(tail(unlist(strsplit(x,split="<-")),n=1))
      }else{
        return(x)
      }
    })
  hasFunction_funcNoArrowNoSpecial <- sapply(hasFunction_funcNoArrow,function(x) {
    containsSpecial <- sapply(specialChars,function(thisChar) grep(x = x,pattern = thisChar))
    if(length(unlist(containsSpecial))>0){
      specials <- names(unlist(containsSpecial))
      return(tail(unlist(strsplit(x,split=specials)),n=1))
    }else{
      return(x)
    }
  })
  hasFunction_funcNoArrowNoSpecialNoSpace <- sapply(hasFunction_funcNoArrowNoSpecial,function(x) gsub(pattern = " ",replacement = "", x = x))
  return(hasFunction_funcNoArrowNoSpecialNoSpace)
}

AutoPackagR <- function(fname){
  specialChars <- c("\\{","\\}","\\[","\\]","<","-","%",">","\\+","\\\\","\\/","%","\\:")


  #Load functions from file
  fileContents <- readLines(fname)
  functions <- getFunctionLines(fileContents)

  #Load list of all, stock, installed functions
  #all
  allPackagesAndFunctions <- collidr::CRAN_packages_and_functions()

  #stock
  stockPackages <- sort(unique(c(ls("package:utils"),
                                 ls("package:base"),
                                 ls("package:graphics"),
                                 builtins(),
                                 "AutoPackagR")))

  #installed
  installedPackages <- installed.packages()[,1]
  installedPackages_FunctionsList <-
    lapply(installedPackages,
           function(x) allPackagesAndFunctions$function_names[which(allPackagesAndFunctions$package_names==x)])
  installedPackages_Functions <- unlist(installedPackages_FunctionsList)

  #Find what is installed, and what needs to be
  nonBaseFunctions <- setdiff(functions,stockPackages)
  requireInstallFunctions <- sapply(nonBaseFunctions,function(x) length(which(installedPackages_Functions==x)))


  requireLoadFunctions <- names(which(requireInstallFunctions==1))
  packagesToLoad <- sapply(requireLoadFunctions,function(x) installedPackages[grep(x,installedPackages_FunctionsList)])
  ## Add catcher if function maps to more than 1 installed package, ask user which


  requireInstallFunctions <- names(which(requireInstallFunctions==0))

  packagesToInstall <- lapply(requireInstallFunctions,function(x) allPackagesAndFunctions[which(allPackagesAndFunctions$function_names==x),])
  packageOutput <- rep("",length(packagesToInstall))
  for(i in 1:length(packagesToInstall)){
    nPackages <- dim(packagesToInstall[[i]])[1]

    if(nPackages==0){
      warning(paste0(requireInstallFunctions[i]," function not found in CRAN.\n"))
    }else{
      packages <- packagesToInstall[[i]]$package_names
      cat(paste0(requireInstallFunctions[i]," function found in ",nPackages," packages:\n",
                 paste0(1:length(packages),". ",packages,collapse="\n")))
      answer <- readline(prompt = "Enter package name or index:\n")
      if(answer==as.numeric(answer)){
        if(as.numeric(answer)<=length(packages)){
          packageOutput[i] <- packages[as.numeric(answer)]
        }else{
          answer <- readline(prompt = "Number of index invalid, try another index:\n")
          packageOutput[i] <- packages[as.numeric(answer)]
          if(is.null(packageOutput[i])){
            warning(paste0("PACKAGE NOT LOADED FOR FUNCTION ",nrequireInstallFunctions[i]))
          }
        }
      }else{
        matchedIndx <- which(packages==answer)
        if(length(matchedIndx)==0){
          answer <- readline(prompt = "Package name not found in list of possibilities, enter index or name:\n")
          matchedIndx <- which(packages==answer)
          if(length(matchedIndx)==0){
            warning(paste0("PACKAGE NOT LOADED FOR FUNCTION ",nrequireInstallFunctions[i]))
          }else{
            packageOutput[i] <- answer
          }
        }else{
          packageOutput[i] <- answer
        }
      }
    }
  }
  #add catcher if a blank one
  if(length(which(packageOutput==""))>0){
    packageOutput <- packageOutput[-which(packageOutput=="")]
  }

  packagesToLoad <- c(packagesToLoad,packageOutput)


  a <- sapply(packageOutput,function(x) install.packages(x))
  a <- sapply(packagesToLoad,function(x) library(x,character.only = TRUE))
  # installedPackages <- removeBlanks(names(a))

  cat("Packages installed: ",packageOutput)
  cat("Packages loaded: ",packagesToLoad)

}
