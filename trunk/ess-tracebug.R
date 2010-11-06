## R code used in the project




### DEBUG/UNDEBUG AT POINT
getDebugged <- function(env){
    env <- as.environment(env)
    allF <- ls(envir = env, all.names = T)
    isDeb <- unlist(lapply(allF,
                           function(obj) is.function(f <- get(obj, envir = env)) && isdebugged(f)
                           ))
    allF[isDeb]
}

getGenerics()
str(get("plot"))

methods:::.showMethodsTable
is(m, "MethodDefinition")
