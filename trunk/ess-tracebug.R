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

###_ watches:
.ess_watch_expressions <- list(a = parse(text = "343 + 4"),
    parse(text = "sdfsf + 33"),
    parse(text = "str(iris)"))

if(!exists(".ess_watch_expressions"){
    cat("Can not find watch expressions. \nHave you defined a watch?")
}else{
    if(!exists(".ess_watch_execute")){
        .ess_watch_execute <- function(){
            .essWEnames <- allNames(.ess_watch_expressions)
            len0p <- !nzchar(.essWEnames)
            .essWEnames[len0p] <- seq_along(len0p)[len0p]
            for(i in seq_along(.ess_watch_expressions)){
                cat("\n@-- ", .essWEnames[[i]], " ", rep.int("-", max(0, 30 - length(.essWEnames[[i]]))), "@\n", sep = "")
                cat( paste("@--", deparse(.ess_watch_expressions[[i]][[1]])), " \n", sep = "")
                tryCatch(print(eval(.ess_watch_expressions[[i]])),
                         error = function(e) cat("Error:", e$message, "\n" ),
                         warning = function(w) cat("warning: ", w$message, "\n"))
            }
        }
    }
    .ess_watch_execute()
}



