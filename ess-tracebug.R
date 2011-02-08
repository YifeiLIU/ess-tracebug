## R code used in the project




### DEBUG/UNDEBUG AT POINT

## length(tf <- apropos(".", mode = "function"))
## length(tg <- getGenerics())
## sum(tg %in% tf) ## all generics are present as function in the vector returnde by apropos
## findMethods("+")
## setClass("FOO", contains="character")
## setMethod("+", c("FOO", "character"), function(e1,e2) paste(e1,e2,collapse=""))
## str(findMethods("+"))
## showMethods("+")
## str(findMethods("+"))
## isGeneric("sin")
## findMethodSignatures("sin")
## getGeneric("sin")
## te <- getMethodsForDispatch(getGeneric("lu"))

local({
    .ess_dbg_getTracedAndDebugged <- function(){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        generics <- methods::getGenerics()
        out <- c()
        for(i in seq_along(generics)){
            menv <- methods::getMethodsForDispatch(methods::getGeneric(generics[[i]], package=generics@package[[i]]))
            traced <- unlist(eapply(menv, is, 'traceable', all.names=TRUE))
            if(length(traced) && any(traced))
                out <- c(paste(generics[[i]],':', names(traced)[traced],sep=''), out)
            if(is(getFunction(generics[[i]], where = .GlobalEnv),  'traceable')) # if the default is traced,  it does not appear in the menv
                out <- c(generics[[i]], out)
        }
        debugged <- apropos('.', mode = 'function')
        ## traced function don't appear here. Not realy needed and would affect performance.
        debugged <- debugged[which(unlist(lapply(debugged, isdebugged) , recursive=FALSE, use.names=FALSE))]
        c(debugged, out)
    }
    .ess_dbg_UntraceOrUndebug <- function(name){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        ## name is a name of a function to be undebugged or has a form name:Class1#Class2#Class3 for traced methods
        name <- strsplit(name, ':', fixed = TRUE)[[1]]
        if(length(name)>1){
            ## a method
            fun <- name[[1]]
            sig <- strsplit(paste(name[-1], collapse=''), '#', fixed=TRUE)[[1]]
            untrace(fun, signature = sig)
        }else{
            ## function
            if(is(getFunction(name), 'traceable'))
                untrace(name)
            else
                undebug(name)
        }
    }
    .ess_dbg_UndebugALL <- function(funcs){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        invisible(lapply(funcs, .ess_dbg_UntraceOrUndebug))
    }
    environment(.ess_dbg_UndebugALL) <-
        environment(.ess_dbg_UntraceOrUndebug) <-
            environment(.ess_dbg_getTracedAndDebugged) <- .GlobalEnv  ## to see all the funcs
    assign('.ess_dbg_getTracedAndDebugged', .ess_dbg_getTracedAndDebugged, envir= .BaseNamespaceEnv)
    assign('.ess_dbg_UntraceOrUndebug', .ess_dbg_UntraceOrUndebug, envir= .BaseNamespaceEnv)
    assign('.ess_dbg_UndebugALL', .ess_dbg_UndebugALL, envir= .BaseNamespaceEnv)
})








setGeneric("blabla", function(a=34,b=343) standardGeneric("blabla"))

methods:::.showMethodsTable()
sapply(tf,)

isGeneric("plot")
is(m, "MethodDefinition")

###_ WATCHES:
.ess_watch_expressions <- list()
.ess_watch_expressions <- list(a = parse(text = "343 + 4"),
    parse(text = "sdfsf + 33"),
    parse(text = "str(iris)"))

assign(".ess_watch_eval", function(){
    if(!exists(".ess_watch_expressions")){
        assign(".ess_watch_expressions", list(), envir = .GlobalEnv)
    }
    if(length(.ess_watch_expressions) == 0L){
        cat("\n# Watch list is empty!\n
# a/i     append/insert new expression
# k       kill
# e       edit the expression
# r       rename
# n/p     navigate
# u/U     move the expression up/down
# q       kill the buffer
")
    }else{
        .essWEnames <- allNames(.ess_watch_expressions)
        len0p <- !nzchar(.essWEnames)
        .essWEnames[len0p] <- seq_along(len0p)[len0p]
        for(i in seq_along(.ess_watch_expressions)){
            cat("\n@---- ", .essWEnames[[i]], " ", rep.int("-", max(0, 30 - nchar(.essWEnames[[i]]))), "@\n", sep = "")
            cat( paste("@--->", deparse(.ess_watch_expressions[[i]][[1L]])), " \n", sep = "")
            tryCatch(print(eval(.ess_watch_expressions[[i]])),
                     error = function(e) cat("Error:", e$message, "\n" ),
                     warning = function(w) cat("warning: ", w$message, "\n"))
        }}
}, envir = .GlobalEnv)



function (what, tracer = NULL, exit = NULL, at = numeric(), print = TRUE,
          signature = NULL, where = .GlobalEnv, edit = FALSE, from = NULL,
          untrace = FALSE)
{
    if (is.function(where)) {
        if (is(where, "genericFunction"))
            where <- parent.env(environment(where))
        else where <- environment(where)
        fromPackage <- getPackageName(where)
    }
    else fromPackage <- ""
    doEdit <- !identical(edit, FALSE)
    whereF <- NULL
    pname <- character()
    def <- NULL
    if (is.function(what)) {
        def <- what
        if (is(def, "genericFunction")) {
            what <- def@generic
            whereF <- .genEnv(what, where)
            pname <- def@package
        }
        else {
            fname <- substitute(what)
            if (is.name(fname)) {
                what <- as.character(fname)
                temp <- .findFunEnvAndName(what, where)
                whereF <- temp$whereF
                pname <- temp$pname
            }
            else if (is.call(fname) && identical(fname[[1L]],
                                                 as.name("::"))) {
                whereF <- as.character(fname[[2L]])
                require(whereF, character.only = TRUE)
                whereF <- as.environment(paste("package", whereF,
                                               sep = ":"))
                pname <- fname[[2L]]
                what <- as.character(fname[[3L]])
            }
            else if (is.call(fname) && identical(fname[[1L]],
                                                 as.name(":::"))) {
                pname <- paste(fname[[2L]], "(not-exported)")
                whereF <- loadNamespace(as.character(fname[[2L]]))
                what <- as.character(fname[[3L]])
            }
            else stop("argument 'what' should be the name of a function")
        }
    }
    else {
        what <- as(what, "character")
        if (length(what) != 1) {
            for (f in what) {
                if (nargs() == 1)
                    trace(f)
                else Recall(f, tracer, exit, at, print, signature,
                            where, edit, from, untrace)
            }
            return(what)
        }
        temp <- .findFunEnvAndName(what, where, signature)
        whereF <- temp$whereF
        pname <- temp$pname
    }
    if (what %in% .InvalidTracedFunctions)
        stop(gettextf("Tracing the internal function \"%s\" is not allowed",
                      what))
    if (.traceTraceState) {
        message(".TraceWithMethods: after computing what, whereF")
        browser()
    }
    if (nargs() == 1)
        return(.primTrace(what))
    if (is.null(whereF)) {
        allWhere <- findFunction(what, where = where)
        if (length(allWhere) == 0)
            stop(gettextf("no function definition for \"%s\" found",
                          what), domain = NA)
        whereF <- as.environment(allWhere[[1L]])
    }
    if (is.null(tracer) && is.null(exit) && identical(edit, FALSE))
        tracer <- quote({
        })
    if (is.null(def))
        def <- getFunction(what, where = whereF)
    if (is(def, "traceable") && identical(edit, FALSE) && !untrace)
        def <- .untracedFunction(def)
    if (!is.null(signature)) {
        fdef <- if (is.primitive(def))
            getGeneric(what, TRUE, where)
        else def
        def <- selectMethod(what, signature, fdef = fdef, optional = TRUE)
        if (is.null(def)) {
            warning(gettextf("Can't untrace method for \"%s\"; no method defined for this signature: %s",
                             what, paste(signature, collapse = ", ")))
            return(def)
        }
    }
    if (untrace) {
        if (.traceTraceState) {
            message(".TraceWithMethods: untrace case")
            browser()
        }
        if (is.null(signature)) {
            if (is(def, "traceable")) {
                newFun <- .untracedFunction(def)
            }
            else {
                .primUntrace(what)
                return(what)
            }
        }
        else {
            if (is(def, "traceable"))
                newFun <- .untracedFunction(def)
            else {
                warning(gettextf("the method for \"%s\" for this signature was not being traced",
                                 what), domain = NA)
                return(what)
            }
        }
    }
    else {
        if (!is.null(exit)) {
            if (is.function(exit)) {
                tname <- substitute(exit)
                if (is.name(tname))
                    exit <- tname
                exit <- substitute(TRACE(), list(TRACE = exit))
            }
        }
        if (!is.null(tracer)) {
            if (is.function(tracer)) {
                tname <- substitute(tracer)
                if (is.name(tname))
                    tracer <- tname
                tracer <- substitute(TRACE(), list(TRACE = tracer))
            }
        }
        original <- .untracedFunction(def)
        traceClass <- .traceClassName(class(original))
        if (is.null(getClassDef(traceClass)))
            traceClass <- .makeTraceClass(traceClass, class(original))
        if (doEdit && is.environment(edit)) {
            def <- .findNewDefForTrace(what, signature, edit,
                                       fromPackage)
            environment(def) <- environment(original)
            if (is.null(c(tracer, exit))) {
                newFun <- new(traceClass, original)
                newFun@.Data <- def
            }
            else {
                newFun <- new(traceClass, def = def, tracer = tracer,
                              exit = exit, at = at, print = print, doEdit = FALSE)
                newFun@original <- original
            }
            newFun@source <- edit
        }
        else newFun <- new(traceClass, def = if (doEdit)
                           def
        else original, tracer = tracer, exit = exit, at = at,
                           print = print, doEdit = edit)
    }
    global <- identical(whereF, .GlobalEnv)
    if (.traceTraceState) {
        message(".TraceWithMethods: about to assign or setMethod")
        browser()
    }
    if (is.null(signature)) {
        if (bindingIsLocked(what, whereF))
            .assignOverBinding(what, newFun, whereF, global)
        else assign(what, newFun, whereF)
        if (length(grep("[^.]+[.][^.]+", what)) > 0) {
            S3MTableName <- ".__S3MethodsTable__."
            tracedFun <- get(what, envir = whereF, inherits = TRUE)
            if (exists(S3MTableName, envir = whereF, inherits = FALSE)) {
                tbl <- get(S3MTableName, envir = whereF, inherits = FALSE)
                if (exists(what, envir = tbl, inherits = FALSE))
                    assign(what, tracedFun, envir = tbl)
            }
        }
    }
    else {
        if (untrace && is(newFun, "MethodDefinition") && !identical(newFun@target,
                                                                    newFun@defined))
            newFun <- NULL
        setMethod(fdef, signature, newFun, where = baseenv())
    }
    if (!global) {
        action <- if (untrace)
            "Untracing"
        else "Tracing"
        nameSpaceCase <- FALSE
        location <- if (.identC(fromPackage, "")) {
            if (length(pname) == 0 && !is.null(whereF))
                pname <- getPackageName(whereF)
            nameSpaceCase <- isNamespace(whereF) && !is.na(match(pname,
                                                                 loadedNamespaces())) && identical(whereF, getNamespace(pname))
            if (length(pname) == 0)
                "\""
            else {
                if (nameSpaceCase)
                    paste("\" in environment <namespace:", pname,
                          ">", sep = "")
                else paste("\" in package \"", pname, "\"", sep = "")
            }
        }
        else paste("\" as seen from package \"", fromPackage,
                   "\"", sep = "")
        object <- if (is.null(signature))
            " function \""
        else " specified method for function \""
        .message(action, object, what, location)
        if (nameSpaceCase && !untrace && exists(what, envir = .GlobalEnv)) {
            untcall <- paste("untrace(\"", what, "\", where = getNamespace(\"",
                             pname, "\"))", sep = "")
            .message("Warning: Tracing only in the namespace; to untrace you will need:\n    ",
                     untcall, "\n")
        }
    }
    what
}
