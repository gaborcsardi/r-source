#  File src/library/base/R/methodsSupport.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

trace <- function(what, tracer, exit, at, print, signature,
                  where = topenv(parent.frame()), edit = FALSE)
{
    if(nargs() > 1L && !.isMethodsDispatchOn()) {
        ns <- try(loadNamespace("methods"))
        if(isNamespace(ns))
            message("(loaded the methods namespace)", domain = NA)
        else ## (should not be possible)
            stop("tracing functions requires the 'methods' package, but unable to load the 'methods' namespace")
    }
    else if(nargs() == 1L)
        return(.primTrace(what))
    tState <- tracingState(FALSE)
    on.exit(tracingState(tState))
    ## now call the version in the methods package, to ensure we get
    ## the correct namespace (e.g., correct version of class())
    call <- sys.call()
    call[[1L]] <- quote(methods:::.TraceWithMethods)
					# -> ../../methods/R/trace.R
    call$where <- where
    eval.parent(call)
}

untrace <- function(what, signature = NULL, where = topenv(parent.frame())) {
    if(!.isMethodsDispatchOn()) ## can't have called trace except in primitive form
        return(.primUntrace(what))
    ## at this point we can believe that the methods namespace was successfully loaded
    tState <- tracingState(FALSE)
    on.exit(tracingState(tState))
    ## now call the version in the methods package, to ensure we get
    ## the correct namespace (e.g., correct version of class())
    call <- sys.call()
    call[[1L]] <- quote(methods:::.TraceWithMethods)
    call$where <- where
    call$untrace <- TRUE
    invisible(eval.parent(call))
}


tracingState <- function(on = NULL) .Internal(traceOnOff(on))


asS4 <- function(object, flag = TRUE, complete = TRUE)
    .Internal(setS4Object(object, flag, complete))

asS3 <- function(object, flag = TRUE, complete = TRUE)
    .Internal(setS4Object(object, !as.logical(flag), complete))


.doTrace <- function(expr, msg) {
    on <- tracingState(FALSE)	   # turn it off QUICKLY (via a .Internal)
    if(on) {
	on.exit(tracingState(TRUE)) # restore on exit, keep off during trace
	if(!missing(msg)) {
	    call <- deparse(sys.call(sys.parent(1L)))
	    if(length(call) > 1L)
		call <- paste(call[[1L]], "....")
	    cat("Tracing", call, msg, "\n")
	}
	exprObj <- substitute(expr)
	eval.parent(exprObj)
    }
    NULL
}

returnValue <- function(default = NULL) .Internal(returnValue(default))
