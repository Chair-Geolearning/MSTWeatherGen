init_monitoring <- function(names) {
  log_file <- "monitoring_aii_nuii_spatial.txt"
  writeLines(
    paste(c("label", paste0("aii_", names), paste0("nuii_", names)), collapse=";"),
    log_file
  )
  assign(".log_file", log_file, envir=.GlobalEnv)
  assign(".log_iter", 0L,       envir=.GlobalEnv)
  
  log_file_st <- "monitoring_abcde_Ai_st.txt"
  writeLines(
    paste(c("label", "a","b","c","d","e", paste0("Ai_", names)), collapse=";"),
    log_file_st
  )
  assign(".log_file_st", log_file_st, envir=.GlobalEnv)
  assign(".log_iter_st", 0L,          envir=.GlobalEnv)
  
  log_file_temp <- "monitoring_r1ii_r2ii_temporal.txt"
  writeLines(
    paste(c("label", paste0("r1ii_", names), paste0("r2ii_", names)), collapse=";"),
    log_file_temp
  )
  assign(".log_file_temp", log_file_temp, envir=.GlobalEnv)
  assign(".log_iter_temp", 0L,            envir=.GlobalEnv)
  
  return(list(
    log_file      = log_file,
    log_file_st   = log_file_st,
    log_file_temp = log_file_temp
  ))
}

stop_monitoring <- function() {
  assign(".log_file",      NULL, envir=.GlobalEnv)
  assign(".log_file_st",   NULL, envir=.GlobalEnv)
  assign(".log_file_temp", NULL, envir=.GlobalEnv)
}

log_params <- function(par_all, names, label, log_file, log_file_st, log_file_temp, which = c("spatial", "st", "temp")) {
  
  
  # aii et nuii
  if ("spatial" %in% which) {
    
    aii_names  <- paste(paste(names, names, sep="-"), "aii",  sep=":")
    nuii_names <- paste(paste(names, names, sep="-"), "nuii", sep=":")
    line <- paste(c(label,
                    round(par_all[aii_names],  6),
                    round(par_all[nuii_names], 6)),
                  collapse=";")
    write(line, file=log_file, append=TRUE)
  }
  
  # a,b,c,d,e et Ai
  if ("st" %in% which) {
    
    Ai_names <- paste(names, "Ai", sep=":")
    line <- paste(c(label,
                    round(par_all[c("a","b","c","d","e")], 6),
                    round(par_all[Ai_names], 6)),
                  collapse=";")
    write(line, file=log_file_st, append=TRUE)
  }
  
  # r1ii et r2ii
  if ("temp" %in% which) {
    
    line <- paste(c(label,
                    round(par_all[paste(names, "r1ii", sep=":")], 6),
                    round(par_all[paste(names, "r2ii", sep=":")], 6)),
                  collapse=";")
    write(line, file=log_file_temp, append=TRUE)
  }
}

write_loglik_monitoring <- function(par, parms, names) {
  
  # Spatio — aii et nuii
  aii_names  <- paste(paste(names, names, sep="-"), "aii",  sep=":")
  nuii_names <- paste(paste(names, names, sep="-"), "nuii", sep=":")
  
  if (any(aii_names %in% parms)) {
    log_f <- tryCatch(get(".log_file", envir=.GlobalEnv), error=function(e) NULL)
    if (!is.null(log_f) && is.character(log_f)) {
      iter <- tryCatch(get(".log_iter", envir=.GlobalEnv), error=function(e) 0L) + 1L
      assign(".log_iter", iter, envir=.GlobalEnv)
      line <- paste(c(
        paste0("iter_", iter),
        round(par[parms %in% aii_names],  6),
        round(par[parms %in% nuii_names], 6)
      ), collapse=";")
      tryCatch(write(line, file=log_f, append=TRUE), error=function(e) NULL)
    }
  }
  
  # Spatio Temp — a,b,c,d,e et Ai
  abcde_names <- c("a", "b", "c", "d", "e")
  Ai_names    <- paste(names, "Ai", sep=":")
  
  if (any(abcde_names %in% parms)) {
    log_f <- tryCatch(get(".log_file_st", envir=.GlobalEnv), error=function(e) NULL)
    if (!is.null(log_f) && is.character(log_f)) {
      iter <- tryCatch(get(".log_iter_st", envir=.GlobalEnv), error=function(e) 0L) + 1L
      assign(".log_iter_st", iter, envir=.GlobalEnv)
      line <- paste(c(
        paste0("iter_", iter),
        round(par[parms %in% abcde_names], 6),
        round(par[parms %in% Ai_names],    6)
      ), collapse=";")
      tryCatch(write(line, file=log_f, append=TRUE), error=function(e) NULL)
    }
  }
  
  # Temp — r1ii et r2ii
  r1ii_names <- paste(names, "r1ii", sep=":")
  r2ii_names <- paste(names, "r2ii", sep=":")
  
  if (any(r1ii_names %in% parms)) {
    log_f <- tryCatch(get(".log_file_temp", envir=.GlobalEnv), error=function(e) NULL)
    if (!is.null(log_f) && is.character(log_f)) {
      iter <- tryCatch(get(".log_iter_temp", envir=.GlobalEnv), error=function(e) 0L) + 1L
      assign(".log_iter_temp", iter, envir=.GlobalEnv)
      line <- paste(c(
        paste0("iter_", iter),
        round(par[parms %in% r1ii_names], 6),
        round(par[parms %in% r2ii_names], 6)
      ), collapse=";")
      tryCatch(write(line, file=log_f, append=TRUE), error=function(e) NULL)
    }
  }
}


