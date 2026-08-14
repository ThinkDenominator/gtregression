# Helpers kept tiny & CRAN-safe
ns_gt   <- function() asNamespace("gtregression")
has_fn  <- function(name) isTRUE(name %in% getNamespaceExports("gtregression"))

# Call first available exported fn from candidates, keeping only supported args
call_gt <- function(candidates, ...) {
  args <- list(...)
  nm <- Filter(has_fn, candidates)
  if (!length(nm)) stop("None of these functions are exported by gtregression: ",
                        paste(candidates, collapse = ", "))
  fn  <- get(nm[[1]], envir = ns_gt())
  keep <- intersect(names(args), names(formals(fn)))
  do.call(fn, args[keep])
}

as_text <- function(x) paste(utils::capture.output(print(x)), collapse = "\n")
