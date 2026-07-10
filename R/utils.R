## small shared helpers, no adapter or backend dependency.

# null-coalesce; used for ctx$params defaults in tool files
`%||%` <- function(a, b) if (is.null(a)) b else a
