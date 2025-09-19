# Get the names of the most common arguments in a package
rm(list = ls(all.names = TRUE))

PKG_NAME <- "pipenostics"

package_object_names <- ls(paste("package", PKG_NAME, sep = ":"))

package_function_arguments_0 <- sapply(
  package_object_names
  ,
  \(ObjName){
    x <- get(ObjName)
    y <- if(is.function(x)) capture.output({print(args(x))}) else character(1)
    paste(y, collapse = " ")
  }
  ,
  USE.NAMES = TRUE
)

package_function_arguments_1 <- paste(package_function_arguments_0, collapse = "\n")

package_function_arguments_2 <- package_function_arguments_1
for (p in c("function", "NULL", "(", ")", ",", "=", "pipenostics::", "stats::")){
  package_function_arguments_2 <- trimws(gsub(p, " ", package_function_arguments_2, fixed = TRUE))
}

package_function_arguments_3 <- strsplit(
  package_function_arguments_2, " ", fixed = TRUE
)[[1]]

package_function_arguments_4 <- package_function_arguments_3[
  !(package_function_arguments_3 %in% c(
    "", "\n", "\n\n", "\n\n\n", "TRUE", "FALSE", "NA_real_")
  )
]

package_function_arguments_5 <- package_function_arguments_4[
  !grepl("^[0-9]|\"", package_function_arguments_4)
]

package_function_arguments_rating <- sort(
  table(package_function_arguments_5), decreasing = TRUE
)
