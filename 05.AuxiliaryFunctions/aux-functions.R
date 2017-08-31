# FUNCTION TO FIND THE PATH OF THE FOOD SECURITY REPOSITORY ---------------  
get_path <- function() {
  full_path <- getwd()
  path <- sub("^(.*)/.*", "\\1", full_path)
  return(path)
}

# FUNCTION THAT COMPARE DATASETS GIVEN AN ID KEY --------------------------  
db_compare <- function(path_data, by = "registro", file = "compare.txt"){
  ls_tables <- list.files(path_data, "\\.csv$") # listing all csv files
  by <- tolower(by)
  # file <- file.path(path_data, file)
  write("------------------------------------------------", file)
  write("Summary of csv files inside the provided folder.", file, app = T)
  write("------------------------------------------------", file, app = T)
  for(i in 1:length(ls_tables)){
    write(paste0("\n", i, ") file: ", ls_tables[i]), file, app = T)
    data <- read.csv(file.path(path_data, ls_tables[i]), stringsAsFactors = F)
    names(data) <- tolower(names(data))
    write(paste0("Dimension: ", nrow(data), " rows by ",
                 ncol(data), " columns"), file, app = T)
    write(paste0("Number of unique ", by, ": ", 
                length(unique(data$registro))), file, app = T)
    write(paste0("Class of ", by, ": ", class(data$registro)), file, app = T)
  }
}

# FUNCTION TO OBTAIN UNIQUE VALUES OF A VARIABLE --------------------------  
uniquen <- function(var, n = 20) {
  un <- unique(var)
  out <- NA
  if (length(un) <= n) out <- paste(un, collapse = ",")
  return(out)
}

# FUNCTION TO OBTAIN THE PERCENTAGE OF MISSING VALUES ---------------------  
na_per <- function(x) 100 * sum(is.na(x)) / length(x)

# FUNCTION TO CREATE A METADATA OF A DATABASE -----------------------------
db_summarize <- function(data, filename = "metadata.csv") {
  metadata <-
    data.frame(variables = names(data),
               typeof = sapply(data, typeof),
               class = sapply(data, function(x) class(x)[1]),
               n_unique = sapply(data, function(x) length(unique(x))),
               unique = sapply(data, uniquen),
               na_per = sapply(data, na_per))
  write.table(metadata, filename, sep = ",", row.names = FALSE)
}

# FUNCTION STO SHOW THE INDEX A A PARTICULAR VALUE IN A VARIABLE
var_index <- function(data, varname, value) {


}

git_path <- function() {
  system('git rev-parse --show-toplevel', intern = TRUE)
}
