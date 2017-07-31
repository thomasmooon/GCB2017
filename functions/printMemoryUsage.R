printMemoryUsage <- function() {
x <- qdapRegex::rm_white(system("free",intern = T))
ram.total <- as.numeric(unlist(strsplit(trimws(x[2])," "))[2] )
ram.used <- as.numeric(unlist(strsplit(trimws(x[2])," "))[3])
ram.used <- paste(round(ram.used/ram.total*100,0),"% RAM used")
print(ram.used)
}

