x <- combineMplusOutput::combineMplusOutput(mplusPath = "~/Desktop/sdpmplus/output/",
						mplusFileType = ".*.out", recurse = TRUE,
						returnType = "character", parallel = TRUE)


setwd("~/Desktop/sdpmplus/output/")

writeLines(x, con = "allOutputCombined.txt")

modFit <- grep("(.*FILE IS ~/Desktop/sdpmplus/output.*)|(.*Akaike .*[0-9]$)|(.*Bayesian .*[0-9]$)|(.*Sample-Size Adjusted .*[0-9]$)|(.*H0 Value .*[0-9]$)", x, value = TRUE)

v <- as.data.frame(modFit, stringsAsFactors = FALSE)
