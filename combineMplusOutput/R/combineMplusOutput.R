#' @title combineMplusOutput
#' @description Simple package to combine the output from multiple Mplus runs
#' located within a single root directory (with or without subdirectories)
#' @param mplusPath file path to the mplus output files
#' @param mplusFileType Character string identifying the file type which Mplus
#' should attempt to read into memory
#' @param recurse A boolean indicating whether or not the function should search
#' recursively from the top-level root directory passed to the mplusPath argument
#' @param returnType A character string identifying the type of object to be
#' returned to the end user
#' @param parallel A boolean indicating whether the package should load the doMC
#' package and process the files in parallel
#' @return An object containing the output based on the input arguments/parameters
#' @note
#' List objects may require slightly less memory compared to character class objects.
#' When developing the function, the character object was ~ 51MB while the list object
#' of the same data required only 47.9MB.
#'
#' The graphics data - for 32 files with ~ 40k records each - was roughly .66GB.
#' @importFrom doMC registerDoMC
#' @importFrom plyr llply
#' @importFrom rhdf5 h5dump H5close
#' @examples \donttest{
#' # Create an object with the root directory where output is stored
#' rootDir <- "~/Desktop/sdpmplus/output/"
#'
#' # Define the file types to combine
#' fileTypes <- ".*.out"
#'
#' # Should the function search the root directory recursively or not
#' recursiveSearch <- TRUE
#'
#' # What class object should be returned
#' returnClass <- "list"
#'
#' # Run the function in parallel?
#' parallelMplus <- TRUE
#'
#' # Call the function with the object arguments above
#' myMplusOutput <- combineMplusOutput(mplusPath = rootDir,
#' 		mplusFileType = fileTypes, recurse = recursiveSearch,
#' 		returnType = returnClass, parallel = parallelMplus)
#' }
#' @export combineMplusOutput
#'

combineMplusOutput <- function(mplusPath = NULL, mplusFileType = NULL,
							   recurse = TRUE, returnType = "list",
							   parallel = TRUE) {

	# Check for parallel option
	if (parallel == TRUE) {

		# Load package to handle parallelization on the backend
		library(doMC)

		# The doMC package will select a default based on a fraction of the
		# number of cores available on the system
		doMC::registerDoMC()

	} # End IF Block for parallel option

	# Name the elements of the mplusOutput object
	mplusOutNames <- c(paste0(mplusPath, list.files(pattern = mplusFileType,
							  recursive = recurse, path = mplusPath)))

	# Check type of Mplus output files to assign correct read function based
	# on the file type.  Input and output are parsed as text, graphics files
	# are read in consistently with Mplus's own functions, and data files are
	# read in as delimited text files.
	if (grepl("(.*.out$)|(.*.inp$)", mplusFileType, ignore.case = TRUE) == TRUE) {

		# Define reader function
		mplusReader <- function(x) { return(readLines(x)) }

	} else if (grepl("(.*.gh5$)", mplusFileType, ignore.case = TRUE) == TRUE) {

		# Define reader function for Mplus graph files
		mplusReader <- function(x) {

			# Read the HDF5 file into memory
			file <- rhdf5::h5dump(x, all = TRUE)

			# Close the file connection to prevent any memory leaks/conflicts
			# with subsequent function calls
			rhdf5::H5close()

			# Return the data object
			return(file)

		} # End Function definition for Mplus graphics files

	} else if(grepl("(.*.dat$)", mplusFileType, ignore.case = TRUE) == TRUE) {

		# Define reader function
		mplusReader <- function(x) { return(read.delim(x, header = FALSE)) }

	} else {

		# Error out
		stop("Only .inp, .dat, .out, and .gh5 Mplus file types supported")

	} # End Block for file type reader definitions

	# Check returnType parameter values
	if (returnType == "character") {

		# Create a single data object with all of the output combined
		mplusOutput <- unlist(plyr::llply(as.list(mplusOutNames),
						 .parallel = parallel, .fun = mplusReader))

	} else if (returnType == "list") {

		# Create a single data object with all of the output combined
		mplusOutput <- plyr::llply(as.list(mplusOutNames),
							 .parallel = parallel, .fun = mplusReader)

		# Name the elements of the Mplus output object
		names(mplusOutput) <- mplusOutNames

	} # End Block to read files

	# If return type is list, return the list of concatenated files
	return(mplusOutput)

} # End definition of combineMplusOutput object


