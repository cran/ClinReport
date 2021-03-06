% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.quanti.R
\name{report.quanti}
\alias{report.quanti}
\title{Descriptive "Quantitative" statistics (mean, SD, median...) reporting}
\usage{
report.quanti(data, y, x1 = NULL, x2 = NULL, y.label = y,
  round = 2, total = F, scientific = F, digits = NULL,
  at.row = NULL, subjid = NULL, geomean = F, add.mad = F,
  default.stat = T, func.stat = NULL, stat.name = "Statistics",
  func.stat.name = "", drop.x1 = NULL, drop.x2 = NULL)
}
\arguments{
\item{data}{Data.frame object}

\item{y}{Character indicating a numerical vector in the data frame passed to \code{data} argument}

\item{x1}{Character indicating a factor in the data (levels will be displayed in columns)}

\item{x2}{Character indicating a factor in the data (levels will be displayed in lines)}

\item{y.label}{Character Indicates the label for y parameter to be displayed in the title of the table}

\item{round}{Numeric to indicate how to round statistics}

\item{total}{Logical to indicate if a "Total" column should be added}

\item{scientific}{Logical Indicates if statistics should be displayed in scientific notations or not}

\item{digits}{Numeric (used if scientific=TRUE) to indicate how many digits to use in scientific notation}

\item{at.row}{Character Used to space the results (see examples)}

\item{subjid}{Character Indicates the column in which there is the subject Id to add the number of subjects in the column header if x1 and x2 are not null.}

\item{geomean}{Logical If yes geometric mean is calculated  instead of arithmetic mean: \code{exp(mean(log(x),na.rm=TRUE))} for x>0}

\item{add.mad}{Logical If yes the Median Absolute Deviance is added to the median statistics (see function \code{\link{mad}})}

\item{default.stat}{Logical (default to TRUE). If FALSE you can specify your own example}

\item{func.stat}{Function. If specified then default.stat=FALSE and only the specified statistic is reported}

\item{stat.name}{Character. Indicates the name of the variable that report the statistics Default = "Statistics"}

\item{func.stat.name}{Character. Used only if default.stat=FALSE.  Indicates the name of specific statistic you want to report}

\item{drop.x1}{Character. Indicates one or several levels of the x1 factor that you want to drop in the result}

\item{drop.x2}{Character. Indicates one or several levels of the x2 factor that you want to drop in the result}
}
\value{
A desc object.
}
\description{
\code{report.quanti} 
Returns quantitative descriptive statistics such as mean, median, standard deviation etc...

For more examples see the website: \href{https://jfrancoiscollin.github.io/ClinReport}{ClinReport website}
}
\details{
This function computes and reports quantitative statistics on \code{y}. And can gives the statistics by level of two factors (\code{x1}
in columns and/or \code{x2} in rows). 
See the example to show the results. If \code{total=TRUE}, the last column is the statistics
performed overall levels of \code{x1} for each levels of \code{x2}. 
Quantiles are calculated using type 3 (SAS presumed definition) algorithms, but even though,
some differences between SAS and R can appear on quantile values.

"geomean" compute the geometric mean defined as exp(mean(log(y))). The values below or equal 0 are removed and
a message is printed  to indicate how many values were deleted to calculate the geometric mean.

\code{N} returns the number of observations (including NA values)

stat.name is automatically transformed using \code{\link{make.names}} function.
}
\examples{
 
data(datafake)

# Quantitative statistics with no factor

report.quanti(data=datafake,y="y_numeric",total=TRUE,y.label="Awesome results")

#' # Quantitative statistics with no factor with geometric mean (option geomean=TRUE)

report.quanti(data=datafake,y="y_numeric",y.label="Awesome results",geomean=TRUE)

# Quantitative statistics with one factor

report.quanti(data=datafake,y="y_numeric",x1="GROUP")

# One factor with total column

report.quanti(data=datafake,y="y_numeric",x1="GROUP",total=TRUE)

# Quantitative statistics with two factors

report.quanti(data=datafake,y="y_numeric",x1="GROUP",x2="TIMEPOINT")

# Quantitative statistics with two factors and a total column

report.quanti(data=datafake,y="y_numeric",x1="GROUP",x2="TIMEPOINT",total=TRUE)

# Add median absolute deviance to the median statistics

report.quanti(data=datafake,y="y_numeric",x1="GROUP",x2="TIMEPOINT",total=TRUE,add.mad=TRUE)

# Quantitative statistics with spacing rows (option at.row)

report.quanti(data=datafake,y="y_numeric",x1="GROUP",
x2="TIMEPOINT",total=TRUE,at.row="TIMEPOINT")

# Add number of subjects in headers (option subjid)

tab=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
x2="TIMEPOINT",total=TRUE,at.row="TIMEPOINT",subjid="SUBJID")

# Print tab output
tab


#Getting a specific statistic and not the default ones

mystat=function(x) quantile(x,0.99,na.rm=TRUE)

tab=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
x2="TIMEPOINT",total=TRUE,subjid="SUBJID",
func.stat=mystat,func.stat.name="99\% quantile")
tab

mystat2=function(x) mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE)

tab=report.quanti(data=datafake,y="y_numeric",x1="GROUP",
total=TRUE,subjid="SUBJID",func.stat=mystat2,
func.stat.name="Coefficient of variation")
tab

mode=function(x) {
  x=na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tab=report.quanti(data=datafake,y="y_numeric",
func.stat=mode,func.stat.name="Mode")


#Getting raw output
tab$raw.output

#Getting a data.frame version of the output
tab$output

}
\seealso{
\code{\link{report.quali}} \code{\link{report.doc}} \code{\link{desc}}
}
