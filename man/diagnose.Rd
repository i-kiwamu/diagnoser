% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnose.R
\name{diagnose}
\alias{diagnose}
\title{diagnose}
\usage{
diagnose(model, ...)
}
\arguments{
\item{model}{\code{\link[stats]{aov}} object.}

\item{...}{Currently not to be used.}
}
\value{
An object of \code{tbl_df_diag}.
}
\description{
A generic function of a diagnosis of a model object
}
\details{
Available models are \code{aov}, \code{lm}, \code{lme}, and \code{lmerMod}.
}
