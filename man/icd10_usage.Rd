% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{icd10_usage}
\alias{icd10_usage}
\title{Yearly ICD-10 Code Usage from Hospital Admitted Patient Care Activity in England}
\format{
A data frame with 135,951 rows and 5 columns:
\describe{
\item{start_date}{Start date of code usage count}
\item{end_date}{End date of code usage count}
\item{icd10_code}{The 4-character ICD-10 Code.
Note that the punctuation from the code has been removed for compatibility with OpenCodelists.}
\item{usage}{Annual count of code usage.}
\item{description}{Description of the ICD-10 Code}
}
}
\source{
\url{https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity}
}
\usage{
icd10_usage
}
\description{
Yearly summary of 4-character ICD-10 code usage from 1st April 2013 to 31st March 2024.
The code usage represents the annual count of all episodes which record the given ICD-10 code in any primary or secondary position.
Restricted codes for which annual usage is not published have been removed.
Yearly summary of 4-character ICD-10 code usage from 1st April 2013 to 31st March 2024.
The code usage represents the annual count of all episodes which record the given ICD-10 code in any primary or secondary position.
Restricted codes for which annual usage is not published have been removed.
}
\examples{
# Filter to codes in the ICD-10 Chapter XIX: "Injury, poisoning..."
# (codes begin with letters "S" or "T"), with usage > 10,000.
# For each of these, select the year with the highest count.
icd10_usage |>
  dplyr::filter(grepl("^[ST]", icd10_code) & usage > 10000) |>
  dplyr::group_by(description) |>
  dplyr::slice_max(usage)
# Filter to codes present in the CPRD Aurum ICD-10 pregnancy codelist.
# This codelist is available in OpenCodelists.org
codelist <- read.csv(
  "https://www.opencodelists.org/codelist/opensafely/pregnancy-icd10-aurum/5a7d8d12/download.csv"
)
icd10_usage |>
  dplyr::filter(icd10_code \%in\% codelist$code)
# Filter to codes in the ICD-10 Chapter XIX: "Injury, poisoning..."
# (codes begin with letters "S" or "T"), with usage > 10,000.
# For each of these, select the year with the highest count.
icd10_usage |>
  dplyr::filter(grepl("^[ST]", icd10_code) & usage > 10000) |>
  dplyr::group_by(description) |>
  dplyr::slice_max(usage)
# Filter to codes present in the CPRD Aurum ICD-10 pregnancy codelist.
# This codelist is available in OpenCodelists.org
codelist <- read.csv(
  "https://www.opencodelists.org/codelist/opensafely/pregnancy-icd10-aurum/5a7d8d12/download.csv"
)
icd10_usage |>
  dplyr::filter(icd10_code \%in\% codelist$code)
}
\keyword{datasets}
