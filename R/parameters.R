#' Parameter conversion table for SNIRH format
#'
#' This dataset contains the mapping between laboratory parameter names/units
#' and their equivalent SNIRH (Sistema Nacional de Informação de Recursos Hídricos)
#' database format. It includes conversion factors for unit transformations and
#' standardized symbols used in the SNIRH system.
#'
#' @format A data.table with 7 variables and multiple rows covering water quality,
#'   sediment, and biota parameters:
#' \describe{
#'   \item{param_lab}{Character. Parameter name as provided by the laboratory.
#'     These are the original parameter names found in laboratory reports and
#'     may include special characters, accents, or laboratory-specific naming
#'     conventions.}
#'   \item{unit_lab}{Character. Unit of measurement as provided by the laboratory.
#'     These represent the original units used in laboratory measurements and
#'     may vary between laboratories or analytical methods.}
#'   \item{symbol_snirh}{Character. Standardized parameter symbol used in the
#'     SNIRH database. These symbols are unique identifiers that allow for
#'     consistent data storage and retrieval in the national database.}
#'   \item{param_snirh}{Character. Standardized parameter name used in SNIRH.
#'     These names follow SNIRH conventions and provide consistency across
#'     different data sources and time periods.}
#'   \item{unit_snirh}{Character. Standardized unit used in the SNIRH database.
#'     All measurements are converted to these standard units to ensure
#'     comparability and compliance with national monitoring standards.}
#'   \item{factor}{Numeric. Conversion factor to transform laboratory units to
#'     SNIRH units. The formula is: snirh_value = lab_value * factor.
#'     For example, if converting mg/L to µg/L, the factor would be 1000.}
#'   \item{sample_type}{Character. Type of sample matrix. Valid values are:
#'     \itemize{
#'       \item \code{water}: Surface water and groundwater samples
#'       \item \code{biota}: Biological samples (fish, plants, etc.)
#'       \item \code{sediment}: Sediment samples from aquatic environments
#'     }}
#' }
#'
#' @details
#' This dataset is essential for the \code{\link{convert_to_snirh}} function,
#' which uses it to:
#' \itemize{
#'   \item Validate that all laboratory parameters can be converted to SNIRH format
#'   \item Apply appropriate unit conversions using the conversion factors
#'   \item Map laboratory parameter names to standardized SNIRH symbols
#'   \item Ensure data quality and consistency with national standards
#' }
#'
#' The conversion factors are carefully calibrated to maintain measurement
#' accuracy while ensuring compliance with SNIRH database requirements.
#' Parameters without a direct SNIRH equivalent are not included in this table
#' and will cause the conversion function to raise an error.
#'
#' @section Parameter Categories:
#' The parameters are organized by sample type:
#' \describe{
#'   \item{Water parameters}{Include physical properties (temperature, pH,
#'     conductivity), chemical parameters (nutrients, metals, organic compounds),
#'     and biological indicators.}
#'   \item{Sediment parameters}{Cover grain size distribution, chemical composition,
#'     contaminant levels, and organic matter content.}
#'   \item{Biota parameters}{Include bioaccumulation measurements, biological
#'     indices, and organism-specific parameters.}
#' }
#'
#' @section Data Quality:
#' This dataset is maintained according to:
#' \itemize{
#'   \item SNIRH technical specifications and data model requirements
#'   \item Portuguese water quality monitoring standards (WFD implementation)
#'   \item European Water Framework Directive requirements
#'   \item Laboratory accreditation standards (ISO 17025)
#' }
#'
#' @section Updates:
#' This dataset should be updated when:
#' \itemize{
#'   \item New parameters are added to SNIRH database
#'   \item Laboratory methods change, requiring new unit conversions
#'   \item SNIRH symbols or naming conventions are updated
#'   \item New sample types are introduced to the monitoring program
#' }
#'
#' @examples
#' \dontrun{
#' # View all available parameters for water samples
#' water_params <- parameters[sample_type == "water"]
#' print(water_params[, .(param_lab, unit_lab, param_snirh, unit_snirh)])
#'
#' # Check conversion factor for a specific parameter
#' ph_conversion <- parameters[param_lab == "pH" & sample_type == "water"]
#' print(ph_conversion$factor)  # Should be 1 (no conversion needed)
#'
#' # Find all parameters that require unit conversion
#' converted_params <- parameters[factor != 1]
#' print(converted_params[, .(param_lab, unit_lab, unit_snirh, factor)])
#'
#' # Get SNIRH symbols for biota parameters
#' biota_symbols <- parameters[sample_type == "biota", unique(symbol_snirh)]
#' print(biota_symbols)
#' }
#'
#' @seealso
#' \code{\link{convert_to_snirh}} for the main conversion function that uses this data
#'
#' @source
#' \itemize{
#'   \item SNIRH database documentation: \url{https://snirh.apambiente.pt}
#'   \item Portuguese Environment Agency (APA) technical specifications
#'   \item Laboratory analytical method specifications
#'   \item Water Framework Directive monitoring requirements
#' }
#'
#' @references
#' \itemize{
#'   \item APA (2023). Critérios para a monitorização das massas de água. \url{https://apambiente.pt/sites/default/files/_SNIAMB_Agua/DRH/PlaneamentoOrdenamento/PGRH/2022-2027/PGRH_3_PTCONT_Monitorizacao.pdf}
#'   \item APA (2023). Critérios para a classificação das massas de água. \url{https://apambiente.pt/sites/default/files/_SNIAMB_Agua/DRH/PlaneamentoOrdenamento/PGRH/2022-2027/PGRH_3_PTCONT_SistemasClassificacao.pdf}
#'   \item European Commission (2000). Water Framework Directive 2000/60/EC
#'   \item ISO/IEC 17025:2017. General requirements for the competence of testing
#'     and calibration laboratories
#' }
#'
#' @keywords datasets water-quality environmental-monitoring SNIRH Portugal
"parameters"
