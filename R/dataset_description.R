## dataset_description: the REQUIRED biological context prepended to every
## synthesis prompt (docs/milestone_2.md task 1) so the model interprets a
## module in the right frame (e.g. CSF myeloid vs. tumor changes what a given
## program means, and disambiguates gene function like microglia vs. macrophage).
## A missing/empty description is a hard error, not a default.

#' Construct a dataset description
#'
#' The required biological context prepended to every synthesis prompt (see
#' [render_dataset_description()], [build_user_prompt()]) so the model
#' interprets a module in the right frame -- e.g. CSF myeloid vs. tumor
#' changes what a given program means, and disambiguates gene function like
#' microglia vs. macrophage.
#'
#' @param species Species, e.g. `'human'`.
#' @param tissue Tissue, e.g. `'CSF'`.
#' @param cell_compartment Cell compartment / lineage, e.g. `'myeloid'`.
#' @param assay Assay, e.g. `'scRNA-seq'`.
#' @param conditions Optional character vector of conditions/groups present
#'   in the dataset.
#' @param notes Optional free-text notes.
#' @param module_method Optional free-form description of how the modules
#'   themselves were generated, e.g. `'hdWGCNA co-expression modules'` or
#'   `'cNMF factors, k=20'` -- disambiguates what a "module" means for this
#'   run (a co-expression program vs. a factorization component) since that
#'   changes how the model should interpret gene weights/usages.
#' @return A `dataset_description` object.
#' @examples
#' dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq', conditions = c('MS', 'control'))
#' @export
dataset_description <- function(species, tissue, cell_compartment, assay,
                                 conditions = character(0), notes = NA_character_,
                                 module_method = NA_character_){
    desc <- list(
        species = species,
        tissue = tissue,
        cell_compartment = cell_compartment,
        assay = assay,
        conditions = conditions,
        notes = notes,
        module_method = module_method
    )
    structure(desc, class = 'dataset_description')
}

#' Validate a dataset description
#'
#' Hard-errors on a missing/empty required field (`species`, `tissue`,
#' `cell_compartment`, `assay`); `conditions`/`notes` may be empty, since not
#' every dataset has discrete conditions. A missing/empty description is a
#' hard error, not a default, since it's required biological context for synthesis.
#'
#' @param desc A `dataset_description` object.
#' @return Invisibly `TRUE` if valid; otherwise throws.
#' @examples
#' validate_dataset_description(dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq'))
#' @export
validate_dataset_description <- function(desc){
    if (!inherits(desc, 'dataset_description')) stop('object is not class dataset_description')
    required <- c('species', 'tissue', 'cell_compartment', 'assay')
    for (field in required) {
        value <- desc[[field]]
        if (is.null(value) || !is.character(value) || length(value) != 1 || is.na(value) || !nzchar(trimws(value))) {
            stop('dataset_description$', field, ' is required and must be a non-empty string')
        }
    }
    invisible(TRUE)
}

#' Render a dataset description as a compact text block
#'
#' Prepended to the synthesis prompt; see [build_user_prompt()].
#'
#' @param desc A `dataset_description` object.
#' @param data_level Observation-unit descriptor of the `ModuleSet` the
#'   evidence packet was built from, e.g. `'cell'` or `'sample'`; see
#'   [components_ModuleSet()]. Default `'cell'`.
#' @param aggregated Whether that `ModuleSet`'s expression/scores are already
#'   aggregated across cells (e.g. pseudobulk) rather than per-cell. Default `FALSE`.
#' @return A single character string.
#' @examples
#' cat(render_dataset_description(dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq')))
#' @export
render_dataset_description <- function(desc, data_level = 'cell', aggregated = FALSE){
    validate_dataset_description(desc)
    lines <- c(
        'Dataset context:',
        paste0('- species: ', desc$species),
        paste0('- tissue: ', desc$tissue),
        paste0('- cell compartment: ', desc$cell_compartment),
        paste0('- assay: ', desc$assay),
        paste0('- data level: ', data_level),
        paste0('- aggregated: ', aggregated)
    )
    if (length(desc$conditions) > 0) {
        lines <- c(lines, paste0('- conditions: ', paste(desc$conditions, collapse = ', ')))
    }
    if (!is.null(desc$module_method) && !is.na(desc$module_method) && nzchar(trimws(desc$module_method))) {
        lines <- c(lines, paste0('- module generation method: ', desc$module_method))
    }
    if (!is.null(desc$notes) && !is.na(desc$notes) && nzchar(trimws(desc$notes))) {
        lines <- c(lines, paste0('- notes: ', desc$notes))
    }
    paste(lines, collapse = '\n')
}
