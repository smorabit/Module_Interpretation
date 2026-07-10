## ModuleSet adapter interface (generic contract).
## Core tools call only these generics, never a backend package directly.
## docs/implementation_guide.md#5

# list of module ids in the module set (e.g. hdWGCNA module colors/names)
modules <- function(ms, ...) UseMethod('modules')

# genes assigned to `module`, with their membership weight (e.g. kME), ranked strongest first
gene_membership <- function(ms, module, ...) UseMethod('gene_membership')

# per-cell (or per-sample) module scores, one column per module (e.g. hdWGCNA module eigengenes)
module_scores <- function(ms, ...) UseMethod('module_scores')

# underlying expression matrix backing the module set
# shadows base::expression() within this sourced session; no plotmath use in this repo
expression <- function(ms, ...) UseMethod('expression')

# cell / sample metadata data.frame (one row per column returned by expression()/module_scores())
metadata <- function(ms, ...) UseMethod('metadata')

# named list of backend package versions, for provenance logging (tools/fragment.R stay
# backend-agnostic, so this is how they learn which package versions actually produced the evidence)
pkg_versions <- function(ms, ...) UseMethod('pkg_versions')
