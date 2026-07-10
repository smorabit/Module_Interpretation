# R / Rmd code style

Style conventions for analysis and plotting code in this project (`compact_2025/` and related).
Follow these when writing or editing `.R` and `.Rmd` files so the code reads like the rest of the
codebase. The goal is readable, iterable notebook code — not production boilerplate.

---

## Notebook structure (`.Rmd`)

Downstream analysis and plotting live in `.Rmd` notebooks run **interactively, chunk by chunk**
(chunks are tagged `{r eval=FALSE}` — they are not meant to knit straight through).

- The very top of the file carries the interactive-node launch line plus `module load` / `conda activate`
  as plain comment lines, e.g.:
  ```
  # launch an interactive node
  interactive 8 256G 12:00:00 genD

  # to launch:
  module load GCC
  conda activate compact
  ```
- **Setup chunk:** `library()` calls grouped by purpose, each group with a one-line category comment;
  then `theme_set(theme_cowplot())`, `setwd(...)`, `source(...)` of helper scripts, config load, and
  `fig_dir` / `data_dir`.
- **Before each chunk:** one or two sentences of plain prose describing its purpose. Keep it brief; a
  short numbered list of steps is fine for a multi-step chunk.

---

## Naming

- snake_case for variables. `cur_` prefix for loop / iteration variables (`cur_net`, `cur_mod`, `cur_seurat`).
- Conventional short names: `p` / `p1` / `p2` for plots, `patch` for patchwork compositions, `plot_df`
  for the frame being plotted, `X` for an expression matrix, `meta`, `se`, `mae`.
- Package / exported functions are PascalCase (`AggregatePseudobulk`, `PlotPCAEmbedding`); local helper
  functions are snake_case / lowercase (`make_qc_plot`, `scale01`).

---

## Formatting

- **Indentation is Pythonic.** Even though R ignores it, indent the contents of every `{}` block (4
  spaces). Nested blocks nest their indentation.
- **Do not align `<-` or `=` across lines.** One space each side, no padding to line things up.
- Use `<-` for assignment. Prefer **single quotes** for strings.
- **One verb per line** in tidyverse pipes and ggplot chains — break the pipe / `+` so each
  `group_by` / `summarise` / `geom_*` / `theme` sits on its own line:
  ```r
  plot_df <- cur_df %>%
      group_by(module) %>%
      summarise(mean_expr = mean(expr))

  p <- plot_df %>%
      ggplot(aes(x = module, y = mean_expr)) +
      geom_col() +
      RotatedAxis()
  ```
- magrittr `%>%` pipes (not native `|>`). `.$col` and `.` placeholders are fine.
- `do.call(rbind, lapply(...))` for table-building; `print()` for loop progress.

---

## Comments

- Sparse and short — lowercase, one comment per logical step. A ggplot block needs at most one comment
  naming the plot.
- Numbered `# 1.` / `# 2.` comments for genuinely sequential steps.
- Within-chunk section dividers use the banner style:
  ```
  #---------------------------------------------------------#
  # Visualize QC metrics
  #---------------------------------------------------------#
  ```
- **No Roxygen2 headers** anywhere. No file-level Inputs/Outputs/Run/Env docblocks.

---

## Plotting

- Idiom: build the plot, then write it to PDF:
  ```r
  p <- plot_df %>%
      ggplot(aes(x = nUMI, y = nFeatures)) +
      geom_point(aes(color = Tissue)) +
      ggtitle('Pseudobulk QC')

  pdf(paste0(fig_dir, 'pseudobulk_qc.pdf'), width = 8, height = 6)
  print(p)
  dev.off()
  ```
- `ggrastr::rasterise(..., dpi = ...)` for point-heavy layers. `RotatedAxis()`, `NoLegend()`,
  `coord_fixed()` / `coord_equal()` as needed.

---

## Helper functions

- Define helper functions at the **top of the file**, or source them from a `scripts/*.R` helpers file.
- Keep them succinct. A brief description and internal comments are welcome; **no Roxygen2**.

---

## Anti-patterns to avoid

These are the tells of generated code that does not match this project:

- Aligned `<-` / `=` assignment blocks.
- Heavy `## ===== banner ===== ##` headers and verbose multi-line file docblocks.
- Per-line narration comments; Roxygen on internal helpers.
- Over-defensive `stopifnot()` / NA-guards on every helper.
- Base-R `vapply` / `setNames` / `FUN.VALUE` gymnastics where a dplyr / tidyverse expression reads cleaner.
- Sourced `config.R` indirection for a few paths — prefer a flat params block with inline
  `path.expand('~/...')` paths at the top of the file.
