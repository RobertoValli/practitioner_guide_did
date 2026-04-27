# Regenerate the static Mermaid diagram in 09-decision-tree.qmd from tree.json.
#
# Usage (from the guide/ directory):
#   Rscript shinylive-app/generate_mermaid.R
#
# Writes the mermaid source to stdout. Copy it into the {mermaid} block
# in 09-decision-tree.qmd marked with "auto-generated" below.

library(jsonlite)

tree <- fromJSON("shinylive-app/tree.json", simplifyVector = FALSE)
nodes <- tree$nodes

sanitize <- function(s) {
  # Mermaid chokes on unescaped quotes and some brackets inside quoted labels.
  s <- gsub('"', "'", s, fixed = TRUE)
  s
}

short_prompt <- function(prompt, max_len = 60) {
  if (nchar(prompt) <= max_len) return(prompt)
  paste0(substr(prompt, 1, max_len - 1), "…")
}

cat("flowchart TD\n")

# Node declarations.
for (id in names(nodes)) {
  n <- nodes[[id]]
  if (identical(n$type, "question")) {
    label <- sanitize(short_prompt(n$prompt))
    cat(sprintf('    %s{"%s"}\n', id, label))
  } else {
    # Leaf: method title + chapter.
    label <- sprintf("<b>%s</b><br/>%s", sanitize(n$title), sanitize(n$chapter))
    cat(sprintf('    %s["%s"]\n', id, label))
  }
}

cat("\n")

# Edges.
for (id in names(nodes)) {
  n <- nodes[[id]]
  if (identical(n$type, "question")) {
    for (opt in n$options) {
      label <- sanitize(short_prompt(opt$label, max_len = 40))
      cat(sprintf('    %s -->|"%s"| %s\n', id, label, opt[["next"]]))
    }
  }
}

# Style leaves distinctly.
cat("\n")
cat("    classDef leaf fill:#e8f1ff,stroke:#1a5490,color:#1a5490;\n")
leaf_ids <- names(nodes)[vapply(nodes, function(n) identical(n$type, "leaf"),
                                logical(1))]
cat(sprintf("    class %s leaf;\n", paste(leaf_ids, collapse = ",")))
