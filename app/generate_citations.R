# Parse refs.bib and write citations.json: a map from bibtex key to a formatted
# APA-ish citation string. The shinylive app bundles this file alongside
# tree.json and renders full citations on each leaf page.
#
# Run from the guide/ directory whenever refs.bib changes:
#   Rscript shinylive-app/generate_citations.R

library(jsonlite)

parse_bib <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  n <- nchar(txt)
  pos <- 1
  entries <- list()

  while (pos <= n) {
    at <- regexpr("@", substr(txt, pos, n), fixed = TRUE)
    if (at == -1) break
    pos <- pos + at - 1

    brace <- regexpr("\\{", substr(txt, pos, n))
    if (brace == -1) break
    type <- tolower(substr(txt, pos + 1, pos + brace - 2))
    pos <- pos + brace

    if (type %in% c("comment", "string", "preamble")) {
      depth <- 1
      while (pos <= n && depth > 0) {
        ch <- substr(txt, pos, pos)
        if (ch == "{") depth <- depth + 1
        else if (ch == "}") depth <- depth - 1
        pos <- pos + 1
      }
      next
    }

    depth <- 1
    start <- pos
    while (pos <= n && depth > 0) {
      ch <- substr(txt, pos, pos)
      if (ch == "{") depth <- depth + 1
      else if (ch == "}") depth <- depth - 1
      pos <- pos + 1
    }
    body <- substr(txt, start, pos - 2)

    comma_pos <- regexpr(",", body, fixed = TRUE)
    if (comma_pos == -1) next
    key <- trimws(substr(body, 1, comma_pos - 1))
    rest <- substr(body, comma_pos + 1, nchar(body))

    fields <- parse_fields(rest)
    entries[[key]] <- list(type = type, fields = fields)
  }

  entries
}

parse_fields <- function(s) {
  fields <- list()
  rpos <- 1
  rn <- nchar(s)

  while (rpos <= rn) {
    while (rpos <= rn &&
           substr(s, rpos, rpos) %in% c(" ", "\n", "\t", ",")) {
      rpos <- rpos + 1
    }
    if (rpos > rn) break

    eq <- regexpr("=", substr(s, rpos, rn), fixed = TRUE)
    if (eq == -1) break
    fname <- tolower(trimws(substr(s, rpos, rpos + eq - 2)))
    rpos <- rpos + eq

    while (rpos <= rn && substr(s, rpos, rpos) %in% c(" ", "\n", "\t")) {
      rpos <- rpos + 1
    }
    if (rpos > rn) break

    open <- substr(s, rpos, rpos)
    value <- ""
    if (open == "{") {
      rpos <- rpos + 1
      depth <- 1
      vstart <- rpos
      while (rpos <= rn && depth > 0) {
        ch <- substr(s, rpos, rpos)
        if (ch == "{") depth <- depth + 1
        else if (ch == "}") depth <- depth - 1
        if (depth > 0) rpos <- rpos + 1
      }
      value <- substr(s, vstart, rpos - 1)
      rpos <- rpos + 1
    } else if (open == '"') {
      rpos <- rpos + 1
      vstart <- rpos
      while (rpos <= rn && substr(s, rpos, rpos) != '"') rpos <- rpos + 1
      value <- substr(s, vstart, rpos - 1)
      rpos <- rpos + 1
    } else {
      vstart <- rpos
      while (rpos <= rn &&
             !(substr(s, rpos, rpos) %in% c(",", "\n"))) rpos <- rpos + 1
      value <- trimws(substr(s, vstart, rpos - 1))
    }

    fields[[fname]] <- clean_value(value)
  }

  fields
}

clean_value <- function(s) {
  s <- gsub("\\s+", " ", s)
  # Multi-letter LaTeX ligatures first (before brace stripping).
  s <- gsub("\\\\oe\\{?\\}?", "œ", s)
  s <- gsub("\\\\OE\\{?\\}?", "Œ", s)
  s <- gsub("\\\\ae\\{?\\}?", "æ", s)
  s <- gsub("\\\\AE\\{?\\}?", "Æ", s)
  s <- gsub("\\\\ss\\{?\\}?", "ß", s)
  s <- gsub("\\\\o\\{?\\}?", "ø", s)
  s <- gsub("\\\\O\\{?\\}?", "Ø", s)
  s <- gsub("\\\\aa\\{?\\}?", "å", s)
  s <- gsub("\\\\AA\\{?\\}?", "Å", s)
  s <- gsub("\\\\l\\{?\\}?", "ł", s)
  s <- gsub("\\\\L\\{?\\}?", "Ł", s)
  # Single-letter accents.
  s <- latex_accent(s, "~", c(a = "ã", A = "Ã", n = "ñ", N = "Ñ",
                              o = "õ", O = "Õ"))
  s <- latex_accent(s, "'", c(a = "á", A = "Á", e = "é", E = "É",
                              i = "í", I = "Í", o = "ó", O = "Ó",
                              u = "ú", U = "Ú", n = "ń", c = "ć",
                              s = "ś", z = "ź"))
  s <- latex_accent(s, "`", c(a = "à", A = "À", e = "è", E = "È",
                              i = "ì", I = "Ì", o = "ò", O = "Ò",
                              u = "ù", U = "Ù"))
  s <- latex_accent(s, '"', c(a = "ä", A = "Ä", e = "ë", E = "Ë",
                              i = "ï", I = "Ï", o = "ö", O = "Ö",
                              u = "ü", U = "Ü"))
  s <- latex_accent(s, "\\^", c(a = "â", A = "Â", e = "ê", E = "Ê",
                                i = "î", I = "Î", o = "ô", O = "Ô",
                                u = "û", U = "Û"))
  s <- latex_accent(s, "c", c(c = "ç", C = "Ç"))
  s <- gsub("\\{([^{}]*)\\}", "\\1", s)
  s <- gsub("[{}]", "", s)
  s <- gsub("--", "–", s)
  trimws(s)
}

latex_accent <- function(s, accent, map) {
  for (letter in names(map)) {
    pattern <- sprintf("\\\\%s\\{?%s\\}?", accent, letter)
    s <- gsub(pattern, map[[letter]], s, perl = TRUE)
  }
  s
}

format_one_author <- function(a) {
  a <- trimws(a)
  if (grepl(",", a, fixed = TRUE)) {
    parts <- strsplit(a, ",", fixed = TRUE)[[1]]
    last <- trimws(parts[1])
    first <- trimws(parts[2])
  } else {
    toks <- strsplit(a, "\\s+")[[1]]
    if (length(toks) == 0) return("")
    last <- toks[length(toks)]
    first <- paste(toks[-length(toks)], collapse = " ")
  }
  initials <- strsplit(first, "[\\s\\-]+", perl = TRUE)[[1]]
  initials <- initials[nchar(initials) > 0]
  initials <- vapply(initials, function(x) paste0(toupper(substr(x, 1, 1)), "."),
                     character(1))
  if (length(initials) == 0) return(last)
  sprintf("%s, %s", last, paste(initials, collapse = " "))
}

format_authors <- function(s) {
  if (is.null(s) || !nzchar(s)) return("")
  authors <- strsplit(s, " and ", fixed = TRUE)[[1]]
  formatted <- vapply(authors, format_one_author, character(1))
  formatted <- formatted[nzchar(formatted)]
  if (length(formatted) == 0) return("")
  if (length(formatted) == 1) return(formatted)
  if (length(formatted) == 2) return(paste(formatted, collapse = " & "))
  paste0(paste(formatted[-length(formatted)], collapse = ", "),
         ", & ", formatted[length(formatted)])
}

format_apa <- function(entry) {
  f <- entry$fields
  authors <- format_authors(f$author %||% f$editor %||% "")
  year <- f$year %||% "n.d."
  title <- f$title %||% ""

  # Drop trailing period from title to avoid double punctuation.
  title <- sub("\\.$", "", title)

  head <- if (nzchar(authors)) sprintf("%s (%s).", authors, year)
          else sprintf("(%s).", year)

  if (entry$type %in% c("book")) {
    pub <- f$publisher %||% ""
    out <- sprintf("%s *%s*.", head, title)
    if (nzchar(pub)) out <- sprintf("%s %s.", out, pub)
    return(out)
  }

  if (entry$type %in% c("incollection", "inbook", "inproceedings")) {
    bt <- f$booktitle %||% ""
    eds <- f$editor
    pub <- f$publisher %||% ""
    out <- sprintf("%s %s. In ", head, title)
    if (!is.null(eds) && nzchar(eds))
      out <- sprintf("%s%s (Eds.), ", out, format_authors(eds))
    out <- sprintf("%s*%s*.", out, bt)
    if (nzchar(pub)) out <- sprintf("%s %s.", out, pub)
    return(out)
  }

  if (entry$type %in% c("unpublished", "misc", "techreport", "manual")) {
    note <- f$note %||% f$howpublished %||% f$institution %||% ""
    out <- sprintf("%s %s.", head, title)
    if (nzchar(note)) out <- sprintf("%s *%s*.", out, note)
    if (!is.null(f$url)) out <- sprintf("%s %s", out, f$url)
    return(out)
  }

  # Default: article-ish.
  journal <- f$journal %||% f$booktitle %||% f$publisher %||% ""
  vol <- f$volume
  num <- f$number
  pages <- f$pages

  out <- sprintf("%s %s.", head, title)
  if (nzchar(journal)) {
    out <- sprintf("%s *%s*", out, journal)
    if (!is.null(vol) && nzchar(vol)) {
      if (!is.null(num) && nzchar(num)) out <- sprintf("%s, %s(%s)", out, vol, num)
      else out <- sprintf("%s, %s", out, vol)
    }
    if (!is.null(pages) && nzchar(pages)) out <- sprintf("%s, %s", out, pages)
    out <- paste0(out, ".")
  }
  if (!is.null(f$doi) && nzchar(f$doi))
    out <- sprintf("%s https://doi.org/%s", out, f$doi)
  else if (!is.null(f$url) && nzchar(f$url))
    out <- sprintf("%s %s", out, f$url)
  out
}

`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a

bib_path <- "refs.bib"
out_path <- "shinylive-app/citations.json"

entries <- parse_bib(bib_path)
cits <- lapply(entries, format_apa)
names(cits) <- names(entries)

# Keep only keys referenced by the tree (plus any the user may add later).
tree <- fromJSON("shinylive-app/tree.json", simplifyVector = FALSE)
used <- character(0)
for (n in tree$nodes) {
  if (identical(n$type, "leaf")) used <- c(used, unlist(n$citation_keys))
}
used <- unique(used)

missing <- setdiff(used, names(cits))
if (length(missing) > 0) {
  warning("Keys referenced by tree.json but missing from refs.bib: ",
          paste(missing, collapse = ", "))
}

write_json(cits, out_path, auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("Wrote %s with %d entries (%d used by tree.json).\n",
            out_path, length(cits), length(intersect(used, names(cits)))))
