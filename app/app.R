library(shiny)
library(jsonlite)

tree <- fromJSON("tree.json", simplifyVector = FALSE)
nodes <- tree$nodes
root_id <- tree$root

citations <- tryCatch(
  fromJSON("citations.json", simplifyVector = TRUE),
  error = function(e) list()
)

# Minimal markdown → HTML: `code`, *italic*, bare URLs. Returns shiny::HTML().
render_inline_md <- function(text) {
  if (is.null(text) || !nzchar(text)) return(HTML(""))
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }
  html <- esc(text)
  html <- gsub("`([^`]+)`", "<code>\\1</code>", html)
  html <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", html)
  html <- gsub("(https?://[^\\s)]+)",
               '<a href="\\1" target="_blank" rel="noopener">\\1</a>',
               html, perl = TRUE)
  HTML(html)
}

# Links inside the iframe must navigate the top-level page. The chapter HTML
# files live one directory above the shinylive bundle in the rendered Quarto
# site (`_site/03-*.html` vs `_site/shinylive/index.html`).
CHAPTER_BASE <- "../"

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, sans-serif; }
      .tree-question { font-size: 1.1em; margin-bottom: 1em; }
      .tree-help { color: #666; font-size: 0.95em; margin-bottom: 1em; }
      .tree-option-btn { display: block; width: 100%; text-align: left;
                          margin-bottom: 0.4em; padding: 0.7em 1em; }
      .breadcrumbs { color: #666; font-size: 0.85em; margin-bottom: 1em; }
      .leaf-title { color: #1a5490; margin-top: 0; }
      .leaf-section { margin-bottom: 1em; }
      .leaf-section h5 { margin-bottom: 0.3em; color: #333; }
      pre.leaf-code { background: #f5f5f5; padding: 0.8em; border-radius: 4px;
                      font-size: 0.85em; overflow-x: auto; }
      .leaf-flag { display: inline-block; background: #eef; color: #225;
                   padding: 0.2em 0.6em; border-radius: 3px; margin-right: 0.4em;
                   font-size: 0.8em; }
      .leaf-flag.warn { background: #fee; color: #c44; }
      .reset-btn { margin-top: 1em; }
      .leaf-citations { padding-left: 1.2em; }
      .leaf-citations li { margin-bottom: 0.4em; font-size: 0.93em; }
      .leaf-citations a { color: #1a5490; text-decoration: none; word-break: break-all; }
      .leaf-citations a:hover { text-decoration: underline; }
      .leaf-section code { background: #f3f3f3; padding: 0.05em 0.35em;
                           border-radius: 3px; font-size: 0.9em; }
    "))
  ),
  titlePanel("A Decision Tree for Panel-Data Estimators"),
  fluidRow(
    column(
      12,
      uiOutput("breadcrumbs"),
      uiOutput("main_panel"),
      uiOutput("nav_buttons")
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(current = root_id, history = character(0))

  observeEvent(input$reset, {
    rv$current <- root_id
    rv$history <- character(0)
  })

  observeEvent(input$back, {
    if (length(rv$history) > 0) {
      rv$current <- rv$history[length(rv$history)]
      rv$history <- rv$history[-length(rv$history)]
    }
  })

  # Dynamic observers for each question's options.
  lapply(names(nodes), function(nid) {
    node <- nodes[[nid]]
    if (identical(node$type, "question")) {
      lapply(seq_along(node$options), function(i) {
        btn_id <- paste0("go__", nid, "__", i)
        observeEvent(input[[btn_id]], {
          rv$history <- c(rv$history, rv$current)
          rv$current <- node$options[[i]][["next"]]
        }, ignoreInit = TRUE)
      })
    }
  })

  output$breadcrumbs <- renderUI({
    steps <- c(rv$history, rv$current)
    if (length(steps) <= 1) return(NULL)
    labels <- vapply(steps, function(id) {
      n <- nodes[[id]]
      if (identical(n$type, "question")) {
        short <- substr(n$prompt, 1, 40)
        if (nchar(n$prompt) > 40) short <- paste0(short, "…")
        short
      } else {
        n$title
      }
    }, character(1))
    div(class = "breadcrumbs",
        paste0("Path: ", paste(labels, collapse = "  →  ")))
  })

  output$main_panel <- renderUI({
    node <- nodes[[rv$current]]
    if (identical(node$type, "question")) {
      render_question(node)
    } else {
      render_leaf(node)
    }
  })

  output$nav_buttons <- renderUI({
    tagList(
      if (length(rv$history) > 0) {
        actionButton("back", "Back")
      },
      if (length(rv$history) > 0 || identical(nodes[[rv$current]]$type, "leaf")) {
        actionButton("reset", "Start over", class = "reset-btn")
      }
    )
  })
}

render_question <- function(node) {
  tagList(
    div(class = "tree-question", tags$strong(node$prompt)),
    if (!is.null(node$help)) div(class = "tree-help", node$help),
    tagList(lapply(seq_along(node$options), function(i) {
      opt <- node$options[[i]]
      btn_id <- paste0("go__", node$id, "__", i)
      actionButton(btn_id, opt$label,
                   class = "btn btn-default tree-option-btn")
    }))
  )
}

render_leaf <- function(node) {
  flags <- node$flags %||% list()
  flag_tags <- tagList(
    if (isTRUE(flags$binary_only))
      span(class = "leaf-flag warn", "Binary treatment only"),
    if (isTRUE(flags$heterogeneity_robust))
      span(class = "leaf-flag", "Heterogeneity-robust"),
    if (isTRUE(flags$no_dedicated_package))
      span(class = "leaf-flag warn", "No dedicated R package")
  )

  chapter_url <- paste0(CHAPTER_BASE, node$chapter)

  tagList(
    h3(class = "leaf-title", "Recommendation: ", node$title),
    flag_tags,
    div(class = "leaf-section",
        h5("Primary method"),
        p(render_inline_md(node$primary_method))),
    if (length(node$alternatives) > 0)
      div(class = "leaf-section",
          h5("Alternatives"),
          tags$ul(lapply(node$alternatives,
                         function(x) tags$li(render_inline_md(x))))),
    div(class = "leaf-section",
        h5("Required assumptions"),
        tags$ul(lapply(node$assumptions,
                       function(x) tags$li(render_inline_md(x))))),
    div(class = "leaf-section",
        h5("Caveats"),
        tags$ul(lapply(node$caveats,
                       function(x) tags$li(render_inline_md(x))))),
    div(class = "leaf-section",
        h5("R package"),
        p(tags$code(node$package))),
    if (!is.null(node$code) && nchar(node$code) > 0)
      div(class = "leaf-section",
          h5("Minimal code"),
          tags$pre(class = "leaf-code", node$code)),
    if (length(node$citation_keys) > 0)
      div(class = "leaf-section",
          h5("Key references"),
          tags$ul(class = "leaf-citations",
            lapply(node$citation_keys, function(k) {
              text <- citations[[k]]
              if (is.null(text) || !nzchar(text)) text <- k
              tags$li(render_inline_md(text))
            }))),
    div(class = "leaf-section",
        tags$a(href = chapter_url, target = "_top",
               paste0("Read the chapter \u2192 ", node$chapter)))
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
