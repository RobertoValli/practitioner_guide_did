# A Practitioner's Decision Tree for Panel-Data Causal Inference

A Shiny-app-based interactive decision tree that recommends an appropriate estimator for panel data based on the structure of your design (number of treated units, treatment timing, absorbing vs. non-absorbing, binary vs. continuous, ...). Each leaf returns the primary method, alternatives, identifying assumptions, caveats, an R package, and minimal example code.

The app was initially created to provide a heuristic for EUI students to find the most appropriate estimator in the stunningly fast growing literature on DiD-like estimators and extensions. It is a work in progress, so please reach out if you find mistakes or you think something is missing!

**Live:** <https://robertovalli.github.io/practitioner_guide_did/>

## Repository layout

```
_quarto.yml               # Quarto site config
index.qmd                 # landing-page source
app/                      # version-controlled shinylive source
  app.R                   # Shiny UI + server
  tree.json               # decision-tree nodes and recommendations
  citations.json          # full reference text per leaf
  generate_citations.R    # rebuilds citations.json from a .bib file
  generate_mermaid.R      # rebuilds the static mermaid diagram
docs/                     # generated --- this is what GitHub Pages serves
  index.html              # rendered landing page
  site_libs/              # Quarto theme assets
  search.json             # Quarto search index
  tree/                   # shinylive bundle (the embedded app)
    index.html
    app.json
    shinylive/            # WebAssembly + JS assets
    shinylive-sw.js
```

The site is a small Quarto website. `index.qmd` renders to `docs/index.html` (the landing page) and embeds the shinylive app via an iframe at `tree/`. The shinylive bundle is a separate static export written into `docs/tree/`.

## Rebuilding the site

You need both `quarto` and the R `shinylive` package installed.

After editing the landing page (`index.qmd` or `_quarto.yml`):

```sh
quarto render
```

After editing anything in `app/` (the decision-tree app source):

```sh
# Stage source files outside the repo to avoid the appdir/destdir
# overlap that otherwise produces an empty bundle.
mkdir -p /tmp/shinylive_stage
cp app/app.R app/tree.json app/citations.json /tmp/shinylive_stage/
Rscript -e 'shinylive::export(
  appdir  = "/tmp/shinylive_stage",
  destdir = "docs/tree"
)'
```

A full rebuild (landing page + app), in this order:

```sh
quarto render
mkdir -p /tmp/shinylive_stage
cp app/app.R app/tree.json app/citations.json /tmp/shinylive_stage/
Rscript -e 'shinylive::export(appdir = "/tmp/shinylive_stage", destdir = "docs/tree")'
```

Run `quarto render` *before* the shinylive export --- a Quarto render may clean `docs/`, which would wipe the tree bundle.

## Previewing locally

```sh
cd docs
python3 -m http.server 8765
# then open http://127.0.0.1:8765/
```

Opening `docs/index.html` directly via `file://` will fail because of ES-module loading restrictions in the embedded shinylive iframe; a real HTTP server is required. First load of the tree takes 10--20 seconds while WebAssembly + R bootstrap.

## Deployment

GitHub Pages is configured to serve from the `main` branch, `/docs` folder. Pushing to `main` updates the live site within a minute or two.

## License

See [LICENSE](LICENSE).
