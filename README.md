# Many-Facet Rasch Model (MFRM) Analysis Toolkit

This project now ships as a two-tier application:

1. **Backend** – an asynchronous R API (Plumber + `brms`) that performs data validation, CLMM fitting, and result serialisation (`backend/`).
2. **Frontend** – a static HTML/JavaScript client (hostable on GitHub Pages) that mirrors the original Shiny workflow and consumes the API (`docs/`, source in `frontend/`).

The re-design keeps every analytic feature of the legacy Shiny app—data preparation helpers, model configuration, posterior summaries, reliability/separation indices, residual PCA, fit diagnostics, Wright map, and CSV downloads—while avoiding the long Stan compilation timeouts that broke browser sessions.

---

## Repository Layout

| Path              | Description                                                                            |
| ----------------- | -------------------------------------------------------------------------------------- |
| `app.R`         | Historical Shiny app (kept for reference).                                             |
| `backend/`      | Plumber API, async job manager, analysis modules, and helper scripts.                  |
| `backend/R/`    | Modularised R code: data prep, analysis pipeline, serializers, download builders, etc. |
| `backend/jobs/` | Runtime job cache (ignored by git).                                                    |
| `docs/`         | Production-ready static site (served by GitHub Pages).                                 |
| `frontend/`     | Editable source for the SPA (mirrors `docs/`, easier to tweak locally).              |

---

## Backend (Plumber API)

### Requirements

Install the required R packages (Stan/`brms` toolchain must already be configured):

```r
install.packages(c(
  "plumber", "brms", "posterior", "loo", "future", "uuid", "jsonlite",
  "dplyr", "tibble", "tidyr", "stringr", "purrr", "readr", "glue", "psych",
  "R6", "RColorBrewer"
))
```

> Tip: use `renv` or a project-specific library for reproducibility.

### Start the API locally

```sh
./backend/run_api.R      # defaults to 0.0.0.0:8000
```

Environment variables:

| Variable | Default     | Meaning       |
| -------- | ----------- | ------------- |
| `HOST` | `0.0.0.0` | Bind address. |
| `PORT` | `8000`    | Listen port.  |

The API exposes:

| Method & Route                         | Purpose                                                                                  |
| -------------------------------------- | ---------------------------------------------------------------------------------------- |
| `GET /health`                        | Liveness probe.                                                                          |
| `GET /template`                      | Template metadata for the client.                                                        |
| `GET /template/sample?format=csv       | tsv`                                                                                     |
| `POST /jobs`                         | Submit a modelling job (payload: data + column/mode config).                             |
| `GET /jobs`                          | List job IDs seen since startup.                                                         |
| `GET /jobs/{id}`                     | Poll job status (`queued`, `running`, `finished`, `failed`).                     |
| `GET /jobs/{id}/result`              | Retrieve full analysis payload (tables, plot data, metadata).                            |
| `GET /jobs/{id}/download/{resource}` | Stream CSV extracts (`summary`, `reliability`, `fit`, `thresholds`, `facets`). |

Internally, the API:

- Validates input data, removes missing IDs/responses, drops constant facets, and reports warnings.
- Fits a cumulative link mixed model via `brms` with configurable link/threshold settings.
- Summarises posteriors, reliability/separation, residual diagnostics, PCA, Wright map data, etc.
- Serialises lightweight JSON so the frontend can render tables/plots without `gt`/`ggplot2` on the client.
- Executes jobs asynchronously using `future::multisession`, caching job artefacts under `backend/jobs/runtime/`.

---

### Deploying the API (Render example)

The repository ships with a Docker build that bundles all R dependencies, CmdStan, and the Plumber entrypoint. Any container host will work; here is a beginner-friendly Render workflow:

1. Push this repository (or a fork) to GitHub so Render can access it.
2. In Render, choose **New → Web Service**, connect your GitHub account, and select the repo.
3. When prompted for environment, pick **Docker** (the default `Dockerfile` at the repo root is used automatically).
4. Add one environment variable: `HOST=0.0.0.0`. Leave `PORT` blank—Render injects it dynamically and `backend/run_api.R` already honours it.
5. Create the service. The first build compiles the R packages (including `brms`/`rstan`) and typically finishes within a couple of minutes because the Docker image now starts from the public `rocker/r-ver` base.
6. Once the service status turns **Live**, visit `<your-service>.onrender.com/health` and confirm you receive `{"status":"ok"}`.
7. Copy the base URL (without a trailing slash) into the GitHub Pages app’s `API Endpoint` field and press **保存**. The SPA now talks to the hosted API even if your local R session is closed.

Render rebuilds automatically on every git push. Other container providers (Fly.io, Azure Container Apps, ECS/Fargate, etc.) can reuse the same `Dockerfile`; expose port `8000` and keep `BRMS_BACKEND=rstan` unless you later decide to experiment with `cmdstanr` in an environment that allows longer build times.

---

## Frontend (Static SPA)

### Development

The editable source lives in `frontend/`. After changing the markup/scripts, copy the assets into `docs/` (GitHub Pages serves the `docs/` directory).

### Local Preview

```sh
# Any static server works; e.g. using Python 3
cd docs
python3 -m http.server 4000
```

Open `http://localhost:4000/` and, in the header, set the API endpoint (e.g. `http://localhost:8000`).

### Features

- Recreates the five guided steps from the Shiny UI (data entry → configuration → mapping → model options → run).
- Supports paste/upload workflows, template helpers, row appends, and sample downloads.
- Step 3 clarifies that the `受検者 ID` column is included automatically, so only additional facets (評定者・課題など) need checkmarks.
- Polls the backend for job completion, then mirrors every output tab: data preview, model summary/diagnostics, facet parameters, reliability & separation, fit statistics, residual PCA, thresholds/probability curves, residual plots, Wright map, and CSV downloads.
- Uses Plotly.js and PapaParse via CDN, no build step required.

---

## Typical Workflow

1. Start the API (`backend/run_api.R`).
2. Serve the SPA from `docs/` (locally or via GitHub Pages). Configure `API Endpoint` in the header.
3. Paste/upload data, map columns, choose link/threshold options, and run the analysis.
4. Monitor status, inspect plots/tables, and export CSVs through the downloads tab.

Jobs persist for the lifetime of the API process; restart the service to clear cached results.

---

## Migrating from the Shiny App

- The historical Shiny implementation remains in `app.R` for reference but is no longer required for deployment.
- All server-side logic was moved into modular functions under `backend/R/`, enabling reuse in scheduled pipelines or other clients.
- Frontend responsibilities (UI state, plotting, downloads) shifted to the static SPA.
- Long-running Stan compilation now happens server-side with warm-start potential; the browser only polls job status, preventing timeouts.

---

## Next Steps & Ideas

- Enable `cmdstanr` for faster compilation and model caching (requires adjusting the Dockerfile and additional build time).
- Add authentication (API keys, JWT) before exposing the backend publicly.
- Containerise the API (e.g. `rocker/brms`) and front-end (static hosting) for reproducible deployment.
- Extend the job manager with persistent storage (SQLite/Postgres) if multi-user history is needed.
- Add automated tests (e.g. `testthat`) around `prepare_analysis_data()` and `run_mfrm_analysis()`.

---

## Support

Please open issues or PRs if you encounter problems migrating existing workflows to the new architecture.
