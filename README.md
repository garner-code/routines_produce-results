# Produce results for 'Towards a normative theory of routines'

K. Garner (2025). *Towards the definition and measurement of routines and their impact on cognitive control.*

This repository provides a computationally reproducible record of the analyses and figures reported in the paper above. It is intended to allow others to reproduce all results from the raw data.

**If you want a quick overview of the analysis and the code used to produce it, you can follow along [here](https://garner-code.github.io/routines_produce-results/).**

## Overview

The analyses quantify routine behaviour using a transition-entropy (TE / R-score) measure, then ask four main questions:

| Question | Summary |
|---|---|
| **Q1** | Can we reliably elicit routines, and are R scores above chance and stable across sessions? |
| **Q2** | Can we systematically manipulate how routine someone is via a training manipulation (stable vs. variable contexts)? |
| **Q3** | What kinds of responses explain the difference in routines between training groups? |
| **Q4** | How do routines affect learning transfer and task-switching performance? |

## Repository structure

```
routines_produce-results/
├── _quarto.yml                        # Quarto project configuration
├── routines_produce-results.Rproj     # RStudio project file
├── routines_produce-results.qmd       # Main executable notebook (entry point)
├── R/                                 # R analysis scripts
│   ├── ent_functions.R                # Core entropy / routine-score functions
│   ├── ent_initial-data-sort.R        # Sort raw trial data into contexts
│   ├── ent_compute-routine-measure.R  # Compute R scores per participant
│   ├── ent_compute-zs.R               # Compute z-scores against null distributions
│   ├── ent_generate-nulls.R           # Generate permutation null distributions
│   ├── ent_generate-random-agent.R    # Simulate a perfectly random agent
│   ├── ent_apply-impact-models.R      # Logistic regression: what drives choices?
│   ├── ent_impact_scnd-lvl.R          # Second-level beta-coefficient analysis
│   ├── ent_learning-transfer.R        # Learning-transfer analysis
│   ├── ent_reliability_analysis.R     # Reliability of R scores across sessions
│   ├── ent_compare-rs-cond.R          # Compare R scores across conditions
│   └── plot_*.R                       # Visualisation functions (10 files)
├── data-wrangled/                     # (not tracked) pre-processed data files
├── figs/                              # (not tracked) generated figures
├── sims/                              # (not tracked) simulation outputs
├── res/                               # (not tracked) inferential results / stats
└── analysis-output/                   # (not tracked) additional analysis outputs
```

## Prerequisites

### Software

- [R](https://www.r-project.org/) (≥ 4.0 recommended)
- [Quarto](https://quarto.org/) (to render the notebook)
- [RStudio](https://posit.co/products/open-source/rstudio/) (optional, but recommended)

### R packages

Install the required packages from CRAN before rendering the notebook:

```r
install.packages(c(
  "tidyverse", "grid", "gridExtra", "knitr", "magick",
  "ggpubr", "vioplot", "rstatix", "emmeans", "afex",
  "pdftools", "purrr", "GGally", "extrafont", "here"
))
```

## Data

The analysis requires pre-processed data files that are **not** stored in this repository. They must be placed in the `data-wrangled/` directory before running the notebook.

| File | Source repository |
|---|---|
| `exp_lt_evt.csv`, `exp_ts_evt.csv` | [garner-code/doors](https://github.com/garner-code/doors) |
| `exp_lt_avg.csv`, `exp_ts_avg.csv` | [garner-code/doors](https://github.com/garner-code/doors) |
| `exp_lt_maggi-k4.csv` | [garner-code/doors](https://github.com/garner-code/doors) |
| `dat4_seq_model.Rda` | [garner-code/DA_VisRoutes](https://github.com/garner-code/DA_VisRoutes) |

You will also need to create the output directories:

```bash
mkdir -p data-wrangled figs sims res analysis-output
```

## Reproducing the results

Open `routines_produce-results.qmd` in RStudio and click **Render**, or run the following from the repository root:

```bash
quarto render routines_produce-results.qmd
```

This will:
1. Source all R scripts in `R/`.
2. Sort raw trial data into per-participant, per-context sequences.
3. Compute transition-entropy R scores for each participant.
4. Generate permutation null distributions and z-scores.
5. Simulate a perfectly random agent for comparison.
6. Run logistic regression models to identify behavioural drivers.
7. Produce all manuscript and talk figures in `figs/`.
8. Save inferential statistics to `res/`.
9. Output the rendered document as `index.html`.

> **Note:** Some code blocks (null distribution generation, random-agent simulation) are marked `eval=FALSE` because they are computationally expensive. Pre-computed outputs are expected to exist in `sims/` and `data-wrangled/` when those blocks are skipped.

## Output files

| Path | Description |
|---|---|
| `index.html` | Rendered analysis notebook |
| `figs/trajectories.svg` | Example participant door-selection trajectories |
| `figs/z-hsts_ms.svg` / `z-hsts_tlk.svg` | Z-score histograms vs. random agent |
| `figs/reliability_data*.svg` | Reliability of R scores across sessions |
| `figs/rs_by-traintype_hists*.svg` | R scores by training group |
| `figs/task-jumps*.svg` | Task-jump rates by group |
| `figs/FigA.pdf`, `FigB.pdf`, … | Composite manuscript figures |
| `res/exp_*_zs_cl_inf-test.csv` | t-tests of z-scores against zero |
| `res/human_v_agent_zs.csv` | Human vs. random-agent z-score comparison |
| `res/grp_r_ts.csv` | Group comparison of routine scores |
| `data-wrangled/exp_*_rscore.csv` | Per-participant R scores |
| `data-wrangled/exp_*_rnulls.csv` | Permutation null distributions |
| `data-wrangled/exp_*_zs_cl.csv` | Per-participant z-scores |

## Citation

If you use this code, please cite:

> K. Garner (2025). *Towards the definition and measurement of routines and their impact on cognitive control.*

## Contact

For questions about the code or analyses, please open an issue in this repository.
