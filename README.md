# Comparison of Variable Importance Feature Selection Methods in Continuous-Response Random Forest

> **Archive notice.** This repository preserves an archived student project from the University of Bonn. It remains available as part of my academic trajectory and as an example for students considering a computational-statistics project; it is not part of my [current research portfolio](https://jakobjuergens.com/research/). Additional context is available in the [archive of earlier projects](https://jakobjuergens.com/previous-projects/).

## Project context

- **Course:** Final project in Computational Statistics
- **Institution:** University of Bonn
- **Term:** Summer semester 2021
- **Supervisor:** Marina Khismatullina, Ph.D.
- **Author:** Jakob R. Juergens

## Start here

- [`Project_Main.ipynb`](Project_Main.ipynb) is the final report and executable analysis notebook. It contains the literature review, methodological discussion, simulations, application, and embedded output; no separate PDF or HTML report is included.
- [`Auxiliary/sim_functions.R`](Auxiliary/sim_functions.R) contains the simulation and variable-selection functions sourced by the notebook. [`Auxiliary/Visualizations.R`](Auxiliary/Visualizations.R) contains its plotting helpers.
- [`Data/Application/`](Data/Application/) contains the bundled diabetes data and saved application results.
- [`Data/Simulation_1/Summary/`](Data/Simulation_1/Summary/) and [`Data/Simulation_2/Summary/`](Data/Simulation_2/Summary/) contain saved summaries for the two simulation exercises.
- The [contextualized project archive](https://jakobjuergens.com/previous-projects/) places this repository alongside other earlier academic projects.

## Overview

The project studies variable importance and feature selection in continuous-response random forests. It examines how CART-style splitting and commonly used importance rankings can favor predictors because of their scale, number of possible split points or categories, or position in a correlated design rather than because they contain more useful information about the response.

The report compares CART-style forests implemented with `randomForest` and conditional-inference forests implemented with `party`/`partykit`. The measures considered include out-of-bag permutation importance, node-purity importance, and unconditional and conditional permutation importance. It also studies null-importance testing procedures following Altmann et al. (2010), which permute the response, and Hapfelmeier and Ulm (2013), which permute predictors; the notebook discusses the resulting p-values and multiple-testing corrections.

Two controlled simulations compare these procedures under differences in predictor scale, category structure, noise, informativeness, and correlation. The application then uses the same feature-selection workflow on a diabetes data set and shows that different importance procedures can select different sets of variables. These comparisons are specific to the documented designs and application and are not intended as universal rankings of the methods.

## Repository guide

The report and all top-level analysis are in [`Project_Main.ipynb`](Project_Main.ipynb). Run it from the repository root because it sources functions from [`Auxiliary/`](Auxiliary/) and reads or writes relative paths under [`Data/`](Data/). The simulation design and reusable method implementations are concentrated in [`Auxiliary/sim_functions.R`](Auxiliary/sim_functions.R), while the real-data preparation, importance calculations, and feature-selection comparisons appear in the notebook's application section.

For students, the repository illustrates how a literature review and methodological exposition can be translated into controlled simulation designs and then applied to a real feature-selection problem. The saved summaries also make it possible to inspect reported results without repeating every expensive computation.

## Reproducibility

- The principal environment is **R in Jupyter**; the notebook metadata records R 3.6.1 and the IRkernel.
- The apparent execution entry point is [`Project_Main.ipynb`](Project_Main.ipynb), run from the repository root. It uses `randomForest`, `party`, `partykit`, `parallel`, `MASS`, `tidyverse`, `repr`, and `patchwork`.
- The diabetes data are bundled at [`Data/Application/diabetes.csv`](Data/Application/diabetes.csv); the notebook documents their original source and performs the cleaning used in the application.
- Package versions are not pinned in a lockfile or session-information record. Seeds are documented in the notebook for the simulations and null-importance calculations, but several long-running cells are disabled by default and read saved `.RDS` results instead.
- A full run was computationally intensive when the project was written. The repository has not been comprehensively modernized or rerun under current R and package versions, so current compatibility and exact end-to-end reproduction remain unverified.

## Retrospective note

This early project connects random forests, controlled simulation design, and feature selection. Its topic anticipates a later interest in random forests, but it does not contain or establish the asymptotic theory developed in my current research.
