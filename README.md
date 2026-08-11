# Gaming vs Academics Project 
INFO 432 - Group 5

Analyzing the relationship between student gaming habits and academic performance using a dataset of 8,000 students. The project investigates which lifestyle factors (gaming hours, study hours, sleep, attendance, stress) most strongly predict grades, and identifies threshold points where performance noticeably shifts.

**Team:** Jeffrey Cheung, Drew Jones, Danny Eapen, Joel Thomas

**Dataset:** [Kaggle - Gaming vs Academic Performance](https://www.kaggle.com/datasets/nalisha/gaming-vs-academic-performance)

## Project Structure
- `data/` — raw and cleaned datasets
- `scripts/` — R scripts for cleaning, EDA, and analysis
- `output/` — generated plots and figures
- `report/` — final writeup (`final_report.Rmd` and the knitted `final_report.html`)

## Final report

`report/final_report.Rmd` combines the whole project — cleaning, EDA, statistical
testing, threshold analysis, and modeling — into one document that runs end to end from
the raw CSV and knits to a self-contained HTML file. It also regenerates every plot and
CSV in `output/`, so it is the single source of truth for the results.

To build it, either open `report/final_report.Rmd` in RStudio and click **Knit**, or run
from the project root:

```bash
Rscript scripts/render_report.R
```

Takes about two minutes; most of that is the one-way ANOVA in section 4.4, which is kept
only for the method-comparison table. Set `RUN_ANOVA <- FALSE` in the setup chunk for a
fast knit that skips it.

**Requires:** R with `tidyverse`, `rmarkdown`, `knitr`, `broom`, `rpart`, `rpart.plot`,
`corrplot`, and pandoc (bundled with RStudio; `render_report.R` finds it automatically).
