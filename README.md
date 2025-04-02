# Running the agent

The agent implemented by Group 33 is in the `template-agent` directory.

- Test the agent through `run.py`, results will be returned as dictionaries and saved as json-file. A plot of the negotiation trace will also be saved.
- You can also test the agent more extensively by running a tournament with a set of agents. Use the `run_tournament.py` script for this. Summaries of the results will be saved to the results directory.

# Report generation

## Dependencies

- R with the `knitr`, `scales`, and `reticulate` packages. *(get these by installing RStudio)*
- LaTeX
- Python
- the ANAC repository to test

## Usage

### To generate a report of all already completed runs

Open the `results_report.Rmd` R Markdown file in RStudio and click on "Knit".

### To generate a performance benchmark of an agent

Not implemented.
