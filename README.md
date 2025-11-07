# Wolf Text Analysis – Topic Modeling Script

## Overview
This R script performs topic modeling and comparative text analysis on a dataset of wolf-related articles and quotes. It uses the **Structural Topic Model (STM)** framework to identify dominant topics across two key dimensions:

1. **Political actor analysis** – comparing language among *Judicial*, *Executive*, *Legislative*, *Public/Private*, and *NGO* branches.  
2. **Temporal analysis** – comparing article content *before*, *during*, and *after* major policy or management events.

Both analyses produce topic model summaries, diagnostic statistics (semantic coherence and exclusivity), and publication-ready coefficient plots.


## Dependencies
The following R packages are required:

library(stm)
library(tm)
library(SnowballC)
library(Rtsne)
library(rsvd)
library(geometry)
library(scales)


## Input Data

The script expects a CSV file named: wolf_article_data_3_31_25.csv located in the working directory 

### Expected columns

| Column Name      | Description                                                                                |
| ---------------- | ------------------------------------------------------------------------------------------ |
| `Number`         | Unique identifier for each article or quote                                                |
| `Date_Published` | Publication date in `MM/DD/YY` or `MM/DD/YYYY` format                                      |
| `Branch`         | Political actor category (`Judicial`, `Executive`, `Legislative`, `Public/Private`, `NGO`) |
| `Relevant`       | Inclusion flag (`Y`/`N`) for analysis                                                      |
| `Quote`          | Extracted quote text                                                                       |
| `Content`        | Full article text                                                                          |
| `Date_bin`       | Temporal bin (`Before`, `During`, `After`)                                                 |

The script:

* Cleans non-printing characters and punctuation.
* Parses publication dates into numeric sequence values (`Seqdate`).
* Filters to rows marked as relevant (`Relevant == "Y"`).
* Creates two subsets:

  * `quotes` – short statements
  * `articles` – full text


## Workflow Summary

### 1. Data Preparation

* Converts publication dates into numeric sequence (`Seqdate`) and standardized date codes (`YYYYMMDD`).
* Filters to relevant records and branch categories.
* Constructs metadata frames for STM modeling.

### 2. Topic Modeling by Political Actor

* Runs STM (`K = 5` topics) on quote data, using *political actor* as the prevalence covariate.
* Evaluates multiple model runs using `selectModel()` to identify the best configuration.
* Outputs:

  * Model summaries and diagnostics → `output_actors/output_10_combined.txt`
  * Topic coefficient plots → `output_actors/output_10_topicX_combined.pdf`

### 3. Topic Modeling by Time Period

* Runs STM (`K = 5`) on full article text, using *time period* (`Before`, `During`, `After`) as the covariate.
* Outputs summary text and coefficient plots to the `output_time/` directory.


## Output Files

After successful execution, the following directories and files are created:

output_actors/
  ├── output_10_combined.txt         # Model summary and diagnostics (quotes)
  ├── output_10_topic1_combined.pdf  # Topic 1 coefficient plot by actor
  ├── ...                            # Topics 2–5

output_time/
  ├── output_10_combined.txt         # Model summary and diagnostics (articles)
  ├── output_10_topic1_combined.pdf  # Topic 1 coefficient plot by time
  ├── ...                            # Topics 2–5

Each `.txt` file includes:

* Top words per topic
* Semantic coherence and exclusivity metrics
* Regression summaries of topic prevalence by category


## Customization

* **Thresholding:** Adjust `lowerthreshold` to remove infrequent terms (default = 10).
* **Number of topics:** Modify `K` in both `stm()` and `selectModel()` calls.
* **Stopwords:** Update `customstopwords` to exclude domain-specific filler words.
* **Reproducibility:** The random seed (`2112`) ensures consistent results.
