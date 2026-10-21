# rep-pnas

Folder-backed replication of the PNAS manuscript *Digital literacy alters engagement with emotionally manipulative political content on social media*. No article DOI. The study handle is `rep-pnas`.

Analysis steps follow `TikTok_gh/2_analysis.qmd`. This folder reads and writes only inside `rep-pnas`.

## Run

From this directory, with replicateEverything installed:

```r
list_replications("here")
run_replication("here", "tab_s4_experiment")
run_replication("here", "tab_s4_experiment", format = TRUE)
```

`given = "parents"` is the default. Run a parent step before a child that reads it (`repeated_videos`, `type_contrasts`, `afd_3way`, `pid_transitions`). `key_numbers` reads baked objects from Figure 1, Figure 2, Figure S14, Table S9, Table S12, and `repeated_videos`.

Figure 2, Tables S5–S8 (`type_contrasts`), and Tables S10–S11 (`afd_3way`) fit video fixed-effects models on the wave files and are slow.

## Layout

`replication.yml` declares the step DAG. `code/` holds one script per step. Shared code is split into `code/helpers_labels.R`, `code/helpers_graphing.R`, and `code/helpers_estimation.R`; a step sources only the files it calls. Products go to `outputs/`. Benchmarks are in `tests/goldens/`.
