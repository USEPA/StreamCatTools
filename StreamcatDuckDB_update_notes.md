# StreamCat DuckDB update notes

Branch: `feature/duckdb-streamcat` · Implementation commit: `b4ad253`

`sc_get_data()` now reads named StreamCat metrics directly from Parquet at `s3://dmap-data-commons-ow/data/streamcat/lake_full` using DuckDB. Existing calls need no new arguments or connection setup. Results retain API-style column names such as `comid` and `bfiws`; numeric values use the precision stored in Parquet. NLDI validation remains off by default.

## Which backend handles a request?

| DuckDB by default | EPA API for compatibility |
| --- | --- |
| Named StreamCat metrics by COMID, state, hydroregion, or CONUS | County queries, `countOnly = TRUE`, `showPctFull = TRUE`, and `metric = "all"` |
| `sc_get_params()` for `metric_names`, `aoi`, or `region` | Other metadata, including variable descriptions and county lookups |
| NNI components available in the lake | The full NNI bundle and requests containing known API-only NNI components; LakeCat metric requests |

A DuckDB error is reported to the caller, not silently retried through the API. To compare results with the old backend, set `options(StreamCatTools.backend = "api")`. Use `options(StreamCatTools.backend = NULL)` to restore the default. To query a local copy of the lake, set `StreamCatTools.lake_root` to the directory containing `metric_catalog.parquet` and `comid_index.parquet`.

## Try it from the branch

```r
devtools::load_all()
sc_get_data(comid = 718276, metric = "bfi", aoi = "ws")
```

## Verification during the port

All 32 DuckDB expectations passed, including a live single-COMID query against the public S3 bucket. A package check excluding vignettes and the PDF manual reported no errors, warnings, or notes.

The full live suite did not pass: some API-backed tests received missing results while the EPA service returned HTTP 500/503 responses. The suite also identified an NNI plotting regression. After correcting its routing, the focused plot test passed with one EPA 503 warning; the full suite was not rerun after that correction.
