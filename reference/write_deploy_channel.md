# Record which app the deploy is for

Used by `just deploy` to write `deploy.yml` into `inst/app-data/`, so
the deployed app knows whether it is the beta app (see
[`get_app_channel()`](https://bennettoxford.github.io/opencodecounts/reference/get_app_channel.md)).

## Usage

``` r
write_deploy_channel(dest_dir, channel = c("main", "beta"))
```

## Arguments

- dest_dir:

  String, directory with the deployed app data

- channel:

  String, "main" or "beta"

## Value

Invisible path of the written file
