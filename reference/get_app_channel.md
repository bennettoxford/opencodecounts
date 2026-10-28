# Which app is running

Reads the `deploy.yml` that
[`write_deploy_channel()`](https://bennettoxford.github.io/opencodecounts/reference/write_deploy_channel.md)
writes during `just deploy`. Local runs have no such file, so they count
as main.

## Usage

``` r
get_app_channel(
  path = system.file("app-data", "deploy.yml", package = "opencodecounts")
)
```

## Arguments

- path:

  String, path of `deploy.yml`, or "" when there is none

## Value

"main" or "beta"
