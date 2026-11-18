# Meadow

Meadow is an R package that simulates data from one small, hand-coded causal
graph. In that graph `z1` causes both the treatment `x` and the outcome `y`,
`z3` causes only `x`, `z2` causes only `y`, and `z4` is a collider of `x` and
`y`. Meadow was written in 2023 to supply data sets to simulation studies in
bayesim.

## Status

Meadow is retired and nobody maintains it. bayesim never called it, and the
generators have the defects listed below. The repository stays online so that
people can reproduce old results.

## Known defects

- `basedag_data_noisy()` ignores `noisy_y_intercept`. The outcome uses
  `y_intercept`, but `data_gen_output` reports the value of `noisy_y_intercept`.
- `basedag_data_noisy()` draws its noisy parameters before it calls
  `set.seed(seed)`. Two calls with the same `seed` return different parameters
  and different data.
- `basedag_data_noisy()` can draw a negative standard deviation. With the
  default `noise_sd = 1`, about half of all calls return a `y` column that is
  entirely `NaN`.
- `basedag_data_noisy()` passes `noisy_sigma_y` to the random number generator
  of the family as its third positional argument. This is correct for
  `"gaussian"`. For `"student"` the value becomes the degrees of freedom and
  the scale stays at 1.
- `basedag_data()` and `basedag_data_noisy()` discard outcomes outside `lb` and
  `ub`. The remaining rows no longer follow the coefficients in
  `data_gen_output`. With `x_y_coef = 1`, `y_intercept = 1`, `lb = 0` and an
  outcome standard deviation of 1, a linear regression recovers about 0.86.
- `data_gen_conf_y_analysis()` prints empty values for `y_intercept` and
  `y_sigma`. It looks for them in the configuration, which does not contain
  them.
- The generators call bayesfam, which is not on CRAN, but `DESCRIPTION` lists
  it only under `Suggests`. Without bayesfam, `basedag_data_noisy()` always
  fails, and `basedag_data()` fails when `data_family` or `data_link` is a
  string.
- `R CMD check` fails. The help page for `hello()` has an example, but the
  package has no such function. The help pages cause four more warnings. The
  two GitHub workflows never passed and have been removed.

## Alternatives

To simulate data from a causal graph, use the CRAN package
[simDAG](https://cran.r-project.org/package=simDAG). bayesim plans a
`dag_generator(dag, estimands, ...)` built on simDAG that computes true estimand
values by intervention (`do()`). That function does not exist yet.

## Installing the last version

To reproduce old work, install bayesfam and the last code commit of Meadow from
GitHub. The package installs under the name `meadow`.

```r
remotes::install_github("sims1253/bayesfam")
remotes::install_github("sims1253/Meadow@9ec6b0f3785238ca5d3c26da82346885a2e4e774")
```

## Licence

GPL-3. See [LICENSE](LICENSE).
