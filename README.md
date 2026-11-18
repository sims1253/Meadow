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
  `set.seed(seed)`. They depend on the state of the random number generator
  when the function starts, not only on `seed`. Two calls with the same `seed`
  can return different parameters and different data.
- `basedag_data_noisy()` can draw a negative standard deviation. With a
  Gaussian outcome, if it does so for `z1`, `z2`, `z3` or `x`, every value in
  the returned data set is `NA`. In 2000 calls with a Gaussian outcome, every
  standard deviation at 1 and the default `noise_sd = 1`, about half returned
  such a data set.
- `basedag_data_noisy()` passes `noisy_sigma_y` to the random number generator
  of the family as its third positional argument. This is correct for
  `"gaussian"`. With bayesfam 0.3.2, a family with two auxiliary parameters
  gets only the first. The second keeps the default of the generator if it has
  one. `"generalized_gamma"` fails because its `Q` has none. `"unit_lindley"`
  has no auxiliary parameter: the function never returns when `noisy_sigma_y`
  is `NULL`, its default, and it fails with an unused-argument error when
  `noisy_sigma_y` has a value.
- `basedag_data()` and `basedag_data_noisy()` discard outcomes outside `lb` and
  `ub`. The remaining rows no longer follow the coefficients in
  `data_gen_output`. For example, take `basedag_data()` with a Gaussian outcome,
  the identity link, `lb = 0`, and the intercept, every coefficient and every
  standard deviation at 1. With a million rows, `lm(y ~ x + z1 + z2)` then
  estimates the effect of `x` as about 0.87.
- `data_gen_conf_y_analysis()` prints empty values for `y_intercept` and
  `y_sigma`. It reads them from `data_gen_conf$data_gen_output`, a field that
  configurations do not have.
- The generators call bayesfam, which is not on CRAN, but `DESCRIPTION` lists
  it only under `Suggests`. Without bayesfam, `basedag_data_noisy()` always
  fails, and `basedag_data()` fails when `data_family` or `data_link` is a
  string.
- `R CMD check` fails. The help page for `hello()` has an example, but the
  package has no such function. The help pages cause four more warnings. The
  two GitHub workflows never passed and have been removed.

## Alternatives

To simulate data from a causal graph, use the CRAN package
[simDAG](https://cran.r-project.org/package=simDAG).

## Installing the last version

The commands below install the current bayesfam and the last code commit of
Meadow from GitHub. The package installs under the name `meadow`. Calls written
in 2023 may need the bayesfam of that time.

```r
remotes::install_github("sims1253/bayesfam")
remotes::install_github("sims1253/Meadow@9ec6b0f3785238ca5d3c26da82346885a2e4e774")
```

## Licence

GPL-3. See [LICENSE](LICENSE).
