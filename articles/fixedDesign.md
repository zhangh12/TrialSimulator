# An Example of Fixed Design with Two Correlated Endpoints

In this vignette, we illustrate how to use `TrialSimulator` to simulate
a trial of two correlated endpoints. Under a fixed design, only one
milestone is specified for final analysis.

## Simulation Settings

- Trial consists of two active arms of high and low dose, and a
  standard-of-care arm.

- Patients are randomized into the trial with ratio `1:1:1`.

- 30 patients are randomized per month for the first 10 months, and 50
  patients per month after then until 1000 patients.

- Dropout rate is 10% at month 18, which is modeled by an exponential
  distribution, with rate parameter `-log(1 - 0.1)/18`.

- Modeled by a three-state illness-death model, two endpoints `PFS` and
  `OS` are simulated.

  - Primary endpoint `OS` has a medians of 15, 18.5, 20 months in
    standard-of-care, low dose and high dose arms, respectively.
  - Key secondary endpoint `PFS` has a median of 7, 9, 10 months in the
    three arms.
  - Pearson’s correlation between `OS` and `PFS` are 0.68, 0.65, and
    0.60 in the three arms.

- To ensures sufficient powers for both endpoints, final analysis is set
  when we have at least a total of 800 `PFS` events in the
  standard-of-care and high dose arm, meanwhile a total of 550 `OS`
  events are observed in three arms

  - `OS` is tested using one-sided logrank test.
  - `PFS` is tested using one-sided p-value from the Cox proportional
    hazard model as the PH assumption is assumed.
  - The Bonferroni correction is adopted to split overall
    $`\alpha = 5\%`$, i.e., claiming significant effect for an endpoint
    when p-value is lower than $`0.05/4`$.

## Transition Hazards of `PFS` and `OS`

We adopt the illness-death model to simulate the two endpoints. This
ensures `PFS` $`\leq`$`OS` with probability one, and makes no assumption
on latent variables or copula parameters. `TrialSimulator` offers a
function
[`solveThreeStateModel()`](https://zhangh12.github.io/TrialSimulator/reference/solveThreeStateModel.md)
to convert endpoints’ medians and correlation to the transition hazards,
which are required by the built-in generator
[`CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md).
Refer to the vignette [Simulate Correlated Progression-Free Survival and
Overall Survival as Endpoints Under Illness-Death
Model](https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOsIdm.md).
Note that data generated under the illness-death model may induce a
time-varying OS hazard ratio between treatment arms, violating the
proportional hazards (PH) assumption in a Cox model. For a data model
that is more suitable for PH Cox analysis, refer to the built-in
generator
[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
and vignette of the [Gumbel copula
method](https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOsGumbel.md).

``` r

pars_soc <- solveThreeStateModel(median_pfs = 7, median_os = 15, corr = .68, 
                                 h12 = seq(.07, .10, length.out = 50))
pars_soc
#>   corr        h01       h02        h12        error
#> 1 0.68 0.07498342 0.0240376 0.08959184 0.0001363832
```

``` r

pars_low <- solveThreeStateModel(median_pfs = 9, median_os = 18.5, corr = .65, 
                                 h12 = seq(.04, .07, length.out = 50))
pars_low
#>   corr        h01        h02        h12       error
#> 1 0.65 0.05059501 0.02642134 0.06204082 0.001646964
```

``` r

pars_high <- solveThreeStateModel(median_pfs = 10, median_os = 20, corr = .60, 
                                  h12 = seq(.02, .06, length.out = 50))
pars_high
#>   corr        h01        h02        h12      error
#> 1  0.6 0.03964373 0.02967099 0.04693878 0.00208503
```

| arm  |   h01 |   h02 |   h12 |
|:-----|------:|------:|------:|
| soc  | 0.075 | 0.024 | 0.090 |
| low  | 0.051 | 0.026 | 0.062 |
| high | 0.040 | 0.030 | 0.047 |

Transition hazards in three treatment arms {.table}

## Define Treatment Arms

We use generator
[`CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md)
with
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
and
[`arm()`](https://zhangh12.github.io/TrialSimulator/reference/arm.md) to
define the three treatment arms.

``` r

#' define SoC
pfs_os_in_soc <- endpoint(name = c('pfs', 'os'), 
                          type = c('tte', 'tte'), 
                          generator = CorrelatedPfsAndOs3, 
                          h01 = 0.075, h02 = 0.024, h12 = 0.090)

soc <- arm(name = 'soc')
soc$add_endpoints(pfs_os_in_soc)

#' define low dose arm
pfs_os_in_low <- endpoint(name = c('pfs', 'os'), 
                          type = c('tte', 'tte'), 
                          generator = CorrelatedPfsAndOs3, 
                          h01 = 0.051, h02 = 0.026, h12 = 0.062)

low <- arm(name = 'low')
low$add_endpoints(pfs_os_in_low)

#' define high dose arm
pfs_os_in_high <- endpoint(name = c('pfs', 'os'), 
                           type = c('tte', 'tte'), 
                           generator = CorrelatedPfsAndOs3, 
                          h01 = 0.040, h02 = 0.030, h12 = 0.047)

high <- arm(name = 'high')
high$add_endpoints(pfs_os_in_high)
```

We can request for a summary report of, e.g., the high dose arm, by
printing the arm object in R console. The medians of `PFS` and `OS`
matches to the settings very well.

``` r

high
```

CjwhRE9DVFlQRSBodG1sPgo8aHRtbD4KPGhlYWQ+CiAgICA8bWV0YSBjaGFyc2V0PSJVVEYtOCI+CiAgICA8dGl0bGU+QXJtIE5hbWU6IGhpZ2g8L3RpdGxlPgogICAgPHN0eWxlPgogICAgICAgIGJvZHkgewogICAgICAgICAgICBmb250LWZhbWlseTogQXJpYWwsIHNhbnMtc2VyaWY7CiAgICAgICAgICAgIG1hcmdpbjogMjBweDsKICAgICAgICAgICAgYmFja2dyb3VuZC1jb2xvcjogd2hpdGU7CiAgICAgICAgICAgIGRpc3BsYXk6IGZsZXg7CiAgICAgICAgICAgIGZsZXgtZGlyZWN0aW9uOiBjb2x1bW47CiAgICAgICAgICAgIGFsaWduLWl0ZW1zOiBjZW50ZXI7CiAgICAgICAgfQogICAgICAgIGgxIHsKICAgICAgICAgICAgY29sb3I6IGJsYWNrOwogICAgICAgICAgICB0ZXh0LWFsaWduOiBjZW50ZXI7CiAgICAgICAgICAgIG1hcmdpbi1ib3R0b206IDIwcHg7CiAgICAgICAgICAgIGZvbnQtc2l6ZTogMjBweDsKICAgICAgICB9CiAgICAgICAgLnN1YnRpdGxlIHsKICAgICAgICAgICAgdGV4dC1hbGlnbjogY2VudGVyOwogICAgICAgICAgICBjb2xvcjogIzY2NjsKICAgICAgICAgICAgbWFyZ2luLWJvdHRvbTogMjBweDsKICAgICAgICAgICAgZm9udC1zaXplOiAxNnB4OwogICAgICAgIH0KICAgICAgICB0YWJsZSB7CiAgICAgICAgICAgIGJvcmRlci1jb2xsYXBzZTogY29sbGFwc2U7CiAgICAgICAgICAgIGZvbnQtc2l6ZTogMTRweDsKICAgICAgICAgICAgYm9yZGVyOiAxcHggc29saWQgIzk5OTsKICAgICAgICAgICAgd2lkdGg6IGF1dG87CiAgICAgICAgICAgIG1hcmdpbjogMCBhdXRvOwogICAgICAgIH0KICAgICAgICB0aCB7CiAgICAgICAgICAgIGJhY2tncm91bmQtY29sb3I6ICNmMGYwZjA7CiAgICAgICAgICAgIGNvbG9yOiBibGFjazsKICAgICAgICAgICAgcGFkZGluZzogMTBweDsKICAgICAgICAgICAgdGV4dC1hbGlnbjogbGVmdDsKICAgICAgICAgICAgZm9udC13ZWlnaHQ6IG5vcm1hbDsKICAgICAgICAgICAgYm9yZGVyOiAxcHggc29saWQgIzk5OTsKICAgICAgICAgICAgd2hpdGUtc3BhY2U6IG5vd3JhcDsKICAgICAgICAgICAgZm9udC1zaXplOiAxNHB4OwogICAgICAgIH0KICAgICAgICB0ZCB7CiAgICAgICAgICAgIHBhZGRpbmc6IDEwcHg7CiAgICAgICAgICAgIGJvcmRlcjogMXB4IHNvbGlkICM5OTk7CiAgICAgICAgICAgIHZlcnRpY2FsLWFsaWduOiB0b3A7CiAgICAgICAgICAgIGxpbmUtaGVpZ2h0OiAxLjQ7CiAgICAgICAgICAgIGZvbnQtc2l6ZTogMTRweDsKICAgICAgICB9CiAgICAgICAgLm5vLWNvbCB7CiAgICAgICAgICAgIHRleHQtYWxpZ246IGNlbnRlcjsKICAgICAgICAgICAgd2hpdGUtc3BhY2U6IG5vd3JhcDsKICAgICAgICB9CiAgICAgICAgLnZhcmlhYmxlLWNvbCB7CiAgICAgICAgICAgIHdoaXRlLXNwYWNlOiBub3dyYXA7CiAgICAgICAgfQogICAgICAgIC5zdGF0cy1jb2wgewogICAgICAgIH0KICAgICAgICAuZnJlcXMtY29sIHsKICAgICAgICAgICAgbGluZS1oZWlnaHQ6IDIwcHg7CiAgICAgICAgfQogICAgICAgIC5ncmFwaC1jb2wgewogICAgICAgICAgICB0ZXh0LWFsaWduOiBjZW50ZXI7CiAgICAgICAgICAgIHdoaXRlLXNwYWNlOiBub3dyYXA7CiAgICAgICAgICAgIHZlcnRpY2FsLWFsaWduOiB0b3A7CiAgICAgICAgfQogICAgICAgIGltZyB7CiAgICAgICAgICAgIGRpc3BsYXk6IGJsb2NrOwogICAgICAgICAgICBtYXJnaW46IDAgYXV0bzsKICAgICAgICAgICAgdmVydGljYWwtYWxpZ246IHRvcDsKICAgICAgICB9CiAgICA8L3N0eWxlPgo8L2hlYWQ+Cjxib2R5PgogICAgPGgxPkFybSBOYW1lOiBoaWdoPC9oMT4KICAgIDxkaXYgY2xhc3M9InN1YnRpdGxlIiBzdHlsZT0idGV4dC1hbGlnbjogbGVmdDsiPgogICAgICAgIEVuZHBvaW50cyAoMik6cGZzLCBvczxicj4KICAgIDwvZGl2PgoKICAgIDx0YWJsZT4KICAgICAgICA8dGhlYWQ+CiAgICAgICAgICAgIDx0cj4KICAgICAgICAgICAgICAgIDx0aCBjbGFzcz0ibm8tY29sIj5ObzwvdGg+CiAgICAgICAgICAgICAgICA8dGggY2xhc3M9InZhcmlhYmxlLWNvbCI+VmFyaWFibGU8L3RoPgogICAgICAgICAgICAgICAgPHRoIGNsYXNzPSJzdGF0cy1jb2wiPlN0YXRzIC8gRnJlcXM8L3RoPgogICAgICAgICAgICAgICAgPHRoIGNsYXNzPSJncmFwaC1jb2wiPkdyYXBoPC90aD4KICAgICAgICAgICAgPC90cj4KICAgICAgICA8L3RoZWFkPgogICAgICAgIDx0Ym9keT4KICAgICAgICAgICAgPHRyPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJuby1jb2wiPjE8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJ2YXJpYWJsZS1jb2wiPnBmczxicj5bdGltZS10by1ldmVudF08L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJzdGF0cy1jb2wiPk1lZGlhbiB0aW1lOiAxMC4wOTxicj5FdmVudHM6IDEwMDAwPGJyPk1pc3Npbmc6IDAgKDAlKTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9ImdyYXBoLWNvbCI+PGltZyBzcmM9ImRhdGE6aW1hZ2UvcG5nO2Jhc2U2NCxpVkJPUncwS0dnb0FBQUFOU1VoRVVnQUFBSGdBQUFCUUNBSUFBQUJkK1NiZUFBQUFDWEJJV1hNQUFBN0RBQUFPd3dISGI2aGtBQUFJRVVsRVFWUjRuTzJjWFV3VDZSckgzeGxHS0xWUVd1Z1g3bG44V0tta0gxcytqNmFJWUk4ZU9iWVhHcnRpMUN0aXhNVEVyOFE3RXVWR0V5L2t4c1RrYk1pSlIwVE1raFgwSUNBZ3hvL0txZ1ZST2FkMWJWMUxTNnNVTExTRmxrNW5MOWdZenNwMFNqc2RaWnpmRlhUZWVlYmYvMHlmbVhuZjkza2hETU1BUStLQlA3ZUFyd1hHYUlwZ2pLWUlHSVI4N3ZkdVgraHpDNkU3U2JKdnkwRjYwTnIzZENvdlY4UmMzd2tES1MyRUJ4MnVsTzhVcXhEY1JwY3ZYN2JaYkI2UDUrelpzd0FBaDhNeE1qSlNVbEpDbmN5bER5ekoxMVJXVmxhczVZVHdIL1BHeHNic2R2dkZpeGZuL2gwY0hEeDE2aFExK21nREZIYjFYdW53WjJWL285cjhmWVRVNFhBNGlvdUw3WFk3QU1CdXQ1ZVVsTXo5elJBbENDVGNzRU9md2s2Rm90OW54WW9WZ1VEQTdYWm5abVltVGhuTmdBRmdMY3JsT1JRS3hmUG56eE1oaUs3RStKd2hrOGxldm54SnJoUjZFNlBSS3BWcVlHQ0FYQ24wSmthamk0cUtuang1UXE0VWVoT2owWEs1M0dxMStudytjdFhRbUJpTlJoQkVLcFV5YVRwNmtKNUxQNzRPOGJOWTZmTGRmOHROV3NTZTY5ZXZmL1RvRWZOK0dDVXdqOC9uWjBCd1drWTYvak9leFdJWkdocENVWFQraHhzMmJEQVlEQWtYU0Jzd0RNTXdESFhZSENpR2gxNnZWeXFWcWFtcDh6ODBtODFyMXF6QjNZZmgvNEg2ZnZyUFRCb0NBdW5yL3JFK0J6OTF6SDhGLzNpR0pCTEo0T0NnV0N5bTRvcFk0c0JjRG9haUtNWko0eXp5dmdoQlVHbHA2ZjM3OXhNampHNGdxcjl2VjhXNnMxcXRmdkRnd2E1ZHU4aFVSRlBpNnVvdkxTMjlkKzhlV1ZMb1RWeEc1K2ZubTgxbXY5OVBsaG9hRTVmUkNJSW9sVXFqMFVpV0dob1Q3eWdoY3orTWtuaU4xbWcwM2QzZHBFaWhOL0F6bzlIUTl2UDFkcU1ySE12K0d6ZHVOQnFOVEpvbUJQYSsrZVgrLzN5WWIyb3Fwamw0TEJhcnNMQ1F5UjZFUUJpR0FRRENveU11MFRjU25FUlNWbFkyTkRRME16TXpNelB6NmRZelo4NThuSW5BZ0FjOGRMdWpzNnVyMnpTWmpOK3AxTkhSWVRBWStIeitnbHUzYmR0MjY5YXRSQW1rQzdCN0lvU2lLT3I1TUlXZm85bHNOcGZMaGFDRlQ0VktwWEs3M1NNakk0blNTQXVRaWgrMGNZYUFJR2pyMXEwM2I5NnNxYWtoUlJNdElXZTJYVlZWVlZOVEV5bWg2QW81Um0vZXZQblZxMWMybTQyVWFMU0VIS01SQk5GcXRXMXRiYVJFb3lXa1RkVGRzMmZQbFN0WHlJcEdQMGd6dXF5c3pHcTFNdGtERHhpZDlveS9OLzNYUEJtTUwxQlNVcEplcjJkdWlYakFnZC82R3YvMTQ4LzNSdjF4bDhIdDI3ZXZvYUVCWStycEZnVERNR3htZk96WHR4Rkd3WThmUDY3VmFqa2NEdUZZcjBxbHVuUG5Eam5qeHZRQ2RnMzFkZDE5OG1LYUpjQlAxenFkVHFmVHNWZ3N3dE4yOE9EQmhvWUdNaThFdWdEMzljOW1TYkt3WjQrRzhRdXp5c3ZMdFZwdGNuSXlZYmlxcXFyMjluYVh5MFdtUmxvQVlRR1BhMnlhbFNYaVJyVHgwM2tkZUp3NGNRSkYwZnI2ZXRJMDBvTW9VNHpkYnMvT3pvNm1wZFBwek16TWZQdjJiUndKallhUVgxa29Fb21xcTZ2UG56OVBldVFsVFVKS09JOGRPM2JwMGlVbVU4OG5JVWFMeGVMcTZ1cVRKMDhtSXZnU0pWRkZ5YlcxdFoyZG5jUER3d21LditTQVo2Y21wbVlCQ0FiaWZBWC9FeHdPcDdhMjlzQ0JBMythVmYzVkFyZGNlL3J3MnI4N2Z1cnVKOWRwQUE0ZE9wU1Nrc0lVTTgrQnFQTUZpT0t2a3oxUElmekowZGV2WHplYnpRc09nVWNBaHVIbTVtYVZTbFZRVUxCang0NTRsUzV4a0w4b3YzTlB6S3dvVk16aXArc1hMMTZZVEtaUWFOR0xlZ2dFZ3JhMnRzcktTcUZRcUZhcjQxSzYxTG4yejY3SGcwOTdHMjg4bTQzMHZCMzlDOHVuZEhWMUNRU0N1M2Z2eHJZN1BZQkxDK0gzRGhjbUs4N0RYNjhqVHJaczJYTDE2bFc5WHQvYTJwcW9ZM3o1UkhsQzRybWk1eGdZR01qSnlUbDkralNLNG5mSTBoZnFGdmRScVZUOS9mMmRuWjI3ZCs5MnU5MlVIZmNMZ2RKVmxFUWlVVzl2YjA1T2prS2hhR2xwd2I2cXNaZ29yL3o0VThkOERBYURRcUdReStYTnpjM1QwOU5raGYyUytUeEd6OUhhMnFyUmFMS3lzZzRmUHR6ZTNoNEtoY2lOLzBYeHg3UmRRcUx2K0Y4c1ZxdTFxYW5weG8wYlZxdTF2THg4MDZaTnhjWEZTcVV5bWdHZEpVVHNsYk9rOCtiTm01NmVub2NQSC9iMzkxc3NGb2xFc25MbHlxS2lJcWxVdW5idDJ1enNiSkZJeE9Gd0VpY2dvVUFESFRjZFlSaG1mVnRjTHN2RW1TTHRjcmtzRnN2T25UdEhSMGVwa1JVSUJHdzIyK3ZYcng4L2Ztd3ltYXhXcThQaGVQZnVYVGdjNXZQNVhDNVhMQmJETUx4OCtYSTJtNTJXbGdZQTRQRjRNQXh6dVZ3QVFGcGFHb0lnQUFBMm01MlNrZ0lBWUxGWXFhbXBmM3huQ01ySXlDRFVzR3pac3NXZVZ6YWJqVmV3SGRXTS8rM2J0dzhQRDRkQ29jOCtFY252OTQrUGozczhIcWZUaVdHWTErdjErLzFlcnhmRHNBOGZQcUFvT2prNUNRQ1luSnljNnpYMCtYekJZQkFBTUQwOS9iR3ZKaHdPZXp3ZXdtUE56czU2dmQ1RnlWdTFhaFZlNlZTMHFZTWhUaEF1Qi9PaEtMeVlvdnY2K3ZxNnVqb2VqeGU1R1lxaVkyTmpJcEdJTUtEVDZSUUtoVEJNb0dCaVltSitCc0RENy9jSGcwSEM1QkFLaGNiSHg0VkNJYUc4MGRGUnNWaU1WL0R3RWJmYmZlSENoYjE3OXk2NE5aYWlleDZQVjFGUmNlN2N1Y2pOYkRiYi92MzdiOSsrVFJoUXJWWTNOallTTGtkeDlPaFJqVWFqMCtraU4ydHNiRFNaVEhWMWRaR2JtYzNtSTBlT1JGTjlVMUJRME5MU01wZjlJMUJUVXpOM3QxaVFXSHFTeEdLeFZDcGR2WHAxNUdicDZlbDVlWG1FelFBQU1wa3NMeStQOENjaWxVb1ZDZ1ZoUUxsY0RrRVFZYlBrNU9RbzVjbmw4blhyMWhIK2tuSnpjd1VDQWQ3V2FKK2pHZUtFV1RHYUloaWpLWUl4bWlJWW95bUNNWm9pR0tNcGdqR2FJaGlqS1lJeG1pSVlveW1DTVpvaWZnZncxVXVFdjdqcFdRQUFBQUJKUlU1RXJrSmdnZz09IiBhbHQ9IktNIGN1cnZlIj48L3RkPgogICAgICAgICAgICA8L3RyPgogICAgICAgICAgICA8dHI+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9Im5vLWNvbCI+MjwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InZhcmlhYmxlLWNvbCI+b3M8YnI+W3RpbWUtdG8tZXZlbnRdPC90ZD4KICAgICAgICAgICAgICAgIDx0ZCBjbGFzcz0ic3RhdHMtY29sIj5NZWRpYW4gdGltZTogMjAuMTU8YnI+RXZlbnRzOiAxMDAwMDxicj5NaXNzaW5nOiAwICgwJSk8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJncmFwaC1jb2wiPjxpbWcgc3JjPSJkYXRhOmltYWdlL3BuZztiYXNlNjQsaVZCT1J3MEtHZ29BQUFBTlNVaEVVZ0FBQUhnQUFBQlFDQU1BQUFEbFJVRzdBQUFDNkZCTVZFVUFBQUFIQndjSkNRa01EQXdPRGc0UER3OFFFQkFSRVJFU0VoSVRFeE1VRkJRVkZSVVdGaFlYRnhjWUdCZ1pHUmthR2hvYkd4c2NIQndkSFIwZ0lDQWlJaUlqSXlNa0pDUW1KaVlwS1NrcUtpb3VMaTR2THk4eU1qSXpNek0xTlRVMk5qWTVPVGs2T2pvOVBUMCtQajQvUHo5Q1FrSkRRME5FUkVSSFIwZEtTa3BMUzB0TVRFeE5UVTFRVUZCU1VsSlRVMU5VVkZSVlZWVldWbFpiVzF0Y1hGeGRYVjFpWW1Ka1pHUmxaV1ZtWm1abloyZG9hR2hxYW1wcmEydHNiR3h0YlcxdWJtNXZiMjl5Y25KMGRIUjFkWFYyZG5aM2QzZDRlSGg1ZVhsNmVucDdlM3Q4Zkh4OWZYMStmbjUvZjMrQWdJQ0RnNE9FaElTRmhZV0ZuTmlHaG9hSGg0ZUlpSWlKaVltS2lhT0tpb3FMaTR1TWpJeU5qWTJQajQrUWtKQ1JrWkdTaW5TU2twS1hsNWVabVptWm5hMmVucDZnb0tDaG9hR2lvcUtqbzZPa29iU2twS1NtcHFhbm5MdW9tcnFvcUtpcHFhbXFtN3FxcXFxcnE2dXJyYTZzckt5dHBidXVxc0N2cjYrdnNzdXZ0YzJ4c2JHeXNyS3pzN08wdExTMXRiVzF0cmEyc01XMnRyYTNwYlMzdDdlM3Z0UzRwS0s0c3NlNHVMaTR1Ym00dTlDNXVicTV2OVc2dXJxN3U3dThvWnU4dkx5OXZiMjl5ZUM5ME9pK3BiTytxYk8vcExLL3Y3L0FyTFhBd01EQTArckJ3Y0hDK2YvRXJaZkV4TVRHc2E3R3hzYkl5TWpKeWNuTDBNM016TXpOczYzTnpjM04wczdPenM3UHVMVFF6YzNSdXJmUisvL1UxTlRWMWRYVzF0YlcyZkhYMTlmWXQ3L1kyTmpaMmRuWi9QL2EydHJiMjl2Yi9QL2QzZDNmL1AvZzRPRGcvUC9pemREaTR1TGs1T1RsNWVYbXpyem01dWJtK3ZMbS9mL241K2ZuL2YvbzBiL281ZWpvNk9qby9mL3A2ZW5yNit2czNPTHM2dXpzL3YvdDZ0anQ3ZTN1N3U3dS92L3Y3Ky92L3YvdzhQRHcrZi94OGZIeTh2THkrdi95Ly8vejZ1eno4L1AwKy8vMTE3RDE5ZlgxOXY3Mjl2YjQrUGo1K2ZuNS8vLzY4dTc2K3ZyNi8vLzcrL3Y3Ly8vODl2RDk1dVA5L2YzKzZiNys3dXorL3Y3LytPNy8rZkgvK2ZqLysvVC8vL2YvLy8rbHZrVjJBQUFBQ1hCSVdYTUFBQTdEQUFBT3d3SEhiNmhrQUFBRERVbEVRVlJvZ2UzWmVWZ01ZUndIOEVXeDJ4MDJOeW5IVXFRdFJaUXp0NXhwSGVWYVY4VG1QaVBKa1dQZFFvNGk5NUV6Y2xWdUluZHVvc2g5NXZldldtYmFkMlpLejZOM2ZvL0gvdjZaL2Y1bTl2bk04Kzd1TzdQdlNBQ3BKQVpZTkRnNzlSa09QSGJycXIwb2NITFlpc2RNQ1BSdEM1QzBSUndZNFBNZEpnUjRHUU1zVVlnRGZ4cTU4Q3Fia3F3QWJrbS9pQUwvZUtHWGNtRW9mMUFVbUVnNnVNbGdKSGlFS3hKOHhob0pCdmt4Sk5nMUVBa2UwRndNZU9tUVNVYzVjSXl0R1BDMkNWTXZNaUVoM0R4M2sybjBVZ1FZNE1ONUppZ3JGZGR0N2RlSkFLK05tSCtLVGIrR0dueTZpQUR2Q3AxN2d3dEgxaEFCSnRKdk9OUDRJUTRNRGlGSXNLbzFFaHhURFFsK0swM0RnY0ZoT25WNDkrWnA1SzJQcnZ4YlVJYzM5cDU4bkEvSHk2bkQrbE9tdlhFeDVtVzVJN1RoUFJHTDJaa3JQYzZTZWRuU256YThPblQyQ1RheFF3M0xhdEtHaVpRSFo4bnU0c0RnRkl3RWoxSWl3V25TZEJ3WUhDY2l3VUZPZE9IM053KzhFb1NmeWxLb3dzOEg5cnduQ0VOVEZWVVl2bDVucDh4V2ppWDBkbTJYZjZRSlgxcTAvaDBUSXRVbSt2dnN3bW5DWTNhc3lWdDhJWVlhaHRlakNYKzdmRDh2a1hDRzJVbUtNSkZJR0RwNklzSFhLUDZpQ29UQnN5c1NIRzk2QVFlR05vMnB3YThmZlJlZU1uVjEyenFLRmp4NjNwd04rY01RWERtTEVuejYzQ0YyS1dLS1Nzbzd3SkhTZjJWSmR1b1RkaEcxVzBNajNnR0pwc3Zwd01SNk5YK29BYUxNRGxPQms4Tld2aWtRaHFIeXN6UmdJZ25DME4xbVB3NE1RZVlhSEJoMlZuUjdnQUpEdXBkMS93d01PT2RHcUg2WlBrWDVKU3MwbkRQZWJpWHQrdTBycWdjV2dpdDcrVlZHU0NNYldSMXY5YWJFditlSmxiMHIwUlovZkVOU2lKKzdyWlhFUkY2bGxyT0h0KzhndFdiY2VLMVdHeDNMcTdnRWdXSXZzOFNLUU4yeXBRdDd3aWtKc1ZxTld0V3VtWWVMc29GQ29haGVsVmNWNUFKVm00V0pvUmF4eUVYVTNBb294VDFMeTBKMExQZ2RLYS9qb0EvelRtV1lFL2R6NmN2cnFKeTVIVDhsdDlQTGhkdnA0VjRndktBOXR4UGVnZHVaMVluYm1kbVoxL0hoZG1ib1g5ci93eWZtQnRnQUcyQUQvTS9CUHdITG5MVlp0N2VvNkFBQUFBQkpSVTVFcmtKZ2dnPT0iIGFsdD0iS00gY3VydmUiPjwvdGQ+CiAgICAgICAgICAgIDwvdHI+CiAgICAgICAgICAgIDx0cj4KICAgICAgICAgICAgICAgIDx0ZCBjbGFzcz0ibm8tY29sIj4zPC90ZD4KICAgICAgICAgICAgICAgIDx0ZCBjbGFzcz0idmFyaWFibGUtY29sIj5wZnNfZXZlbnQ8YnI+W2V2ZW50IGluZGljYXRvcl08L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJzdGF0cy1jb2wiPjE6IDEwMDAwICgxMDAlKTxicj5NaXNzaW5nOiAwICgwJSk8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJncmFwaC1jb2wiPjxpbWcgc3JjPSJkYXRhOmltYWdlL3BuZztiYXNlNjQsaVZCT1J3MEtHZ29BQUFBTlNVaEVVZ0FBQUhnQUFBQW9DQU1BQUFBQ05NNFhBQUFBTFZCTVZFV1ptWm1vcUtpcnE2dXRyYTJ3c0xDMnRyYTV1Ym0vdjcvSXlNak56YzNUMDlQZjM5L2w1ZVhyNit2Ly8vL1Qrc1ErQUFBQUNYQklXWE1BQUE3REFBQU93d0hIYjZoa0FBQUFTVWxFUVZSWWhlM1d1UUdBTUJERXdPVUhQOWQvdVM1REJGSURreW9GbFJHb3ZqZWlTMWhZV0ZoWStJL3c5aEtkbVFkVHNQWEI0UGxCY0wrRmhZV0ZoWVdGYVhnOEVNeXdWUXZrVU5wMEhiRUptUUFBQUFCSlJVNUVya0pnZ2c9PSIgYWx0PSJiYXJwbG90IiBzdHlsZT0iaGVpZ2h0OiBhdXRvOyB3aWR0aDogMTIwcHg7Ij48L3RkPgogICAgICAgICAgICA8L3RyPgogICAgICAgICAgICA8dHI+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9Im5vLWNvbCI+NDwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InZhcmlhYmxlLWNvbCI+b3NfZXZlbnQ8YnI+W2V2ZW50IGluZGljYXRvcl08L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJzdGF0cy1jb2wiPjE6IDEwMDAwICgxMDAlKTxicj5NaXNzaW5nOiAwICgwJSk8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJncmFwaC1jb2wiPjxpbWcgc3JjPSJkYXRhOmltYWdlL3BuZztiYXNlNjQsaVZCT1J3MEtHZ29BQUFBTlNVaEVVZ0FBQUhnQUFBQW9DQU1BQUFBQ05NNFhBQUFBTFZCTVZFV1ptWm1vcUtpcnE2dXRyYTJ3c0xDMnRyYTV1Ym0vdjcvSXlNak56YzNUMDlQZjM5L2w1ZVhyNit2Ly8vL1Qrc1ErQUFBQUNYQklXWE1BQUE3REFBQU93d0hIYjZoa0FBQUFTVWxFUVZSWWhlM1d1UUdBTUJERXdPVUhQOWQvdVM1REJGSURreW9GbFJHb3ZqZWlTMWhZV0ZoWStJL3c5aEtkbVFkVHNQWEI0UGxCY0wrRmhZV0ZoWVdGYVhnOEVNeXdWUXZrVU5wMEhiRUptUUFBQUFCSlJVNUVya0pnZ2c9PSIgYWx0PSJiYXJwbG90IiBzdHlsZT0iaGVpZ2h0OiBhdXRvOyB3aWR0aDogMTIwcHg7Ij48L3RkPgogICAgICAgICAgICA8L3RyPgogICAgICAgIDwvdGJvZHk+CiAgICA8L3RhYmxlPgo8L2JvZHk+CjwvaHRtbD4K

## Define a Trial

With three arms, we can define a trial using the function
[`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).
Recruitment curve are specified through `enroller` with a built-in
function `StaggeredRecruiter` of piecewise constant rate. We set
`duration` to be an arbitrary large number (500) but controlling the end
of trial through a pre-defined milestone later. Note that if
`seed = NULL`, `TrialSimulator` will pick a seed for the purpose of
reproducibility.

``` r

accrual_rate <- data.frame(end_time = c(10, Inf),
                           piecewise_rate = c(30, 50))
trial <- trial(
  name = 'Trial-3415', n_patients = 1000,
  seed = 1727811904, duration = 500,
  enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
  dropout = rexp, rate = -log(1 - 0.1)/18, ## 10% by month 18
  silent = TRUE
)

trial$add_arms(sample_ratio = c(1, 1, 1), soc, low, high) ## 1:1:1
trial
#>  ⚕⚕         Trial Name:  Trial-3415  
#>  ⚕⚕        Description:  Trial-3415  
#>  ⚕⚕     Number of Arms:  3  
#>  ⚕⚕    Registered Arms:  soc, low, high  
#>  ⚕⚕       Sample Ratio:  1, 1, 1  
#>  ⚕⚕ Number of Patients:  1000  
#>  ⚕⚕   Planned Duration:  500  
#>  ⚕⚕            Regimen:  not set  
#>  ⚕⚕        Random Seed:  1727811904
```

## Define Milestone and Action for Final Analysis

To ensure sufficient powers of testing `PFS` and `OS`, final analysis is
performed when we have at least 700 events for `OS` and 800 events for
`PFS`. In the action function, we compute one-sided p-value of `PFS`
using the proportional hazard Cox model, and one-sided p-value of `OS`
using the logrank test. This is consistent with the assumption of
illness-death model implemented in data generator
[`CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md).
Note that five columns are available in locked data: `arm`, `pfs`, ‘os’,
‘pfs_event’, and ‘os_event’, which are used to construct model formula.
Estimates of hazard ratio are also computed. Built-in functions
`fitCoxph` and `fitLogrank` return data frames. Refer to their help
documants for more details.

``` r

action <- function(trial){
  
  locked_data <- trial$get_locked_data('final')
  
  pfs <- fitCoxph((Surv(pfs, pfs_event) ~ arm), placebo = 'soc', 
                  data = locked_data, alternative = 'less', 
                  scale = 'hazard ratio')
  
  os <- fitLogrank((Surv(os, os_event) ~ arm), placebo = 'soc', 
                   data = locked_data, alternative = 'less')
  
  ## Bonferroni test is applied to four hypotheses: 
  ## PFS_low, PFS_high, OS_low, and OS_high
  pfs$decision <- ifelse(pfs$p < .05/4, 'reject', 'accept')
  os$decision <- ifelse(os$p < .05/4, 'reject', 'accept')
  
  trial$save(
    value = pfs %>% filter(arm == 'low') %>% select(estimate, decision, info), 
    name = 'pfs_low')
  
  trial$save(
    value = pfs %>% filter(arm == 'high') %>% select(estimate, decision, info), 
    name = 'pfs_high')
  
  trial$save(
    value = os %>% filter(arm == 'low') %>% select(decision, info), 
    name = 'os_low')
  
  trial$save(
    value = os %>% filter(arm == 'high') %>% select(decision, info), 
    name = 'os_high')
  
}
```

Now we can define and register the milestone to a listener, which
monitors the trial for us through a controller

``` r

final <- milestone(name = 'final', action = action, 
                   when = eventNumber(endpoint = 'pfs', n = 450, 
                                      arms = c('soc', 'high')) & 
                     eventNumber(endpoint = 'os', n = 550)
                   )

listener <- listener()
listener$add_milestones(final)
#> A milestone <final> is registered.

controller <- controller(trial, listener)
```

We can run a massive number of replicates in simulation to study
operating characteristics of a trial design by specifying `n` in
`Controller$run()`. The simulation results can be accessed by calling
the member function `get_output()` of the controller.

``` r

controller$run(n = 1000, plot_event = FALSE, silent = TRUE)
output <- controller$get_output()
```

``` r

output %>% 
  head(5) %>% 
  kable(escape = FALSE) %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                position = "left") %>%
  scroll_box(width = "100%")
```

| trial | seed | milestone_time\_\<final\> | n_events\_\<final\>\_\<pfs\> | n_events\_\<final\>\_\<os\> | n_events\_\<final\>\_\<patient_id\> | n_events\_\<final\>\_\<arms\> | pfs_low\_\<estimate\> | pfs_low\_\<decision\> | pfs_low\_\<info\> | pfs_high\_\<estimate\> | pfs_high\_\<decision\> | pfs_high\_\<info\> | os_low\_\<decision\> | os_low\_\<info\> | os_high\_\<decision\> | os_high\_\<info\> | error_message |
|:---|---:|---:|---:|---:|---:|:---|---:|:---|---:|---:|:---|---:|:---|---:|:---|---:|:---|
| Trial-3415 | 1727811904 | 36.30115 | 769 | 550 | 1000 | c(“high”…. | 0.9455634 | accept | 524 | 0.7186429 | reject | 511 | accept | 381 | reject | 373 |  |
| Trial-3415 | 1324384826 | 36.01193 | 768 | 550 | 1000 | c(“high”…. | 0.8100537 | reject | 517 | 0.7885803 | reject | 519 | accept | 380 | reject | 373 |  |
| Trial-3415 | 1767496878 | 34.88384 | 775 | 550 | 1000 | c(“high”…. | 0.7731265 | reject | 522 | 0.8023016 | reject | 528 | reject | 381 | reject | 380 |  |
| Trial-3415 | 295939270 | 35.21148 | 747 | 550 | 1000 | c(“high”…. | 0.8732924 | accept | 514 | 0.7772976 | reject | 496 | accept | 388 | reject | 369 |  |
| Trial-3415 | 2110257267 | 34.37072 | 763 | 550 | 1000 | c(“high”…. | 0.8071533 | reject | 524 | 0.7257276 | reject | 511 | reject | 383 | reject | 372 |  |

For example, we can compute the powers and summarize the estimates of
hazard ratio for `PFS`.

``` r

output %>% 
  summarise(
    across(matches('_<decision>$'), ~ mean(. == 'reject') * 100, .names = 'Power_{.col}'), 
    across(matches('_<estimate>$'), ~ mean(.x), .names = 'HR_{.col}')
  ) %>%
  rename_with(~ sub('_<decision>$', '', .), starts_with('Power_')) %>%
  rename_with(~ sub('_<estimate>$', '', .), starts_with('HR_')) %>% 
  kable(col.name = NULL, digits = 3, align = 'r') %>% 
  add_header_above(c('Low', 'High', 'Low', 'High', 'Low', 'High'), align = 'r') %>% 
  add_header_above(c('PFS' = 2, 'OS' = 2, 'PFS' = 2)) %>% 
  add_header_above(c('Power (%)' = 4, 'Hazard Ratio' = 2)) %>% 
  kable_styling(full_width = TRUE)
```

[TABLE]

Note that `OS` does not satisfy the proportional hazard assumption, and
a composite condition on event numbers is used to trigger the final
analysis. Thus, the powers in the table above would not match to the
output from power calculation packages, which is as expected.
