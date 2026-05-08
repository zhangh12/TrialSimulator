# Simulate Correlated Progression-Free Survival and Overall Survival Using a Gumbel Copula

Progression-free survival (PFS) and overall survival (OS) are common
time-to-event endpoints in oncology trials. Because progression or death
defines a PFS event, simulated trial data should always satisfy
$`\mbox{PFS} \leq \mbox{OS}`$. At the same time, the two endpoints are
often highly correlated, so independent marginal simulation is usually
not appropriate.

This vignette describes the copula-based generator implemented in
[`TrialSimulator::CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md).
The function is intended for simulation settings where the user wants to
specify three interpretable quantities:

- the marginal median PFS,
- the marginal median OS,
- Kendall’s tau between the observed, uncensored PFS and OS times.

The method uses a Gumbel survival copula for a latent time to
progression (TTP) and OS, and then defines

``` math
\mbox{PFS} = \min(\mbox{TTP}, \mbox{OS}).
```

This construction guarantees $`\mbox{PFS} \leq \mbox{OS}`$ for every
simulated patient. It also keeps both marginal PFS and OS exponential,
so the inputs can be specified directly through their medians.

This is a useful distinction from the illness-death model in
[`TrialSimulator::CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md).
The illness-death model gives a clinically interpretable transition
structure, but OS generated from that model can have a time-varying
hazard ratio between treatment arms. Therefore, if OS will be analyzed
using a proportional hazards Cox model and the simulation should be
consistent with that analysis model, the copula-based method described
here is often more appropriate.

## Model

Let $`X`$ denote latent TTP and let $`Y`$ denote OS. The model assumes
exponential marginal survival functions

``` math
\Pr(X > x) = \exp(-\lambda_X x), \qquad
\Pr(Y > y) = \exp(-\lambda_Y y),
```

and a Gumbel–Hougaard survival copula

``` math
\Pr(X > x, Y > y)
=
\exp\left[
-\left\{(\lambda_X x)^\theta + (\lambda_Y y)^\theta\right\}^{1/\theta}
\right],
\qquad \theta \geq 1.
```

The observed PFS time is

``` math
P = \min(X, Y).
```

Under this model, OS is exponential with rate $`\lambda_Y`$. PFS is also
exponential, with survival function

``` math
\Pr(P > t)
=
\Pr(X > t, Y > t)
=
\exp\left[
-\left\{(\lambda_X^\theta + \lambda_Y^\theta)t^\theta\right\}^{1/\theta}
\right]
=
\exp(-\lambda_P t),
```

where

``` math
\lambda_P = \left(\lambda_X^\theta + \lambda_Y^\theta\right)^{1/\theta}.
```

Therefore, if the target medians are $`m_P`$ for PFS and $`m_Y`$ for OS,

``` math
\lambda_P = \frac{\log(2)}{m_P},
\qquad
\lambda_Y = \frac{\log(2)}{m_Y}.
```

For a candidate value of $`\theta`$, the latent TTP rate is then

``` math
\lambda_X =
\left(\lambda_P^\theta - \lambda_Y^\theta\right)^{1/\theta}.
```

This requires $`m_P < m_Y`$, as expected for PFS and OS.

## Choosing the Copula Parameter

For the Gumbel copula, Kendall’s tau between latent TTP and OS is

``` math
\tau(X,Y) = 1 - \frac{1}{\theta}.
```

However,
[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
asks for Kendall’s tau between the observed, uncensored endpoints
$`P=\min(X,Y)`$ and $`Y`$, not between latent TTP and OS. These are not
the same quantity. The formula below is derived in the appendix. Under
the model above,

``` math
\tau(P,Y)
=
1 - \frac{1}{\theta}
\left[
1 - \left(\frac{m_P}{m_Y}\right)^\theta
\right].
```

Equivalently, writing $`\tau_X = \tau(X,Y)`$ and
$`\theta = 1/(1-\tau_X)`$,

``` math
\tau(P,Y)
=
1 -
(1-\tau_X)
\left[
1 -
\left(\frac{m_P}{m_Y}\right)^{1/(1-\tau_X)}
\right].
```

[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
solves this equation numerically for the latent Kendall’s tau
$`\tau_X`$, converts it to $`\theta`$, and then simulates from the
Gumbel copula.

For any finite $`\theta`$, $`\tau(P,Y)`$ is larger than $`\tau(X,Y)`$:

``` math
\tau(P,Y)-\tau(X,Y)
=
\frac{1}{\theta}
\left(\frac{m_P}{m_Y}\right)^\theta
> 0.
```

Intuitively, even if latent TTP and OS have a weaker association,
observed PFS contains deaths by construction, which increases the
association between observed PFS and OS.

The requested value of $`\tau(P,Y)`$ has a lower bound. When
$`\theta=1`$, TTP and OS are independent, but PFS still contains OS
deaths through the definition $`P=\min(X,Y)`$. In this limiting case,

``` math
\tau(P,Y) = \frac{m_P}{m_Y}.
```

Thus, for fixed medians $`m_P`$ and $`m_Y`$,
[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
can only target

``` math
\frac{m_P}{m_Y} \leq \tau(P,Y) < 1.
```

If the requested Kendall’s tau is too small for the two medians, the
function stops with an error.

## Example

Suppose we want to simulate PFS with median 5 months and OS with median
11 months, targeting Kendall’s tau of 0.6 between observed, uncensored
PFS and OS times.

``` r

pfs_and_os <- endpoint(name = c('pfs', 'os'),
                       type = c('tte', 'tte'),
                       generator = CorrelatedPfsAndOs2,
                       median_pfs = 5,
                       median_os = 11,
                       kendall = 0.6,
                       pfs_name = 'pfs',
                       os_name = 'os')

pfs_and_os
```

CjwhRE9DVFlQRSBodG1sPgo8aHRtbD4KPGhlYWQ+CiAgICA8bWV0YSBjaGFyc2V0PSJVVEYtOCI+CiAgICA8dGl0bGU+RW5kcG9pbnRzICgyKTwvdGl0bGU+CiAgICA8c3R5bGU+CiAgICAgICAgYm9keSB7CiAgICAgICAgICAgIGZvbnQtZmFtaWx5OiBBcmlhbCwgc2Fucy1zZXJpZjsKICAgICAgICAgICAgbWFyZ2luOiAyMHB4OwogICAgICAgICAgICBiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTsKICAgICAgICAgICAgZGlzcGxheTogZmxleDsKICAgICAgICAgICAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjsKICAgICAgICAgICAgYWxpZ24taXRlbXM6IGNlbnRlcjsKICAgICAgICB9CiAgICAgICAgaDEgewogICAgICAgICAgICBjb2xvcjogYmxhY2s7CiAgICAgICAgICAgIHRleHQtYWxpZ246IGNlbnRlcjsKICAgICAgICAgICAgbWFyZ2luLWJvdHRvbTogMjBweDsKICAgICAgICAgICAgZm9udC1zaXplOiAyMHB4OwogICAgICAgIH0KICAgICAgICAuc3VidGl0bGUgewogICAgICAgICAgICB0ZXh0LWFsaWduOiBjZW50ZXI7CiAgICAgICAgICAgIGNvbG9yOiAjNjY2OwogICAgICAgICAgICBtYXJnaW4tYm90dG9tOiAyMHB4OwogICAgICAgICAgICBmb250LXNpemU6IDE2cHg7CiAgICAgICAgfQogICAgICAgIHRhYmxlIHsKICAgICAgICAgICAgYm9yZGVyLWNvbGxhcHNlOiBjb2xsYXBzZTsKICAgICAgICAgICAgZm9udC1zaXplOiAxNHB4OwogICAgICAgICAgICBib3JkZXI6IDFweCBzb2xpZCAjOTk5OwogICAgICAgICAgICB3aWR0aDogYXV0bzsKICAgICAgICAgICAgbWFyZ2luOiAwIGF1dG87CiAgICAgICAgfQogICAgICAgIHRoIHsKICAgICAgICAgICAgYmFja2dyb3VuZC1jb2xvcjogI2YwZjBmMDsKICAgICAgICAgICAgY29sb3I6IGJsYWNrOwogICAgICAgICAgICBwYWRkaW5nOiAxMHB4OwogICAgICAgICAgICB0ZXh0LWFsaWduOiBsZWZ0OwogICAgICAgICAgICBmb250LXdlaWdodDogbm9ybWFsOwogICAgICAgICAgICBib3JkZXI6IDFweCBzb2xpZCAjOTk5OwogICAgICAgICAgICB3aGl0ZS1zcGFjZTogbm93cmFwOwogICAgICAgICAgICBmb250LXNpemU6IDE0cHg7CiAgICAgICAgfQogICAgICAgIHRkIHsKICAgICAgICAgICAgcGFkZGluZzogMTBweDsKICAgICAgICAgICAgYm9yZGVyOiAxcHggc29saWQgIzk5OTsKICAgICAgICAgICAgdmVydGljYWwtYWxpZ246IHRvcDsKICAgICAgICAgICAgbGluZS1oZWlnaHQ6IDEuNDsKICAgICAgICAgICAgZm9udC1zaXplOiAxNHB4OwogICAgICAgIH0KICAgICAgICAubm8tY29sIHsKICAgICAgICAgICAgdGV4dC1hbGlnbjogY2VudGVyOwogICAgICAgICAgICB3aGl0ZS1zcGFjZTogbm93cmFwOwogICAgICAgIH0KICAgICAgICAudmFyaWFibGUtY29sIHsKICAgICAgICAgICAgd2hpdGUtc3BhY2U6IG5vd3JhcDsKICAgICAgICB9CiAgICAgICAgLnN0YXRzLWNvbCB7CiAgICAgICAgfQogICAgICAgIC5mcmVxcy1jb2wgewogICAgICAgICAgICBsaW5lLWhlaWdodDogMjBweDsKICAgICAgICB9CiAgICAgICAgLmdyYXBoLWNvbCB7CiAgICAgICAgICAgIHRleHQtYWxpZ246IGNlbnRlcjsKICAgICAgICAgICAgd2hpdGUtc3BhY2U6IG5vd3JhcDsKICAgICAgICAgICAgdmVydGljYWwtYWxpZ246IHRvcDsKICAgICAgICB9CiAgICAgICAgaW1nIHsKICAgICAgICAgICAgZGlzcGxheTogYmxvY2s7CiAgICAgICAgICAgIG1hcmdpbjogMCBhdXRvOwogICAgICAgICAgICB2ZXJ0aWNhbC1hbGlnbjogdG9wOwogICAgICAgIH0KICAgIDwvc3R5bGU+CjwvaGVhZD4KPGJvZHk+CiAgICA8aDE+RW5kcG9pbnRzICgyKTwvaDE+CiAgICA8ZGl2IGNsYXNzPSJzdWJ0aXRsZSIgc3R5bGU9InRleHQtYWxpZ246IGxlZnQ7Ij4KICAgICAgICBwZnMsIG9zPGJyPgogICAgPC9kaXY+CgogICAgPHRhYmxlPgogICAgICAgIDx0aGVhZD4KICAgICAgICAgICAgPHRyPgogICAgICAgICAgICAgICAgPHRoIGNsYXNzPSJuby1jb2wiPk5vPC90aD4KICAgICAgICAgICAgICAgIDx0aCBjbGFzcz0idmFyaWFibGUtY29sIj5WYXJpYWJsZTwvdGg+CiAgICAgICAgICAgICAgICA8dGggY2xhc3M9InN0YXRzLWNvbCI+U3RhdHMgLyBGcmVxczwvdGg+CiAgICAgICAgICAgICAgICA8dGggY2xhc3M9ImdyYXBoLWNvbCI+R3JhcGg8L3RoPgogICAgICAgICAgICA8L3RyPgogICAgICAgIDwvdGhlYWQ+CiAgICAgICAgPHRib2R5PgogICAgICAgICAgICA8dHI+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9Im5vLWNvbCI+MTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InZhcmlhYmxlLWNvbCI+cGZzPGJyPlt0aW1lLXRvLWV2ZW50XTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InN0YXRzLWNvbCI+TWVkaWFuIHRpbWU6IDUuMTxicj5FdmVudHM6IDEwMDAwPGJyPk1pc3Npbmc6IDAgKDAlKTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9ImdyYXBoLWNvbCI+PGltZyBzcmM9ImRhdGE6aW1hZ2UvcG5nO2Jhc2U2NCxpVkJPUncwS0dnb0FBQUFOU1VoRVVnQUFBSGdBQUFCUUNBTUFBQURsUlVHN0FBQUMzMUJNVkVVQkFRRUdCZ1lIQndjSUNBZ0pDUWtLQ2dvTURBd05EUTBPRGc0UER3OFJFUkVTRWhJVEV4TVVGQlFWRlJVV0ZoWVhGeGNZR0JnWkdSa2FHaG9iR3hzY0hCd2RIUjBlSGg0Zkh4OGdJQ0FpSWlJakl5TWxKU1VuSnljb0tDZ3BLU2txS2lvckt5c3NMQ3d0TFMwdUxpNHZMeTh3TURBek16TTBORFExTlRVNE9EZzlQVDFBUUVCQ1FrSkRRME5FUkVSRlJVVkdSa1pIUjBkSVNFaEtTa3BMUzB0TVRFeFJVVkZTVWxKVFUxTlZWVlZXVmxaWFYxZFlXRmhjWEZ4ZVhsNWdZR0JqWTJOa1pHUm1abVpwYVdscWFtcHJhMnRzYkd4dGJXMXdjSEJ5Y25KMWRYVjNkM2Q1ZVhsN2UzdC9mMytBZ0lDQmdZR0RnNE9GaFlXRm5OaUdob2FIaDRlSWlJaUppWW1LaWFPTGk0dU1qSXlOalkyUGo0K1JrWkdTaW5TU2twS1RrNU9VbEpTV2xwYVptWm1abmEyY25KeWRuWjJlbnA2Zm41K2dvS0Nob2FHam82T2tvYlNubkx1b21ycW9xS2lwcWFtcW03cXFxcXFycTZ1cnJhNnRwYnV1cXNDdXJxNnZyNit2c3N1dnRjMnlzckt6czdPMHRMUzF0cmEyc01XMnRyYTNwYlMzdDdlM3Z0UzRwS0s0c3NlNHVMaTR1Ym00dTlDNXVibTV1YnE1djlXNnVycTd1N3U4b1p1OHZMeTl5ZUM5ME9pK3BiTytxYk8vcExLL3Y3L0FyTFhBMCtyQytmL0VyWmZFeE1UR3NhN0d4c2JIeDhmS3lzckx5OHZMME0zTXpNek5zNjNOMHM3T3pzN1B1TFRQejgvUXpjM1J1cmZSMGRIUisvL1MwdExUMDlQVjFkWFcxdGJXMmZIWDE5Zll0Ny9aMmRuWi9QL2IyOXZiL1AvZDNkM2UzdDdmL1AvZzRPRGcvUC9pemREaTR1TGs1T1RsNWVYbXpyem01dWJtK3ZMbS9mL241K2ZuL2YvbzBiL281ZWpvNk9qby9mL3E2dXJyNit2czNPTHM2dXpzL3YvdDZ0anUvdi92Nysvdi92L3crZi94OGZIeTh2THkrdi95Ly8vejZ1ejA5UFQwKy8vMTE3RDE5ZlgxOXY3Mjl2YjM5L2Y0K1BqNStmbjUvLy82OHU3Nit2cjYvLy83Ky92Ny8vLzg5dkQ4L1B6OTV1UDkvZjMrNmI3Kzd1eisvdjcvK083LytmSC8rZmovKy9ULy8vZi8vLytzQnpxOEFBQUFDWEJJV1hNQUFBN0RBQUFPd3dISGI2aGtBQUFEUWtsRVFWUm9nZTNZK1YvTGNSd0g4Q1ZHYXQxQ1NLV0RTcEdFbXNLVUtLVklTRmdtUjZITWJTUno1TXA5Smtja0lva29PUW9oUjg3Y3BKZ2NFZkwrQTJ5dGZiZlBXc2NqZmI2ZlgvYjZaWHM5UHQvSDQ3bDlIbnQ4SHArOUdVQW9ERFZNRzF4VjlJWU1QTy9JamxORTRQeTRiUzlsSlNyWUR5QXZpUjRZNE9kaldRbnoxZ0lRT3RNRC81aTkvaTdWOG93QTdyTXFhWUgvdmxkb0VoaTZaTklDSTYwYUhqU2RFTXpqRUlKVGJBakJJdVlITWpCMFAwUUk5Z3FuQTk0eWJXR1dFaHp0UmdkOGRNSFNXN0tTTGRTWHZOelRwZUVJRVcvMTk1dXk0bWJWc3ZyVktwVUdlRS84MnN0VWsyNDErRXlpQVQ0ZXUvcWhNc3dmUUFPTXRCcTR3SUFRREdicGhHRGZzWVRnQkFkQ3NFaXZrQXdNbmhIWTRSTUhscUZYbitvSW5MREQrOGN2dWxnYkx0TzdqaHRXUERMdHRWckkzZzdoNG9aUHhtK2lUcTUzR1lheXQ1dHgvNjRaTzJOWFhxSWF0ZFVnTXJpR0dVYWFISWFoRXduQmlkMEl3V0NSU0FqbWVoQ0NpM1VLeU1EZ0U0Z1YvdnJvN0VmVmNLYXhDQ2Y4ZHZLWXA2cGhjSTdDQ2NPdkI5U1JPY0tWcWJpMjFhd2NJM3g3NDk0dnNyS2J4MElXZTA3RkNNODl0a3MrZkVHM0drNGJsK0NEZjk5NUptOUtNTGlQd2djalRSbk9aVjBnQTBOb2IwS3d5R0ltR1JqU1dUbGtZSmhnWDRZSC92VGlUeDFIcGpRVnJuaUdRSXc1YTFidHF3K0c1NVpZN2lLTUt6Zk9aY25La25IYUtoNjVhb0pqNHNhb0tucEZEVkdEUE5xb2V1YThVUXdHR0psWHE5cHFjZEtNbS8rV3pjaVAyLzY1SVJoeUxZZFhORGVNdExwZ2VOTEhMbzBJREJVekRObk5PazV1TEN5Ky9JV3kyQmtrWUlEWElib3VHMGpBQUNVUjVwMThCY1gwdytJa0J6aTBzaGdjZWVaL2I2QXFKM3NOcFB4d09MdXpwcm1MVjJDa01EV3Z0SW13NG1Tdk1LbnhrN1hTVkFIWDM2T1h0YUVHczRPdFkxOTNqcmVmZjRna1UzaThHRDZmTDB5UTVHQnlUVkt5cGFIK25TQVRBWmVPN1pydzJZdHpraE9XUi9PNElVRitrZ3pqY056RWNYYVN4TTYySmphbTBqaFNNTExWTkFZZG9rb1MxdFlVaVNaYWRmU1IycDZKTHJjMlFhcUJ0dEt5SEs3MVVXYjF6MGFpa1lsVXp3Q2tydWlLUG0yOUdLbWpCNkxMak1xNjRYVWowZDdqRzFLRDV5TTF0eC82TkJ1OUR3c0MwR1hiZXI0eFRWSERhbGdOcTJFMTNPajhBemhSVE1tVHcxZUFBQUFBQUVsRlRrU3VRbUNDIiBhbHQ9IktNIGN1cnZlIj48L3RkPgogICAgICAgICAgICA8L3RyPgogICAgICAgICAgICA8dHI+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9Im5vLWNvbCI+MjwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InZhcmlhYmxlLWNvbCI+b3M8YnI+W3RpbWUtdG8tZXZlbnRdPC90ZD4KICAgICAgICAgICAgICAgIDx0ZCBjbGFzcz0ic3RhdHMtY29sIj5NZWRpYW4gdGltZTogMTAuOTU8YnI+RXZlbnRzOiAxMDAwMDxicj5NaXNzaW5nOiAwICgwJSk8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJncmFwaC1jb2wiPjxpbWcgc3JjPSJkYXRhOmltYWdlL3BuZztiYXNlNjQsaVZCT1J3MEtHZ29BQUFBTlNVaEVVZ0FBQUhnQUFBQlFDQU1BQUFEbFJVRzdBQUFDNVZCTVZFVUhCd2NKQ1FrS0Nnb01EQXdORFEwT0RnNFBEdzhSRVJFU0VoSVRFeE1VRkJRVkZSVVdGaFlYRnhjWUdCZ1pHUmthR2hvY0hCd2VIaDRnSUNBaElTRWlJaUlqSXlNa0pDUW5KeWNvS0NncEtTa3JLeXNzTEN3dExTMHVMaTR2THk4d01EQXhNVEV6TXpNME5EUTFOVFUzTnpjNU9UazdPenM4UER3OVBUMCtQajVBUUVCQlFVRkRRME5FUkVSRlJVVktTa3BMUzB0TVRFeFBUMDlRVUZCUlVWRlNVbEpWVlZWV1ZsWlhWMWRjWEZ4ZFhWMWZYMTlnWUdCa1pHUmxaV1ZtWm1acGFXbHJhMnRzYkd4dWJtNXZiMjl3Y0hCeGNYRnljbkoyZG5aM2QzZDVlWGw2ZW5wN2UzdDhmSHgrZm41L2YzK0FnSUNCZ1lHQ2dvS0RnNE9FaElTRmhZV0ZuTmlHaG9hSWlJaUppWW1LaWFPTGk0dU1qSXlOalkyT2pvNlBqNCtTaW5TU2twS1ZsWldZbUppWm1abVpuYTJlbnA2Zm41K2dvS0Nob2FHam82T2tvYlNrcEtTbHBhV25uTHVucDZlb21ycW9xS2lwcWFtcW03cXFxcXFycmE2c3JLeXRwYnV0cmEydXFzQ3VycTZ2cjYrdnNzdXZ0YzJ6czdPMXRiVzF0cmEyc01XMnRyYTNwYlMzdDdlM3Z0UzRwS0s0c3NlNHVMaTR1Ym00dTlDNXVibTV1YnE1djlXNnVycTd1N3U4b1p1OXllQzkwT2krcGJPK3FiTyt2cjYvcExLL3Y3L0FyTFhBMCtyQndjSEMrZi9FclpmRXhNVEZ4Y1hHc2E3R3hzYkl5TWpKeWNuS3lzckx5OHZMME0zTXpNek5zNjNOemMzTjBzN1B1TFRQejgvUXpjM1J1cmZSMGRIUisvL1MwdExUMDlQVTFOVFcxdGJXMmZIWDE5Zll0Ny9aL1AvYTJ0cmIvUC9jM056ZDNkM2UzdDdmMzkvZi9QL2cvUC9oNGVIaXpkRGw1ZVhtenJ6bTV1Ym0rdkxtL2YvbjUrZm4vZi9vMGIvbzVlam82T2pvL2YvcDZlbnMzT0xzNnV6cy92L3Q2dGp0N2UzdTd1N3Uvdi92L3YvdzhQRHcrZi94OGZIeSt2L3kvLy96NnV6ejgvUDA5UFQwKy8vMTE3RDE5ZlgxOXY3MzkvZjUrZm41Ly8vNjh1NzYrdnI2Ly8vNy8vLzg5dkQ4L1B6OTV1UDkvZjMrNmI3Kzd1eisvdjcvK083LytmSC8rZmovKy9ULy8vZi8vLyt5Qmh6N0FBQUFDWEJJV1hNQUFBN0RBQUFPd3dISGI2aGtBQUFEQmtsRVFWUm9nZTNZYVZoTVVSZ0g4SXFhUVJPVnNjVFlFaEpKeGRoRkZDbDdsbENoUWJMR1dDTkpsb1RzZTZUc2tTelp4cEpDU0VKMlpkOE5nN3lmdVhMdnpKbnV6VHlQempsZjV2L2x6bi9PYzUvZjNQUGhQSE5mRTZBVUV5Tk1EQzdLZTBZSG5yNW4zUkVxY0ZiMG1pZHNDUnZpQTVDWlNBWUcrSHFQTGNGZExRQWlXNUtCdjB4WWZwTnJtVlVBVkZJeThNK1hPbzJCMWVVTGlNQklZMkJvbUVRSjdqaVdFaHpvVFFsZTFZSVNuR0ZEQ2RhSTc1S0FWNCtaZFFxRndXa0RDWGpmakhuWDJLS0trVEFYNzBBU01NRG5xMnlSMXpGbEx1RnRTY0NiWTVkZTVGcnhWcWZYSkFFZmpGcDhXdzlXaS9NSndFZ3Joc0U1bmhMY3B4OGxPSTdBMmNVTFB4QjlvZ09ETEpVUzNHVTBmdmpRenZub1h4OG0wK1Q0NGUzRDVwd3BBZWRLMU5oaDNTUFQwWUxkZVZrS2R2aHc3RXJ1NUhweHZQTGZUMTVCMk9IMVVRc3ZjSTNkYXBqdGpoMUdHZ2ZuVjNoSEI0WW1XeW5CZnYwcHdVbTFLTUZRSTQwUzdOc1hNL3p4em9rM2ZIQmFOY3p3ODVHRDcvUEJVQS92dTVzSmZMdkZIWms5WE10cFZ3WjU0WVd2cjlqeWdTMmJGQlcxS3hsV3I3SENVdzlzMUE1ZmRMY2FYQ1ppaGIvZmVLaHRDQnhUVjRNVFJob0NheHpDNmNBUUo4UDR5S1hCWUwrQUVqelRIdDhqbHdwckhNYmpnOTgrL3NGN1pQNUpzbVVPTm5qeWtrWGJCR0h3YllVTnZuVGxKRGVLaUFnUTZ5Mi9xajBPRjF5VTk1UWJvZzVzWjY2L25scnBHQ1lZbVZlWDJHcUFNR2t1SGpncmV1MzcwbUR3dHp1TEJVWWFId3doVnNsMFlJaTBEQ3I3OTJWRFlEamZyUDR1S2pEQUpHdjVVU293RkFaYWRrZ295NVBiVVBqMzY1Uy9uY2c5SkwzTVlKN0pubUN5dzlyYmlKMDhSMFNrUFBwL1dIZXlsNU5vOWM4YkxzY0hkM2VWbVpuWk5uTHo2RGxnNkNpRlFxbFVSc1F6U2RqTGs5TXFKTmtjckRzUmNMRzFOdlFIRjU3Yk1WY1IwTnZIMDZPelhDNTNjMmJTdUFGUHFrdVJOT2RnWktzSkJoMmlNZ2tXU1FWamJpMjRWTlZVK0RaSkd4NjR4RGVoclZXQ2NRd1ZYTm90RnI3TnI1c2g4TEpld3Z2VGFiL2dVa0ZUNGR1bUREY0VKaFFqYklTTnNCRTJ3Z2JuRjlsOW9XUWNJYVArQUFBQUFFbEZUa1N1UW1DQyIgYWx0PSJLTSBjdXJ2ZSI+PC90ZD4KICAgICAgICAgICAgPC90cj4KICAgICAgICAgICAgPHRyPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJuby1jb2wiPjM8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJ2YXJpYWJsZS1jb2wiPnBmc19ldmVudDxicj5bZXZlbnQgaW5kaWNhdG9yXTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InN0YXRzLWNvbCI+MTogMTAwMDAgKDEwMCUpPGJyPk1pc3Npbmc6IDAgKDAlKTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9ImdyYXBoLWNvbCI+PGltZyBzcmM9ImRhdGE6aW1hZ2UvcG5nO2Jhc2U2NCxpVkJPUncwS0dnb0FBQUFOU1VoRVVnQUFBSGdBQUFBb0NBTUFBQUFDTk00WEFBQUFMVkJNVkVXWm1abW9xS2lycTZ1dHJhMndzTEMydHJhNXVibS92Ny9JeU1qTnpjM1QwOVBmMzkvbDVlWHI2K3YvLy8vVCtzUStBQUFBQ1hCSVdYTUFBQTdEQUFBT3d3SEhiNmhrQUFBQVNVbEVRVlJZaGUzV3VRR0FNQkRFd09VSFA5ZC91UzVEQkZJRGt5b0ZsUkdvdmplaVMxaFlXRmhZK0kvdzloS2RtUWRUc1BYQjRQbEJjTCtGaFlXRmhZV0ZhWGc4RU15d1ZRdmtVTnAwSGJFSm1RQUFBQUJKUlU1RXJrSmdnZz09IiBhbHQ9ImJhcnBsb3QiIHN0eWxlPSJoZWlnaHQ6IGF1dG87IHdpZHRoOiAxMjBweDsiPjwvdGQ+CiAgICAgICAgICAgIDwvdHI+CiAgICAgICAgICAgIDx0cj4KICAgICAgICAgICAgICAgIDx0ZCBjbGFzcz0ibm8tY29sIj40PC90ZD4KICAgICAgICAgICAgICAgIDx0ZCBjbGFzcz0idmFyaWFibGUtY29sIj5vc19ldmVudDxicj5bZXZlbnQgaW5kaWNhdG9yXTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InN0YXRzLWNvbCI+MTogMTAwMDAgKDEwMCUpPGJyPk1pc3Npbmc6IDAgKDAlKTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9ImdyYXBoLWNvbCI+PGltZyBzcmM9ImRhdGE6aW1hZ2UvcG5nO2Jhc2U2NCxpVkJPUncwS0dnb0FBQUFOU1VoRVVnQUFBSGdBQUFBb0NBTUFBQUFDTk00WEFBQUFMVkJNVkVXWm1abW9xS2lycTZ1dHJhMndzTEMydHJhNXVibS92Ny9JeU1qTnpjM1QwOVBmMzkvbDVlWHI2K3YvLy8vVCtzUStBQUFBQ1hCSVdYTUFBQTdEQUFBT3d3SEhiNmhrQUFBQVNVbEVRVlJZaGUzV3VRR0FNQkRFd09VSFA5ZC91UzVEQkZJRGt5b0ZsUkdvdmplaVMxaFlXRmhZK0kvdzloS2RtUWRUc1BYQjRQbEJjTCtGaFlXRmhZV0ZhWGc4RU15d1ZRdmtVTnAwSGJFSm1RQUFBQUJKUlU1RXJrSmdnZz09IiBhbHQ9ImJhcnBsb3QiIHN0eWxlPSJoZWlnaHQ6IGF1dG87IHdpZHRoOiAxMjBweDsiPjwvdGQ+CiAgICAgICAgICAgIDwvdHI+CiAgICAgICAgPC90Ym9keT4KICAgIDwvdGFibGU+CjwvYm9keT4KPC9odG1sPgo=

For verification only, we can call the generator directly. This is not
the recommended way to use `TrialSimulator` for simulation studies; in
practice, the generator should usually be supplied to
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
and used inside the trial simulation workflow. Direct calls are helpful
here because they let us check the marginal medians, the observed
Kendall’s tau, and the ordering $`\mbox{PFS} \leq \mbox{OS}`$ before
adding enrollment, censoring, treatment arms, and analyses.

``` r

set.seed(123)
dat <- CorrelatedPfsAndOs2(n = 10000,
                           median_pfs = 5,
                           median_os = 11,
                           kendall = 0.6)

head(dat, 2)
#>        pfs       os pfs_event os_event
#> 1 2.744789 19.37430         1        1
#> 2 5.342269 25.04203         1        1
```

The simulation should approximately recover the requested medians and
Kendall’s tau between observed, uncensored PFS and OS times.

``` r

with(dat, median(pfs))
#> [1] 5.102113
with(dat, median(os))
#> [1] 11.28231
with(dat, cor(pfs, os, method = 'kendall'))
#> [1] 0.5954657
with(dat, all(pfs <= os))
#> [1] TRUE
```

Because this generator returns event indicators equal to 1 for both
endpoints, censoring and staggered enrollment should be handled by the
broader trial simulation workflow rather than by this endpoint
generator.

## Practical Guidance

This generator is most useful when the simulation objective is to
specify marginal PFS and OS medians and a rank-based association between
the two observed endpoints. Kendall’s tau is often more stable and
interpretable than Pearson correlation for skewed time-to-event
variables, and it is directly connected to the Gumbel copula parameter.

There are still modeling assumptions to keep in mind:

- PFS and OS are both marginally exponential.
- Dependence is induced through a Gumbel survival copula on latent TTP
  and OS.
- The Gumbel copula only allows non-negative dependence.
- The requested Kendall’s tau is for observed PFS and OS event times
  before censoring is applied.
- The attainable Kendall’s tau between observed PFS and OS is
  constrained by the PFS/OS median ratio.

For simulations that require transition-specific hazards or a
mechanistic disease process, the illness-death model implemented in
[`TrialSimulator::CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md)
may be more appropriate. For simulations where the main requirement is a
direct medians-plus-Kendall’s-tau specification, especially when OS will
be analyzed under a proportional hazards Cox model,
[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
provides a compact alternative.

## Appendix: Kendall’s Tau for Observed PFS and OS

This appendix gives the derivation behind the formula used in
[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md).

Let $`X`$ and $`Y`$ be nonnegative random variables with exponential
margins

``` math
X \sim \operatorname{Exp}(\lambda_X),
\qquad
Y \sim \operatorname{Exp}(\lambda_Y),
```

and joint survival function

``` math
S_{X,Y}(x,y)
=
\Pr(X>x, Y>y)
=
\exp\left[
-\left\{(\lambda_X x)^\theta + (\lambda_Y y)^\theta\right\}^{1/\theta}
\right],
\qquad \theta \ge 1.
```

Define $`P=\min(X,Y)`$. We want Kendall’s tau between $`P`$ and $`Y`$.

Standardize the margins by setting

``` math
A=\lambda_X X, \qquad B=\lambda_Y Y.
```

Then the joint survival function of $`(A,B)`$ is

``` math
\Pr(A>a, B>b)
=
\exp\left\{-(a^\theta+b^\theta)^{1/\theta}\right\}.
```

Use the polar-type transformation

``` math
R=(A^\theta+B^\theta)^{1/\theta},
\qquad
W=\frac{A^\theta}{A^\theta+B^\theta}.
```

A Jacobian calculation gives the joint density

``` math
f_{R,W}(r,w)=\frac{1}{\theta} e^{-r}(r+\theta-1),
\qquad r>0,\; 0<w<1.
```

Thus, $`R`$ and $`W`$ are independent and
$`W \sim \operatorname{Unif}(0,1)`$. The inverse transformation is

``` math
A = R W^{1/\theta},
\qquad
B = R(1-W)^{1/\theta}.
```

Let

``` math
a=\lambda_X^\theta,
\qquad
b=\lambda_Y^\theta,
\qquad
c=\frac{a}{a+b},
\qquad
d=\frac{b}{a+b}.
```

The event $`X \le Y`$ is equivalent to $`W \le c`$:

``` math
X \le Y
\iff
\frac{A}{\lambda_X} \le \frac{B}{\lambda_Y}
\iff
W \le c,
```

so

``` math
P=
\begin{cases}
X, & W \le c,\\
Y, & W > c.
\end{cases}
```

Using the survival-copula representation of Kendall’s tau,

``` math
\tau(P,Y)=4E\{S_{P,Y}(P,Y)\}-1.
```

When $`W \le c`$, $`P=X`$ and

``` math
S_{P,Y}(P,Y)=S_{X,Y}(X,Y)=e^{-R}.
```

When $`W>c`$, $`P=Y`$. Since

``` math
S_{P,Y}(y,y)=\Pr(P>y,Y>y)=\Pr(X>y,Y>y),
```

we have, using $`Y=B/\lambda_Y=R(1-W)^{1/\theta}/\lambda_Y`$,

``` math
S_{P,Y}(Y,Y)
=
\exp\left[
-R\left(\frac{1-W}{d}\right)^{1/\theta}
\right].
```

Therefore,

``` math
E\{S_{P,Y}(P,Y)\}
=
E\{e^{-R}\mathbf 1(W\le c)\}
+
E\left[
e^{-R((1-W)/d)^{1/\theta}}
\mathbf 1(W>c)
\right].
```

By independence of $`R`$ and $`W`$,

``` math
E\{S_{P,Y}(P,Y)\}
=
cL_R(1)
+
\int_c^1
L_R\left\{\left(\frac{1-w}{d}\right)^{1/\theta}\right\}
dw,
```

where $`L_R(s)=E(e^{-sR})`$. From the density of $`R`$,

``` math
L_R(s)
=
\frac{1}{\theta}
\int_0^\infty e^{-(1+s)r}(r+\theta-1)\,dr
=
\frac{\theta+(\theta-1)s}{\theta(1+s)^2}.
```

In particular,

``` math
L_R(1)=\frac{2\theta-1}{4\theta}.
```

For the integral term, use the substitution

``` math
t=\left(\frac{1-w}{d}\right)^{1/\theta},
\qquad
dw=-d\theta t^{\theta-1}dt.
```

Then

``` math
\int_c^1
L_R\left\{\left(\frac{1-w}{d}\right)^{1/\theta}\right\}
dw
=
d\theta\int_0^1 t^{\theta-1}L_R(t)\,dt.
```

Substituting $`L_R(t)`$ gives

``` math
d\int_0^1
\frac{\theta t^{\theta-1}+(\theta-1)t^\theta}{(1+t)^2}
dt.
```

Because

``` math
\frac{d}{dt}\left(\frac{t^\theta}{1+t}\right)
=
\frac{\theta t^{\theta-1}+(\theta-1)t^\theta}{(1+t)^2},
```

the integral is

``` math
d\left[\frac{t^\theta}{1+t}\right]_0^1
=
\frac{d}{2}.
```

Thus,

``` math
E\{S_{P,Y}(P,Y)\}
=
c\frac{2\theta-1}{4\theta}+\frac{d}{2}
=
\frac{2\theta-1+d}{4\theta},
```

using $`c+d=1`$. Finally,

``` math
\tau(P,Y)
=
4E\{S_{P,Y}(P,Y)\}-1
=
\frac{\theta-1+d}{\theta}
=
1-\frac{1}{\theta}
\left(
\frac{\lambda_X^\theta}{\lambda_X^\theta+\lambda_Y^\theta}
\right).
```

Since $`P`$ has rate

``` math
\lambda_P =
\left(\lambda_X^\theta+\lambda_Y^\theta\right)^{1/\theta},
```

this can also be written as

``` math
\tau(P,Y)
=
1-\frac{1}{\theta}
\left(
\frac{\lambda_X^\theta}{\lambda_P^\theta}
\right)
=
1-\frac{1}{\theta}
\left[
1-\left(\frac{\lambda_Y}{\lambda_P}\right)^\theta
\right].
```

Because $`\lambda_Y/\lambda_P = m_P/m_Y`$, the formula used by
[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
is

``` math
\tau(P,Y)
=
1-\frac{1}{\theta}
\left[
1-\left(\frac{m_P}{m_Y}\right)^\theta
\right].
```
