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

CjwhRE9DVFlQRSBodG1sPgo8aHRtbD4KPGhlYWQ+CiAgICA8bWV0YSBjaGFyc2V0PSJVVEYtOCI+CiAgICA8dGl0bGU+RW5kcG9pbnRzICgyKTwvdGl0bGU+CiAgICA8c3R5bGU+CiAgICAgICAgYm9keSB7CiAgICAgICAgICAgIGZvbnQtZmFtaWx5OiBBcmlhbCwgc2Fucy1zZXJpZjsKICAgICAgICAgICAgbWFyZ2luOiAyMHB4OwogICAgICAgICAgICBiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTsKICAgICAgICAgICAgZGlzcGxheTogZmxleDsKICAgICAgICAgICAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjsKICAgICAgICAgICAgYWxpZ24taXRlbXM6IGNlbnRlcjsKICAgICAgICB9CiAgICAgICAgaDEgewogICAgICAgICAgICBjb2xvcjogYmxhY2s7CiAgICAgICAgICAgIHRleHQtYWxpZ246IGNlbnRlcjsKICAgICAgICAgICAgbWFyZ2luLWJvdHRvbTogMjBweDsKICAgICAgICAgICAgZm9udC1zaXplOiAyMHB4OwogICAgICAgIH0KICAgICAgICAuc3VidGl0bGUgewogICAgICAgICAgICB0ZXh0LWFsaWduOiBjZW50ZXI7CiAgICAgICAgICAgIGNvbG9yOiAjNjY2OwogICAgICAgICAgICBtYXJnaW4tYm90dG9tOiAyMHB4OwogICAgICAgICAgICBmb250LXNpemU6IDE2cHg7CiAgICAgICAgfQogICAgICAgIHRhYmxlIHsKICAgICAgICAgICAgYm9yZGVyLWNvbGxhcHNlOiBjb2xsYXBzZTsKICAgICAgICAgICAgZm9udC1zaXplOiAxNHB4OwogICAgICAgICAgICBib3JkZXI6IDFweCBzb2xpZCAjOTk5OwogICAgICAgICAgICB3aWR0aDogYXV0bzsKICAgICAgICAgICAgbWFyZ2luOiAwIGF1dG87CiAgICAgICAgfQogICAgICAgIHRoIHsKICAgICAgICAgICAgYmFja2dyb3VuZC1jb2xvcjogI2YwZjBmMDsKICAgICAgICAgICAgY29sb3I6IGJsYWNrOwogICAgICAgICAgICBwYWRkaW5nOiAxMHB4OwogICAgICAgICAgICB0ZXh0LWFsaWduOiBsZWZ0OwogICAgICAgICAgICBmb250LXdlaWdodDogbm9ybWFsOwogICAgICAgICAgICBib3JkZXI6IDFweCBzb2xpZCAjOTk5OwogICAgICAgICAgICB3aGl0ZS1zcGFjZTogbm93cmFwOwogICAgICAgICAgICBmb250LXNpemU6IDE0cHg7CiAgICAgICAgfQogICAgICAgIHRkIHsKICAgICAgICAgICAgcGFkZGluZzogMTBweDsKICAgICAgICAgICAgYm9yZGVyOiAxcHggc29saWQgIzk5OTsKICAgICAgICAgICAgdmVydGljYWwtYWxpZ246IHRvcDsKICAgICAgICAgICAgbGluZS1oZWlnaHQ6IDEuNDsKICAgICAgICAgICAgZm9udC1zaXplOiAxNHB4OwogICAgICAgIH0KICAgICAgICAubm8tY29sIHsKICAgICAgICAgICAgdGV4dC1hbGlnbjogY2VudGVyOwogICAgICAgICAgICB3aGl0ZS1zcGFjZTogbm93cmFwOwogICAgICAgIH0KICAgICAgICAudmFyaWFibGUtY29sIHsKICAgICAgICAgICAgd2hpdGUtc3BhY2U6IG5vd3JhcDsKICAgICAgICB9CiAgICAgICAgLnN0YXRzLWNvbCB7CiAgICAgICAgfQogICAgICAgIC5mcmVxcy1jb2wgewogICAgICAgICAgICBsaW5lLWhlaWdodDogMjBweDsKICAgICAgICB9CiAgICAgICAgLmdyYXBoLWNvbCB7CiAgICAgICAgICAgIHRleHQtYWxpZ246IGNlbnRlcjsKICAgICAgICAgICAgd2hpdGUtc3BhY2U6IG5vd3JhcDsKICAgICAgICAgICAgdmVydGljYWwtYWxpZ246IHRvcDsKICAgICAgICB9CiAgICAgICAgaW1nIHsKICAgICAgICAgICAgZGlzcGxheTogYmxvY2s7CiAgICAgICAgICAgIG1hcmdpbjogMCBhdXRvOwogICAgICAgICAgICB2ZXJ0aWNhbC1hbGlnbjogdG9wOwogICAgICAgIH0KICAgIDwvc3R5bGU+CjwvaGVhZD4KPGJvZHk+CiAgICA8aDE+RW5kcG9pbnRzICgyKTwvaDE+CiAgICA8ZGl2IGNsYXNzPSJzdWJ0aXRsZSIgc3R5bGU9InRleHQtYWxpZ246IGxlZnQ7Ij4KICAgICAgICBwZnMsIG9zPGJyPgogICAgPC9kaXY+CgogICAgPHRhYmxlPgogICAgICAgIDx0aGVhZD4KICAgICAgICAgICAgPHRyPgogICAgICAgICAgICAgICAgPHRoIGNsYXNzPSJuby1jb2wiPk5vPC90aD4KICAgICAgICAgICAgICAgIDx0aCBjbGFzcz0idmFyaWFibGUtY29sIj5WYXJpYWJsZTwvdGg+CiAgICAgICAgICAgICAgICA8dGggY2xhc3M9InN0YXRzLWNvbCI+U3RhdHMgLyBGcmVxczwvdGg+CiAgICAgICAgICAgICAgICA8dGggY2xhc3M9ImdyYXBoLWNvbCI+R3JhcGg8L3RoPgogICAgICAgICAgICA8L3RyPgogICAgICAgIDwvdGhlYWQ+CiAgICAgICAgPHRib2R5PgogICAgICAgICAgICA8dHI+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9Im5vLWNvbCI+MTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InZhcmlhYmxlLWNvbCI+cGZzPGJyPlt0aW1lLXRvLWV2ZW50XTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InN0YXRzLWNvbCI+TWVkaWFuIHRpbWU6IDU8YnI+RXZlbnRzOiAxMDAwMDxicj5NaXNzaW5nOiAwICgwJSk8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJncmFwaC1jb2wiPjxpbWcgc3JjPSJkYXRhOmltYWdlL3BuZztiYXNlNjQsaVZCT1J3MEtHZ29BQUFBTlNVaEVVZ0FBQUhnQUFBQlFDQU1BQUFEbFJVRzdBQUFDOUZCTVZFVUhCd2NJQ0FnSkNRa0tDZ29MQ3dzTURBd05EUTBPRGc0UER3OFFFQkFSRVJFU0VoSVRFeE1VRkJRVkZSVVdGaFlYRnhjWUdCZ1pHUmthR2hvYkd4c2NIQndkSFIwZUhoNGZIeDhnSUNBaElTRWlJaUlrSkNRbEpTVW5KeWNvS0NncEtTa3FLaW90TFMwdUxpNHZMeTh3TURBeE1URXpNek0wTkRRMU5UVTJOalkzTnpjNE9EZzZPam85UFQwK1BqNC9QejlCUVVGRFEwTkVSRVJHUmtaSFIwZElTRWhKU1VsS1NrcE1URXhOVFUxT1RrNVBUMDlSVVZGU1VsSlZWVlZhV2xwYlcxdGNYRnhnWUdCaFlXRmtaR1JtWm1abloyZHJhMnRzYkd4dGJXMXVibTV3Y0hCeGNYRnljbkowZEhSM2QzZDRlSGg1ZVhsNmVucDdlM3Q4Zkh4OWZYMStmbjZCZ1lHQ2dvS0RnNE9FaElTRmhZV0ZuTmlHaG9hSWlJaUppWW1LaWFPTWpJeU5qWTJPam82UGo0K1FrSkNSa1pHU2luU1hsNWVZbUppWm1abVpuYTJibTV1Y25KeWRuWjJmbjUrZ29LQ2lvcUtqbzZPa29iU25uTHVvbXJxb3FLaXBxYW1xbTdxcXFxcXJyYTZ0cGJ1dHJhMnVxc0N1cnE2dnI2K3Zzc3V2dGMyeXNyS3pzN08wdExTMXRyYTJzTVcydHJhM3BiUzN0N2UzdnRTNHBLSzRzc2U0dUxpNHVibTR1OUM1dWJxNXY5VzZ1cnE3dTd1OG9adTh2THk5eWVDOTBPaStwYk8rcWJPK3ZyNi9wTEsvdjcvQXJMWEF3TURBMCtyQytmL0R3OFBFclpmRXhNVEdzYTdHeHNiSHg4Zkl5TWpKeWNuS3lzckwwTTNNek16TnM2M04wczdQdUxUUHo4L1F6YzNSdXJmUjBkSFIrLy9TMHRMVDA5UFUxTlRWMWRYVzF0YlcyZkhYMTlmWXQ3L1ovUC9iMjl2Yi9QL2QzZDNmMzkvZi9QL2cvUC9pemREaTR1TGo0K1BrNU9UbDVlWG16cnptK3ZMbS9mL241K2ZuL2YvbzBiL281ZWpvNk9qby9mL3MzT0xzNnV6czdPenMvdi90NnRqdDdlM3U3dTd1L3YvdjcrL3Yvdi93OFBEdytmL3krdi95Ly8vejZ1eno4L1AwKy8vMTE3RDE5ZlgxOXY3Mjl2YjM5L2Y0K1BqNStmbjUvLy82OHU3Ni8vLzcrL3Y3Ly8vODl2RDgvUHo5NXVQOS9mMys2YjcrN3V6Ky92Ny8rTzcvK2ZILytmai8rL1QvLy9mLy8vODJwV2J4QUFBQUNYQklXWE1BQUE3REFBQU93d0hIYjZoa0FBQUROa2xFUVZSb2dlM1pkMVJTVVJ3SGNDcExjMnRwam9aV05yV2hEVWRLV1pvdFFyTnRZWlpaMGRUS29tSER6S3kwWWJhWGxiYXpLV1lEbTFvMmJlOU5WcmF0N1A1VElPOXhMeURacVh2dlAzelA0Y0QzUEhnZmVPZHdlZThIQjFBS1J3Y1RnMHNLbnRHQnArNWFmWWdLbkJlLzhqRlRvZ2J6QWNqZFFRWUc0T3RkcG9RSEdBQ1EwSVlNL0dYaWttdHN5NjFXZWlNQi8zd0pOVGxxZklNSWpEUTU3SnBDQ2VhRlVZSWpmU25CWWdkSzhPZXFEK2pBb0hVQ0NYakY2T25IVmVDUVFCTHdubW16THpGRmttQXV1OXZxUWdJRzROTkZwbkRyVlpUZFNmVmZFNEEzSkMwK3pUYkZjdGswblFDOEwyN2hUVlc0K3pBQ01OSVU4RngzU3ZBOXc0OTBZRkQzSUNXNFN3UWxlRVk3U3ZCVG8xZlk0ZjNiNXFDblB2SzBtb2NkM2pKazVrbDFlSlFmZGhoZU1wc1pNRWMreHdZN2ZDQnBHYnR5dmNpeVpCN1dQb3diWGhNMy94VGJsS2Uyd2IxeHcwaFR3bUs3WWpvd2NOcE9DZTZQK1RTa1REalRCdThQUlprd2FCNUxDWTUyeFF0L3VIWDBqVWE0eUVxTUZYNCtZdEI5alRBWTJBMHJETDVkWjVkTW5rZGxhTk41TXlsTytQTFNUZStac2w1b0FtL3pHb3NUbnJ4M25YTDRnazRETmpwaS9FWnh2bDk1cUd3cVl3aVhHSXd3MGxUZzJQcjRQckpXdU5nTjM1bTlWaGhrbStUU2dVSFB6cFRnTzJaSGNNRnZILzNRdkdTV1pveFRJU1o0MHFJRm03WEF3S01YSnZqTWhXUHNLR0pXcUpIYUUvSXRFL0hBSlFWUDJDSHFBRjk5OVdla21xWmlnWkY1dGNZQjZuSXpIRk1nVGw3OHFuZmFZWkJtSG9VQlJsb1pJK05NUjU5OEtqQW83R3N4bmdvTVFFWmp6MndxTUNnS01mVmVXMFFCQmtBNnZJRnBqNTBVNE44NUVWalROa0NVOHo4dXF6Uk85clFsUStCWnZWSUx2MzVSS1puL05GMUdKbnRYMHl6SzlhTGJ5U09EMjdkMDFPTllPemkzNVhJNzhmaEJBa0dFVUNpTUZJbEVzY215cE85R0kxR0UvVm9pRXdGM1c2dS9ldGZTcytMMGVKRm8zRkJCQ0ovZjFkL2YzNWZMNVhxNHllTGNFSTI5SXV6bENYcW9DUVlkb3NvU2JtZ1BwWVllM093cjJNR3Rpalhjak0zaFpvbnVSbTJtb3Y0MzdnUWZDWlRFV25DVGNMTGcxbWdLM0RyMmdWdVlON0tiT24rR0U0UGdkczRMMmRnRVdVSTZaTUV0TkxyY3U5RUlFNG9PMXNFNldBZnI0SExuRjhxRlZVVTEzOWxQQUFBQUFFbEZUa1N1UW1DQyIgYWx0PSJLTSBjdXJ2ZSI+PC90ZD4KICAgICAgICAgICAgPC90cj4KICAgICAgICAgICAgPHRyPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJuby1jb2wiPjI8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJ2YXJpYWJsZS1jb2wiPm9zPGJyPlt0aW1lLXRvLWV2ZW50XTwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InN0YXRzLWNvbCI+TWVkaWFuIHRpbWU6IDEwLjg0PGJyPkV2ZW50czogMTAwMDA8YnI+TWlzc2luZzogMCAoMCUpPC90ZD4KICAgICAgICAgICAgICAgIDx0ZCBjbGFzcz0iZ3JhcGgtY29sIj48aW1nIHNyYz0iZGF0YTppbWFnZS9wbmc7YmFzZTY0LGlWQk9SdzBLR2dvQUFBQU5TVWhFVWdBQUFIZ0FBQUJRQ0FJQUFBQmQrU2JlQUFBQUNYQklXWE1BQUE3REFBQU93d0hIYjZoa0FBQUlFVWxFUVZSNG5PMmNiVXdUV3hySHA5T1dZY3RBS2JSYVduZEJ3d29VRUM1UVhRV0NYa1hGUXN4cTFxanhFeVorTVpGRXhjUkVvNUtZYVB4aTRrdDBGMDJ6eTcweDlRSkdzM0J0Z0pRc3NySmFiS3VCTFJVUVMxOUlGU2gwU3QrbXN4L1lKU3lYbHFHZEdlemMrWDNyOUpsbi92bWZNK2RsWnM1aFlSZ0dNSkFQdU5ZQ2ZpMHdSbE1FWXpSRmdFQUErZXI0aWdUV1dnamRZZWYrYmllUTVCdlZhR2R6TnE5bjZqZHBjTXFLUVoxMUFzck0zOGdKR2RUVTFHUTJtNTFPNTQwYk53QUFzRnF0Rm90RkxwZFRKelAyQWRPKzIxMVZWYlhyOTNBZzlERHZ5NWN2Rm92bHdZTUg4ei8xZXYyVksxY29Fa2dYV01HSnJoOS9kZ3NsR3dxL0x3alRkRml0VnJsY2JyRllBQUN3MisyRmhZVjJ1NTA2bWJFUGg3VnUreC8vQlBGK3c4Si9qbGdzWnJGWU5wc3RMUzJOUEdVMEF3U0ErRlc1UEU5QlFZRmVyeWRERUYySmNKeXhiZHUydnI0K1lxWFFtd2lOTGlvcTZ1L3ZKMVlLdlluUTZKS1NrcmR2M3hJcmhkNUVhTFJVS2dWQjBHdzJFNnVHeGtRK0YyU2E2VlVCZHY2MThjK1BXMXArN0JoQ1YzY21ZL1NxQUFVcEtTbkpMREF4T1NuMEdHOWtaTVJnTUtEby94VkZhV2xwYjI4djZRTHBBbXYrRFV2UU5qNnhma05haUlia3lKRWpScVBSWkRLNTNlNkZnMTZ2VnlRU09Sd09DSUtvMFJyVGNMcWIyenlKSE1DYmxIMWdRNmdnbFVvMVB3VmZmQkNDb0MxYnRyeDU4NmFzckl4OG5URVB5SWN4RkVVeE9CRmVmYi9JTk5PckFNT0h4V0tSU0NSTERqNS8vbHloVU9ETThDc25xa2Y5TzNiczZPdnJXOUpKTWl4TFZFYW5wcVptWkdRd2MzRThSUHZ5cXFLaVFxUFJFS0dFNWtScjlKNDllem83T3dtUlFtK2lOYnFzck96MTY5ZCt2NThRTlRRRzFQZjMvL041NjdPMi9vbGdKT2ZETUN5VHlaaEIzb3FBcmsvLzZ2azNnaUd6czVGK2c3ZHYzejYxV2syb0tqb3lQOHBEcldZckduSU1XRjVlenVmeklRaGE5dC9lM3Q2dFc3ZVNNZmFrRXl5OXV0MkdnYXk0RGNVVnN0UVF6NVhjYnZmWTJOanUzYnV0VnVzdi8wVlJWQ3dXRHc0T0NvVkNjaXRGTEFOK25RcWdLSW82cDJkRHQ5RThIby9QNTdOWXk1Y0RtODJ1ckt4c2IyOG5TeU05d0Zuemw1MkNMOURhMnJwMzcxNkNiako2UXN6WGRsVlZWVHFkem1hekVaS05saEJqTkFSQmh3NGRVcWxVaEdTakpZUjlQM3I0OE9HV2xoYWlzdEVQd295dXFLZ3dtVXlqbzZORUphUVpoQm5ONVhKUG5EaWhWQ3FKU2tnelFIVE9PZWt3RGc3TitLTE9WVnRiKytqUkkrYTV4N0tBM2pITkQ4ckcxbi9ZM0ZFdmc4dk96czdMeTJPNnhPWEJNQXp6VEg3NStEbk1GUHpzMmJQVjFkVXdESzg0V2xTcjFmbjUrY0Zna0xnQktFMEFKd3dhZGZmYkQzUHhvdEROZFUxTlRVMU5UWHg4L0lyRlZsbFp5V2F6dTd1N2lhd0x0QURVOVBtRmFVSk0vM29nOU1Lc25UdDNWbGRYeDhYRjRjbDQ1c3labXpkdkVpYVFObUJlcDkxaW4vYXVVUFBEVDhFWDQvUDVwRktwMFdnazRINmpFU0FRbDdSZXNwNlBxN0xpZ3N2bG5qNTkrdXJWcTRSbHBBV2tyQ3lzcTZ2VGFEVHYzcjBqSTNtTVFvclJQQjZ2b2FIaDFLbFR6Smg2QWJMV3lwNDhlVEk1T2ZuT25Uc2s1WTg5Y0xibCtEdkRCVXdtazFBby9QVHAwK3A3RGhvQyttZW5adjBBNFBOR1B3VmZRbVptWm4xOS9mSGp4MzArd25QSEhtQ3pTdHVyK3R2UFAzWDBrZURHK2ZQbmhVSmhmWDA5OGFsakRVN3BkeUpPL3JhWlRpMkxIVExvMmJOblEwTkRIbzludGRsQkVGUXFsY1hGeFFVRkJiVzF0VkVwalhFNHY5MlMrWFhLSXkzTzk0ZnVGejk4K0dBMEdnT0JTRGIxRUFnRUwxKytMQzh2VDAxTlBYandZT1JLWXgzVlg5UnZkTnF1SDE3by9lSGE4Z2c2dzhWb3RkcDE2OVkxTnpkSG5DSFdBY3VLUVlkMUFzdVY1NFRlcnlONmlvcUsxR3AxWFYzZC9mdjNTYnpNdHd6T0FvbXlSczh6T2pxYWs1Tno3dHc1ajhjVFphcVlnOUxOZlRJeU1ucDZlc2JIeDJVeVdVOVBENVdYWG50d0ZnZ2hOWHFCRnk5ZXBLZW5IenQyekdBd0VKWHpHMmR0dHF1cXJxNGVHQmlReVdUNzkrOC9ldlNvd1dCWUV4bFVzbWI3Z3ZGNHZFdVhMcGxNcHNMQ3dnTUhEbXpmdnYzdTNidkR3OE5ycFlkcy9ydHlka1VXNzZsRU9DaUt0clcxTlRjM3Q3ZTNpMFNpMHRKU3VWeStlZlBtek14TWlVUkN4aFdwaDZYNTZlLy9Xem43aC9UUWswTlNqVjRnRUFqb2RMcFhyMTdwZExxUEh6OE9EZzV5T0p6MDlQVHM3R3lKUkpLVmxaV1dsaWFWU2lVU1NVcEtDcWxLQ0lmRGh6RUVSY0d3SzJjbkppYkd4c2FDd1lnV1g2eEtEWWRUVWxKU1VsS3ljTVJxdFE0UEQ0K01qSHorL0xtcnE4dG1zOWxzdHZIeDhabVpHWkZJQk1Od1ltSmlRa0lDbjg4WENBUXdEQ2NrSkVBUWxKeWNEQUFBajhlRElFZ2dFSEM1WEJpR1ExMVVJQkJFcGhhR1lTNlh1L2dJajhjVGk4WExCdU5hZEs5UUtBWUdCZ0tCd0xlekUwb3dHSFE0SEFpQ3pNek1JQWppZERvbkp5ZmRicmZMNWZKNnZVNm5Fd0FBdDl2dDhYaW1wNmQ5UGgrQ0lNdm13VEJzZW5vNk1nMHVsMnZKbTQyTkd6ZDJkSFFzRzR5MzZXQ0lFbHhOeHhKdTM3N2QwTkN3NGgzbmRydTlYaStlTUovUE4zK3pod0ZCRUwvZmp5Y3NFQWp3K1h4Q3dsd3VWekFZVEVwS1dqRk1vVkE4ZnZ3NFRBeW5jSitpTUh5YVh5QVFDSGJ0Mm5YcjFxM3dZVStmUHRWcXRmUGJtWWJoeVpNbjc5Ky92Mzc5ZXZpd3BxWW1rOGwwN2RxMThHRktwZEpzTmwrK2ZEbDhXR05qbzhQaHVIanhZdml3aHc4Zk9wM09DeGN1aEErN2QrL2VpcHVXUlBJa1NTd1daMlZsYmRxMEtYeFlYbDRlZ2lCNHd2eCtQNTR3TnB1OVlsaHViaTRNdzNpeTJlMTJQTm1tcHFid2hNM056WVdQd1R1T1pvZ1Nac2RvaW1DTXBnakdhSXBnaktZSXhtaUtZSXltQ01ab2ltQ01wZ2pHYUlwZ2pLWUl4bWlLK0E4S1doUS9IY2gxUmdBQUFBQkpSVTVFcmtKZ2dnPT0iIGFsdD0iS00gY3VydmUiPjwvdGQ+CiAgICAgICAgICAgIDwvdHI+CiAgICAgICAgICAgIDx0cj4KICAgICAgICAgICAgICAgIDx0ZCBjbGFzcz0ibm8tY29sIj4zPC90ZD4KICAgICAgICAgICAgICAgIDx0ZCBjbGFzcz0idmFyaWFibGUtY29sIj5wZnNfZXZlbnQ8YnI+W2V2ZW50IGluZGljYXRvcl08L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJzdGF0cy1jb2wiPjE6IDEwMDAwICgxMDAlKTxicj5NaXNzaW5nOiAwICgwJSk8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJncmFwaC1jb2wiPjxpbWcgc3JjPSJkYXRhOmltYWdlL3BuZztiYXNlNjQsaVZCT1J3MEtHZ29BQUFBTlNVaEVVZ0FBQUhnQUFBQW9DQU1BQUFBQ05NNFhBQUFBTFZCTVZFV1ptWm1vcUtpcnE2dXRyYTJ3c0xDMnRyYTV1Ym0vdjcvSXlNak56YzNUMDlQZjM5L2w1ZVhyNit2Ly8vL1Qrc1ErQUFBQUNYQklXWE1BQUE3REFBQU93d0hIYjZoa0FBQUFTVWxFUVZSWWhlM1d1UUdBTUJERXdPVUhQOWQvdVM1REJGSURreW9GbFJHb3ZqZWlTMWhZV0ZoWStJL3c5aEtkbVFkVHNQWEI0UGxCY0wrRmhZV0ZoWVdGYVhnOEVNeXdWUXZrVU5wMEhiRUptUUFBQUFCSlJVNUVya0pnZ2c9PSIgYWx0PSJiYXJwbG90IiBzdHlsZT0iaGVpZ2h0OiBhdXRvOyB3aWR0aDogMTIwcHg7Ij48L3RkPgogICAgICAgICAgICA8L3RyPgogICAgICAgICAgICA8dHI+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9Im5vLWNvbCI+NDwvdGQ+CiAgICAgICAgICAgICAgICA8dGQgY2xhc3M9InZhcmlhYmxlLWNvbCI+b3NfZXZlbnQ8YnI+W2V2ZW50IGluZGljYXRvcl08L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJzdGF0cy1jb2wiPjE6IDEwMDAwICgxMDAlKTxicj5NaXNzaW5nOiAwICgwJSk8L3RkPgogICAgICAgICAgICAgICAgPHRkIGNsYXNzPSJncmFwaC1jb2wiPjxpbWcgc3JjPSJkYXRhOmltYWdlL3BuZztiYXNlNjQsaVZCT1J3MEtHZ29BQUFBTlNVaEVVZ0FBQUhnQUFBQW9DQU1BQUFBQ05NNFhBQUFBTFZCTVZFV1ptWm1vcUtpcnE2dXRyYTJ3c0xDMnRyYTV1Ym0vdjcvSXlNak56YzNUMDlQZjM5L2w1ZVhyNit2Ly8vL1Qrc1ErQUFBQUNYQklXWE1BQUE3REFBQU93d0hIYjZoa0FBQUFTVWxFUVZSWWhlM1d1UUdBTUJERXdPVUhQOWQvdVM1REJGSURreW9GbFJHb3ZqZWlTMWhZV0ZoWStJL3c5aEtkbVFkVHNQWEI0UGxCY0wrRmhZV0ZoWVdGYVhnOEVNeXdWUXZrVU5wMEhiRUptUUFBQUFCSlJVNUVya0pnZ2c9PSIgYWx0PSJiYXJwbG90IiBzdHlsZT0iaGVpZ2h0OiBhdXRvOyB3aWR0aDogMTIwcHg7Ij48L3RkPgogICAgICAgICAgICA8L3RyPgogICAgICAgIDwvdGJvZHk+CiAgICA8L3RhYmxlPgo8L2JvZHk+CjwvaHRtbD4K

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
#> 1 6.534539 6.534539         1        1
#> 2 3.116476 3.116476         1        1
```

The simulation should approximately recover the requested medians and
Kendall’s tau between observed, uncensored PFS and OS times.

``` r

with(dat, median(pfs))
#> [1] 5.013457
with(dat, median(os))
#> [1] 11.35727
with(dat, cor(pfs, os, method = 'kendall'))
#> [1] 0.6068072
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
