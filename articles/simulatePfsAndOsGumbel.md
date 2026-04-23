# Simulate Correlated Progression-Free Survival and Overall Survival Using a Gumbel Copula

Progression-free survival (PFS) and overall survival (OS) are common
time-to-event endpoints in oncology trials. Because progression or death
defines a PFS event, simulated trial data should always satisfy
$\text{PFS} \leq \text{OS}$. At the same time, the two endpoints are
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

$$\text{PFS} = \min\left( \text{TTP},\text{OS} \right).$$

This construction guarantees $\text{PFS} \leq \text{OS}$ for every
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

Let $X$ denote latent TTP and let $Y$ denote OS. The model assumes
exponential marginal survival functions

$$\Pr(X > x) = \exp\left( - \lambda_{X}x \right),\qquad\Pr(Y > y) = \exp\left( - \lambda_{Y}y \right),$$

and a Gumbel–Hougaard survival copula

$$\Pr(X > x,Y > y) = \exp\left\lbrack - \left\{ \left( \lambda_{X}x \right)^{\theta} + \left( \lambda_{Y}y \right)^{\theta} \right\}^{1/\theta} \right\rbrack,\qquad\theta \geq 1.$$

The observed PFS time is

$$P = \min(X,Y).$$

Under this model, OS is exponential with rate $\lambda_{Y}$. PFS is also
exponential, with survival function

$$\Pr(P > t) = \Pr(X > t,Y > t) = \exp\left\lbrack - \left\{ \left( \lambda_{X}^{\theta} + \lambda_{Y}^{\theta} \right)t^{\theta} \right\}^{1/\theta} \right\rbrack = \exp\left( - \lambda_{P}t \right),$$

where

$$\lambda_{P} = \left( \lambda_{X}^{\theta} + \lambda_{Y}^{\theta} \right)^{1/\theta}.$$

Therefore, if the target medians are $m_{P}$ for PFS and $m_{Y}$ for OS,

$$\lambda_{P} = \frac{\log(2)}{m_{P}},\qquad\lambda_{Y} = \frac{\log(2)}{m_{Y}}.$$

For a candidate value of $\theta$, the latent TTP rate is then

$$\lambda_{X} = \left( \lambda_{P}^{\theta} - \lambda_{Y}^{\theta} \right)^{1/\theta}.$$

This requires $m_{P} < m_{Y}$, as expected for PFS and OS.

## Choosing the Copula Parameter

For the Gumbel copula, Kendall’s tau between latent TTP and OS is

$$\tau(X,Y) = 1 - \frac{1}{\theta}.$$

However,
[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
asks for Kendall’s tau between the observed, uncensored endpoints
$P = \min(X,Y)$ and $Y$, not between latent TTP and OS. These are not
the same quantity. The formula below is derived in the appendix. Under
the model above,

$$\tau(P,Y) = 1 - \frac{1}{\theta}\left\lbrack 1 - \left( \frac{m_{P}}{m_{Y}} \right)^{\theta} \right\rbrack.$$

Equivalently, writing $\tau_{X} = \tau(X,Y)$ and
$\theta = 1/\left( 1 - \tau_{X} \right)$,

$$\tau(P,Y) = 1 - \left( 1 - \tau_{X} \right)\left\lbrack 1 - \left( \frac{m_{P}}{m_{Y}} \right)^{1/{(1 - \tau_{X})}} \right\rbrack.$$

[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
solves this equation numerically for the latent Kendall’s tau
$\tau_{X}$, converts it to $\theta$, and then simulates from the Gumbel
copula.

For any finite $\theta$, $\tau(P,Y)$ is larger than $\tau(X,Y)$:

$$\tau(P,Y) - \tau(X,Y) = \frac{1}{\theta}\left( \frac{m_{P}}{m_{Y}} \right)^{\theta} > 0.$$

Intuitively, even if latent TTP and OS have a weaker association,
observed PFS contains deaths by construction, which increases the
association between observed PFS and OS.

The requested value of $\tau(P,Y)$ has a lower bound. When $\theta = 1$,
TTP and OS are independent, but PFS still contains OS deaths through the
definition $P = \min(X,Y)$. In this limiting case,

$$\tau(P,Y) = \frac{m_{P}}{m_{Y}}.$$

Thus, for fixed medians $m_{P}$ and $m_{Y}$,
[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
can only target

$$\frac{m_{P}}{m_{Y}} \leq \tau(P,Y) < 1.$$

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

For verification only, we can call the generator directly. This is not
the recommended way to use `TrialSimulator` for simulation studies; in
practice, the generator should usually be supplied to
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
and used inside the trial simulation workflow. Direct calls are helpful
here because they let us check the marginal medians, the observed
Kendall’s tau, and the ordering $\text{PFS} \leq \text{OS}$ before
adding enrollment, censoring, treatment arms, and analyses.

``` r
set.seed(123)
dat <- CorrelatedPfsAndOs2(n = 100000,
                           median_pfs = 5,
                           median_os = 11,
                           kendall = 0.6)

head(dat, 2)
#>        pfs        os pfs_event os_event
#> 1 6.828217 14.728535         1        1
#> 2 2.153555  2.617195         1        1
```

The simulation should approximately recover the requested medians and
Kendall’s tau between observed, uncensored PFS and OS times.

``` r
with(dat, median(pfs))
#> [1] 4.989699
with(dat, median(os))
#> [1] 11.0149
with(dat, cor(pfs, os, method = 'kendall'))
#> [1] 0.598662
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

Let $X$ and $Y$ be nonnegative random variables with exponential margins

$$X \sim \operatorname{Exp}\left( \lambda_{X} \right),\qquad Y \sim \operatorname{Exp}\left( \lambda_{Y} \right),$$

and joint survival function

$$S_{X,Y}(x,y) = \Pr(X > x,Y > y) = \exp\left\lbrack - \left\{ \left( \lambda_{X}x \right)^{\theta} + \left( \lambda_{Y}y \right)^{\theta} \right\}^{1/\theta} \right\rbrack,\qquad\theta \geq 1.$$

Define $P = \min(X,Y)$. We want Kendall’s tau between $P$ and $Y$.

Standardize the margins by setting

$$A = \lambda_{X}X,\qquad B = \lambda_{Y}Y.$$

Then the joint survival function of $(A,B)$ is

$$\Pr(A > a,B > b) = \exp\left\{ - \left( a^{\theta} + b^{\theta} \right)^{1/\theta} \right\}.$$

Use the polar-type transformation

$$R = \left( A^{\theta} + B^{\theta} \right)^{1/\theta},\qquad W = \frac{A^{\theta}}{A^{\theta} + B^{\theta}}.$$

A Jacobian calculation gives the joint density

$$f_{R,W}(r,w) = \frac{1}{\theta}e^{- r}(r + \theta - 1),\qquad r > 0,\; 0 < w < 1.$$

Thus, $R$ and $W$ are independent and $W \sim \operatorname{Unif}(0,1)$.
The inverse transformation is

$$A = RW^{1/\theta},\qquad B = R(1 - W)^{1/\theta}.$$

Let

$$a = \lambda_{X}^{\theta},\qquad b = \lambda_{Y}^{\theta},\qquad c = \frac{a}{a + b},\qquad d = \frac{b}{a + b}.$$

The event $X \leq Y$ is equivalent to $W \leq c$:

$$\left. X \leq Y\Leftrightarrow\frac{A}{\lambda_{X}} \leq \frac{B}{\lambda_{Y}}\Leftrightarrow W \leq c, \right.$$

so

$$P = \begin{cases}
{X,} & {W \leq c,} \\
{Y,} & {W > c.}
\end{cases}$$

Using the survival-copula representation of Kendall’s tau,

$$\tau(P,Y) = 4E\{ S_{P,Y}(P,Y)\} - 1.$$

When $W \leq c$, $P = X$ and

$$S_{P,Y}(P,Y) = S_{X,Y}(X,Y) = e^{- R}.$$

When $W > c$, $P = Y$. Since

$$S_{P,Y}(y,y) = \Pr(P > y,Y > y) = \Pr(X > y,Y > y),$$

we have, using $Y = B/\lambda_{Y} = R(1 - W)^{1/\theta}/\lambda_{Y}$,

$$S_{P,Y}(Y,Y) = \exp\left\lbrack - R\left( \frac{1 - W}{d} \right)^{1/\theta} \right\rbrack.$$

Therefore,

$$E\{ S_{P,Y}(P,Y)\} = E\{ e^{- R}\mathbf{1}(W \leq c)\} + E\left\lbrack e^{- R{({(1 - W)}/d)}^{1/\theta}}\mathbf{1}(W > c) \right\rbrack.$$

By independence of $R$ and $W$,

$$E\{ S_{P,Y}(P,Y)\} = cL_{R}(1) + \int_{c}^{1}L_{R}\left\{ \left( \frac{1 - w}{d} \right)^{1/\theta} \right\} dw,$$

where $L_{R}(s) = E\left( e^{- sR} \right)$. From the density of $R$,

$$L_{R}(s) = \frac{1}{\theta}\int_{0}^{\infty}e^{- {(1 + s)}r}(r + \theta - 1)\, dr = \frac{\theta + (\theta - 1)s}{\theta(1 + s)^{2}}.$$

In particular,

$$L_{R}(1) = \frac{2\theta - 1}{4\theta}.$$

For the integral term, use the substitution

$$t = \left( \frac{1 - w}{d} \right)^{1/\theta},\qquad dw = - d\theta t^{\theta - 1}dt.$$

Then

$$\int_{c}^{1}L_{R}\left\{ \left( \frac{1 - w}{d} \right)^{1/\theta} \right\} dw = d\theta\int_{0}^{1}t^{\theta - 1}L_{R}(t)\, dt.$$

Substituting $L_{R}(t)$ gives

$$d\int_{0}^{1}\frac{\theta t^{\theta - 1} + (\theta - 1)t^{\theta}}{(1 + t)^{2}}dt.$$

Because

$$\frac{d}{dt}\left( \frac{t^{\theta}}{1 + t} \right) = \frac{\theta t^{\theta - 1} + (\theta - 1)t^{\theta}}{(1 + t)^{2}},$$

the integral is

$$d\left\lbrack \frac{t^{\theta}}{1 + t} \right\rbrack_{0}^{1} = \frac{d}{2}.$$

Thus,

$$E\{ S_{P,Y}(P,Y)\} = c\frac{2\theta - 1}{4\theta} + \frac{d}{2} = \frac{2\theta - 1 + d}{4\theta},$$

using $c + d = 1$. Finally,

$$\tau(P,Y) = 4E\{ S_{P,Y}(P,Y)\} - 1 = \frac{\theta - 1 + d}{\theta} = 1 - \frac{1}{\theta}\left( \frac{\lambda_{X}^{\theta}}{\lambda_{X}^{\theta} + \lambda_{Y}^{\theta}} \right).$$

Since $P$ has rate

$$\lambda_{P} = \left( \lambda_{X}^{\theta} + \lambda_{Y}^{\theta} \right)^{1/\theta},$$

this can also be written as

$$\tau(P,Y) = 1 - \frac{1}{\theta}\left( \frac{\lambda_{X}^{\theta}}{\lambda_{P}^{\theta}} \right) = 1 - \frac{1}{\theta}\left\lbrack 1 - \left( \frac{\lambda_{Y}}{\lambda_{P}} \right)^{\theta} \right\rbrack.$$

Because $\lambda_{Y}/\lambda_{P} = m_{P}/m_{Y}$, the formula used by
[`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
is

$$\tau(P,Y) = 1 - \frac{1}{\theta}\left\lbrack 1 - \left( \frac{m_{P}}{m_{Y}} \right)^{\theta} \right\rbrack.$$
