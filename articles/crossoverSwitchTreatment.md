# Simulate Trials with Treatment Switching

The `TrialSimulator` package can handle three types of crossover design.
This vignette demonstrates how to simulate a trial where all or a subset
of patients are offered the option of treatment switching over time. For
example, patients can switch to a higher dose if disease progresses. To
provide such a flexibility in simulation, `TrialSimulator` support a
`regime` object in which users can define the subset of patients who
switch, the switching time, and the updated endpoints under new
treatments.
