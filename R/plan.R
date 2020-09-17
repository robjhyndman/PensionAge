the_plan <-
  drake_plan(

  ## Pension age schemes
  pension_age = tibble(Year = 1920:2050, Age65 = 65) %>%
    mutate(
      Proposed = pmin(70, Age65 + 0.5 * pmax(0, trunc((Year - 2015) / 2))),
      Current = pmin(Proposed, 67),
    ) %>%
    pivot_longer(-Year, values_to = "Age", names_to = "Policy"),

  # Get population data to 1 Jan 2019
  aus.pop = hmd.pop(country = "AUS",
                    username = username,
                    password = password,
                    label = "Australia") %>%
      extract.ages(ages = 0:100),

  # Get mortality data to 2018
  aus.mort = hmd.mx("AUS", username, password, "Australia") %>%
      extract.ages(ages = 0:100, combine.upper = FALSE) %>%
      extract.years(years = 1950:2100),

  # Get fertility data to 2018
  abs_fertility_file = "./data/FERTILITY_AGE_STATE_30042020143630412.csv",
  aus.fert = get_aus_fert(abs_fertility_file),

  # Compute migration data
  aus.mig = netmigration(mort = aus.mort, fert = aus.fert,
    startyearpop = aus.pop, mfratio = 1.05),

  # Population data frame
  auspop_df = aus_pop_df(aus.pop),

  # SMOOTHING
  ausmort.sm = smooth.demogdata(aus.mort, obs.var = "theoretical"),
  ausfert.sm = smooth.demogdata(aus.fert, obs.var = "theoretical"),
  ausmig.sm = smooth.demogdata(aus.mig),

  # MORTALITY model
  mort.fit = coherentfdm(ausmort.sm),
  mortf = forecast(mort.fit, h = 33),

  # FERTILITY model
  fert.fit = fdm(ausfert.sm),
  fertf = forecast(fert.fit, h = 33),

  # MIGRATION
  mig.fit = coherentfdm(ausmig.sm),
  migf = forecast(mig.fit, h = 33),

  # SIMULATED populations
  aus.sim = pop.sim(mort = mortf, fert = fertf, mig = migf, firstyearpop = aus.pop, N = 2000),
  # Convert to tibble
  aussim_df = make_aussim_df(aus.sim, 2050),

  # Combine simulations to create means and quantiles of future years
  future_pop = aussim_df %>%
    group_by(Sex, Age, Year) %>%
    summarise(
      Mean = mean(Population),
      Lower = quantile(Population, prob = 0.1),
      Upper = quantile(Population, prob = 0.9),
      .groups = "keep"
    ) %>%
    ungroup() %>%
    filter(Year <= 2050),

  # Historical data as a tibble
  history_pop = bind_rows(
    as_tibble(aus.pop$pop$male / 1000) %>%
      mutate(
        Age = aus.pop$age,
        Sex = "Male"
      ) %>%
      pivot_longer(c(-Age, -Sex), names_to = "Year", values_to = "Population"),
    as_tibble(aus.pop$pop$female / 1000) %>%
      mutate(
        Age = aus.pop$age,
        Sex = "Female"
      ) %>%
      pivot_longer(c(-Age, -Sex), names_to = "Year", values_to = "Population"),
  ) %>%
    mutate(Year = as.numeric(Year)),

  # Future population simulation
  future_tot_pop = aussim_df %>%
    group_by(Rep, Sex, Year) %>%
    summarise(Population = sum(Population) / 1e3, .groups="keep") %>%
    group_by(Sex, Year) %>%
    summarise(
      Mean = mean(Population),
      Median = median(Population),
      Lo95 = quantile(Population, prob = 0.025),
      Up95 = quantile(Population, prob = 0.975),
      Lo80 = quantile(Population, prob = 0.1),
      Up80 = quantile(Population, prob = 0.9),
      .groups = "keep"
    ) %>%
    ungroup(),

  # OADR values
  OADR_history = oadr(history_pop, filter(pension_age, Policy == "Current")),
  OADR_future = estimate_oadr_future(aussim_df, pension_age),

  # Target OADR
  target = 0.23,

  ### find the pension.age scheme with OADR closest to target
  ### find upper boundary of pension ages whose OADR PI contains target
  ### find Lower boundary of pension ages whose OADR PI contains target
  ### Constrained such that solution is in minimum jumps of 1 month
  pension_age_approved = pension_age %>%
    filter(Policy == "Current", Year >= 2019) %>%
    select(-Policy),
  pension_age_lower = pension_scheme(aussim_df, pension_age_approved, target, "Lo"),
  pension_age_optimal = pension_scheme(aussim_df, pension_age_lower, target, "OADR"),
  pension_age_upper = pension_scheme(aussim_df, pension_age_optimal, target, "Hi"),

  # Final results
  OADR_optimal = oadr_sim(aussim_df, pension_age_optimal),
  OADR_lower = oadr_sim(aussim_df, pension_age_lower),
  OADR_upper = oadr_sim(aussim_df, pension_age_upper),

  # Forecast accuracy via tscv
  rollingsim = rolling_sim(ausmort.sm, ausfert.sm, ausmig.sm, aus.pop, t=2018-1950+1, H=25, K=25),
  oadr_rolling_sim = calc_oadr_rolling_sim(rollingsim, filter(pension_age, Policy == "Current"))
)
