estimate_oadr_future <- function(sim, pension_age) {
  bind_rows(
    oadr_sim(sim, filter(pension_age, Policy == "Age65")) %>%
      mutate(Policy = "Pension age 65"),
    oadr_sim(sim, filter(pension_age, Policy == "Current")) %>%
      mutate(Policy = "Current policy"),
    oadr_sim(sim, filter(pension_age, Policy == "Proposed")) %>%
      mutate(Policy = "Proposed policy")
  ) %>%
  mutate(Policy = factor(Policy,
                         levels = c("Pension age 65", "Current policy", "Proposed policy"))
  )
}
