# Autoplot for fmforecast objects
autoplot.fmforecast <- function(data) {
  require(latex2exp)
  p1 <- ggplot(mapping=aes(x=data$age, y=data$model$basis[,1])) +
    geom_line() + xlab("Age (x)") + ylab(TeX("$\\mu(x)"))
  p2 <- ggplot(mapping=aes(x=data$age, y=data$model$basis[,2])) +
    geom_line() + xlab("Age (x)") + ylab(TeX("$\\phi_1(x)$"))
  p3 <- ggplot(mapping=aes(x=data$age, y=data$model$basis[,3])) +
    geom_line() + xlab("Age (x)") + ylab(TeX("$\\phi_2(x)$"))
  p4 <- autoplot(data$coeff[[2]]) +
    ggtitle("") + xlab("Year (t)") + ylab(TeX("$\\beta_{1,t}$"))
  p5 <- autoplot(data$coeff[[3]]) +
    ggtitle("") + xlab("Year (t)") + ylab(TeX("$\\beta_{2,t}$"))

  print((p1 | p2 | p3) / ( plot_spacer() | p4 | p5))
}

# Function to produce old age dependency ratio using tibbles
# df is data frame with columns Age, Sex, Year, Population
# pension_age is dataframe with columns Year, Age.
oadr <- function(df, pension) {
  pension <- pension %>%
    select(Year, Age) %>%
    rename(Pension = Age)
  df %>%
    left_join(pension, by="Year") %>%
    mutate(
      # Fill historical years with minimum pension age
      Pension = replace_na(Pension, min(Pension, na.rm=TRUE)),
      # Divide population into three groups
      Group = case_when(
        (Age >= 15 & Age < Pension) ~ "Workers",
        (Age >= Pension) ~ "Pension",
        TRUE ~ "Child"
      )
    ) %>%
    group_by(Year, Group) %>%
    summarise(Population = sum(Population)) %>%
    ungroup() %>%
    pivot_wider(Year, names_from=Group, values_from=Population) %>%
    mutate(OADR = Pension / Workers) %>%
    select(Year, OADR)
}

# As above but df now contains Reps.
oadr_sim <- function(df, pension, level=80) {
  # Simulated OADR
  df %>%
    group_by(Rep) %>%
    group_modify(~ oadr(.x, pension=pension)) %>%
    group_by(Year) %>%
    summarise(
      Lo = quantile(OADR, prob=(0.5-level/200)),
      Hi = quantile(OADR, prob=1-(0.5-level/200)),
      OADR = mean(OADR),
    ) %>%
    ungroup()
}

# As above but df now contains Reps and id
oadr_rolling_sim <- function(df, pension, level=80) {
  # Simulated OADR
  df %>%
    group_by(Rep,id) %>%
    group_modify(~ oadr(.x, pension=pension)) %>%
    group_by(Year,id) %>%
    summarise(
      Lo = quantile(OADR, prob=(0.5-level/200)),
      Hi = quantile(OADR, prob=1-(0.5-level/200)),
      OADR = mean(OADR),
    ) %>%
    ungroup()
}
