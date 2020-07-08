##################################################################
##               Analysis code for Geniole et al                ##
##################################################################


#### load packages ####
xfun::pkg_attach2(
  "sjmisc",
  "readxl",
  "robustbase",
  "tidyverse",
  "magrittr",
  "here",
  "ez",
  "remotes"
)



#### define helper functions ####

remotes::install_github("ljcolling/ODPhelper", upgrade = "never")
library(ODPHelper)

make_table <- function(x) {
  n <- length(x$fitted.values)
  df <- x$df.residual
  t_head <- glue::glue("$t$({df})")
  x %>%
    summary() %>%
    coef() %>%
    as_tibble(rownames = "param") %>%
    mutate(r = convert.t.to.r(t = `t value`, n = n), d = convert.r.to.d(r)) %>%
    mutate(d = d, r = abs(r), ci = cohensdCI(d, n = n, conf.level = .95)) %>%
    magrittr::set_colnames(c("Analysis and predictor of agression", "$b$", "$SE$", t_head, "$p$", "$r$", "$d$", "ci")) %>%
    mutate(ci = glue::glue("[{rd(ci[,'lo'])}, {rd(ci[,'hi'])}]")) %>%
    rename(`95\\% CI ($d$)` = ci) %>%
    mutate(
      `$p$` = rp2(`$p$`),
      `$r$` = rp2(`$r$`)
    )
}

#### define env to hold output #####
geniole <- new.env()


#### replicate the analysis ####
#### drug validation analysis ####

# ANOVA
drug_val_file <- here::here("data/drug_validation.xlsx")
drug_val <- readxl::read_excel(drug_val_file, range = "A1:M14")
generate_anova <- function(drug_val) {
  drug_val %>%
    select(1:11) %>%
    pivot_longer(cols = 2:11) %>%
    mutate(drug = stringr::str_sub(name, 1, 1)) %>%
    mutate(
      name = stringr::str_remove_all(name, "P"),
      name = stringr::str_remove_all(name, "T")
    ) %>%
    mutate_at(c("ID", "name", "drug"), list(~ as_factor(.))) -> drug_val_long

  drug_val_long %>% ez::ezANOVA(
    wid = ID,
    dv = value,
    within = .(name, drug),
    detailed = T
  ) -> drug_val_anova
  print_anova(drug_val_anova, "name:drug")
}
generate_anova(drug_val) -> geniole$drug_val_anova

generate_paired <- function(drug_val) {
  drug_val %>%
    select(1:11) %>%
    pivot_longer(cols = 2:11) %>%
    mutate(drug = stringr::str_sub(name, 1, 1)) %>%
    mutate(
      name = stringr::str_remove_all(name, "P"),
      name = stringr::str_remove_all(name, "T")
    ) %>%
    mutate_at(c("ID", "name", "drug"), list(~ as_factor(.))) -> drug_val_long

  # Paired t-tests
  drug_val_long %>%
    pivot_wider(id_cols = c(ID, name), names_from = "drug", values_from = "value") %>%
    group_by(name) %>%
    do(t.test(.$T, .$P, paired = T) %>% broom::tidy()) %>%
    mutate(p.report = if_else(p.value < 0.001, "< .001",
      sprintf(fmt = "%.3f", p.value) %>%
        stringr::str_replace("0.", "= .")
    )) %>%
    mutate(name = as.character(name)) %>%
    mutate(name = if_else(name == "baseline", name, paste0(name, " min"))) %>%
    tibble(report = glue::glue_data_col(
      .x = .,
      "{name}: *t*({parameter}) = ",
      "{rd(statistic,3)}, ",
      "*p* {p.report}"
    )) %>%
    pull(report) %>%
    paste0(collapse = "; ")
}
generate_paired(drug_val) -> geniole$drug_val_ttests
#### archival data set analysis

archival_file <- here::here("data/archival.xlsx")

archival <- readxl::read_excel(archival_file, range = "A1:G115")
generate_table2 <- function(archival) {
  # Generate table 2

  rbind(
    # TABLE 2 part 1
    lmrob(ag_prov ~ std(riskc) * center(drugcond), data = archival,control = lmrob.control(maxit.scale  = 400)) %>% make_table() %>% slice(2:4),
    # TABLE 2 part 2

    lmrob(ag_prov ~ I(std(riskc) + 1) * center(drugcond), data = archival) %>% make_table() %>% slice(3),
    
    # TABLE 2 part 3
    lmrob(ag_prov ~ I(std(riskc) - 1) * center(drugcond), data = archival)  %>% make_table() %>% slice(3)
    ) %>%
    mutate(`Analysis and predictor of agression` = c(
      "Personality-risk score",
      "Drug group",
      "Drug group $\\times$ Personality-risk score",
      "Drug group's conditional effect at\nlow personality-risk score",
      "Drug group's conditional effect at\nhigh personality-risk score"
    ))
}

generate_table2(archival)-> geniole$table2
##### current data set analysis #####
current_data_file <- here::here("data/current.xlsx")
cagr <- readxl::read_excel(current_data_file, range = "A1:Q309")

generate_table3 <- function(cagr) {
  # Generate table 3
  rbind(
    # TABLE 3 part 1
    lmrob(Agg_Prov ~ std(riskc) * center(DrugCondition), data = cagr) %>% make_table() %>%
      slice(2:4),

    # TABLE 3 part 2
    lmrob(Agg_Prov ~ I(std(riskc) + 1) * center(DrugCondition), data = cagr) %>% make_table() %>%
      slice(3),

    # TABLE 2 part 3
    lmrob(Agg_Prov ~ I(std(riskc) - 1) * center(DrugCondition), data = cagr) %>% make_table() %>%
      slice(3) 
  ) %>%
    mutate(`Analysis and predictor of agression` = c(
      "Personality-risk score",
      "Drug group",
      "Drug group $\\times$ Personality-risk score",
      "Drug group's conditional effect at\nlow personality-risk score",
      "Drug group's conditional effect at\nhigh personality-risk score"
    ))
}
generate_table3(cagr) -> geniole$table3

generate_table4 <- function(cagr) {
  rbind(
    # Analysis A
    lmrob(Agg_Prov ~ std(CAG_Repeat) * std(riskc) * center(DrugCondition), data = cagr) %>% make_table() %>% slice(2:8),
    # Analysis B
    lmrob(Agg_Prov ~ I(std(CAG_Repeat) + 1) * std(riskc) * center(DrugCondition), data = cagr) %>% make_table() %>%  slice(7),

    lmrob(Agg_Prov ~ I(std(CAG_Repeat) - 1) * std(riskc) * center(DrugCondition), data = cagr) %>% make_table() %>%  slice(7),
    # Analysis C
    lmrob(Agg_Prov ~ I(std(CAG_Repeat) + 1) * I(std(riskc) + 1) * center(DrugCondition), data = cagr) %>% make_table() %>%  slice(4),

    lmrob(Agg_Prov ~ I(std(CAG_Repeat) + 1) * I(std(riskc) - 1) * center(DrugCondition), data = cagr) %>% make_table() %>%  slice(4),

    lmrob(Agg_Prov ~ I(std(CAG_Repeat) - 1) * I(std(riskc) + 1) * center(DrugCondition), data = cagr) %>% make_table() %>%  slice(4),

    lmrob(Agg_Prov ~ I(std(CAG_Repeat) - 1) * I(std(riskc) - 1) * center(DrugCondition), data = cagr) %>% make_table() %>%  slice(4)
  ) %>%   mutate(`Analysis and predictor of agression` = 
  c(
    "CAG repeat length",
    "Personality-risk score",
    "Drug group",
    "Personality-Risk Score $\\times$ CAG Repeat Length",
    "Drug Group $\\times$ CAG Repeat Length",
    "Drug Group $\\times$ Personality-Risk Score",
    "Drug Group $\\times$ Personality-Risk Score $\\times$ CAG\nRepeat Length",

    "Drug Group $\\times$ Personality-Risk Score at Low CAG\nRepeat Length",
    "Drug Group $\\times$ Personality-Risk Score at High CAG\nRepeat Length",

    "Drug group's conditional effect at low CAG repeat\nlength, low personality-risk score",
    "Drug group's conditional effect at low CAG repeat\nlength, high personality-risk score",

    "Drug group's conditional effect at high CAG repeat\nlength, low personality-risk score",
    "Drug group's conditional effect at high CAG repeat\nlength, high personality-risk score"))
}
generate_table4(cagr) -> geniole$table4


# Generate in-text stats
generate_intext <- function(cagr) {
  text_report <- function(x, this.row = "center(DrugCondition)") {
    n <- x$x %>%
      dim() %>%
      .[1]
    b <- summary(x) %>%
      coef() %>%
      as_tibble(rownames = "keep") %>%
      filter(keep == this.row) %>%
      pull(Estimate)
    t <- summary(x) %>%
      coef() %>%
      as_tibble(rownames = "keep") %>%
      filter(keep == this.row) %>%
      pull(`t value`)
    se <- summary(x) %>%
      coef() %>%
      as_tibble(rownames = "keep") %>%
      filter(keep == this.row) %>%
      pull(`Std. Error`)
    df <- x$df.residual
    p.value <- summary(x) %>%
      coef() %>%
      as_tibble(rownames = "keep") %>%
      filter(keep == this.row) %>%
      pull(`Pr(>|t|)`)
    d <- convert.t.to.d(t, df)
    r <- convert.t.to.r(t, n)
    d.ci <- cohensdCI(d, n)[, c("lo", "hi")] %>% as.vector()

    give.p <- function(x) if_else(x < 0.001, "< .001", sprintf("%.3f", x) %>% stringr::str_replace("= .", pattern = "0."))
    give.r <- function(x) sprintf("%.3f", x) %>% stringr::str_replace(".", pattern = "0.")

    glue::glue("*n* = {n}, *b* = {rd(b)}, *SE* = {rd(se)}, *t*({df}) = {rd(t)}, *p* {give.p(p.value)}, *r* = {give.r(r)}, Cohen's *d* = {rd(d)}, 95% CI = [{rd(d.ci[1])},{rd(d.ci[2])}]")
  }
  list(
    lmrob(FeelGood ~ I(std(CAG_Repeat) + 1) * I(std(riskc) - 1) * center(DrugCondition), data = subset(cagr, FeelGood > 0)) %>% text_report(),

    lmrob(Angry ~ I(std(CAG_Repeat) + 1) * I(std(riskc) - 1) * center(DrugCondition), data = subset(cagr, Angry > 0)) %>% text_report(),

    lmrob(Agg_Prov ~ I(std(CAG_Repeat) + 1) * I(std(riskc) - 1) * center(DrugCondition), data = subset(cagr, FeelGood > 0)) %>% text_report(),

    lmrob(Agg_Prov ~ I(std(CAG_Repeat) + 1) * I(std(riskc) - 1) * center(DrugCondition) + I(std(CAG_Repeat) + 1) * I(std(riskc) - 1) * std(FeelGood), data = subset(cagr, FeelGood > 0)) %>% text_report(),

    lmrob(Agg_Prov ~ I(std(CAG_Repeat) + 1) * I(std(riskc) - 1) * center(DrugCondition) + I(std(CAG_Repeat) + 1) * I(std(riskc) - 1) * std(FeelGood), data = subset(cagr, FeelGood > 0)) %>% text_report(this.row = "std(FeelGood)")
  )
}

generate_intext(cagr) -> geniole$intext_stats

generate_table5 <- function(cagr) {
 rbind(
    lmrob(Agg_Prov ~ I(std(CAG_Repeat)) * center(DrugCondition), data = cagr) %>% make_table() %>% slice(2:4),
    lmrob(Agg_Prov ~ I(std(CAG_Repeat) + 1) * center(DrugCondition), data = cagr) %>% make_table() %>% slice(3),
    lmrob(Agg_Prov ~ I(std(CAG_Repeat) - 1) * center(DrugCondition), data = cagr) %>% make_table() %>% slice(3)
  ) %>% mutate(`Analysis and predictor of agression` = c(
    "CAG repeat length",
    "Drug group",
    "Drug group $\\times$ CAG Repeat Length",
    "Drug group's conditional effect at low CAG repeat length",
    "Drug group's conditional effect at high CAG repeat length"
  ))

}
generate_table5(cagr) -> geniole$table5




# Write out session and package information
geniole$session_info <- desc_session()
saveRDS(geniole, here::here("./made/geniole.Rdata"))
