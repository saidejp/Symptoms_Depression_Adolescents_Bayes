



# packages ----------------------------------------------------------------



# installing and loading packages
pacman::p_load(tidyverse, brms, bayesplot, ggdist)


# data --------------------------------------------------------------------


# data and dictionaries were downloaded from: https://ensanut.insp.mx/busqueda.php

# clinical data
datos <- read_delim("ensadol2022_entrega_w.csv", delim = ";")

# demographic data
dat_demo <- read_delim("integrantes_ensanut2022_w.csv", delim = ";")



# variables ---------------------------------------------------------------

# variable names with d09 were variables of discipline or violence
# and variables with d06 were CESD-7 items
# NA and 9 were codes for people without response

dat_malt <- 
  datos %>%
  select(FOLIO_I, FOLIO_INT, edad, sexo, d0601a:d0601g, d0901a:d0901k) %>% 
  mutate(
    across(
      .cols = d0901a:d0901k, 
      .fns = function(x) {
        x <- as.numeric(x)
        x <- replace_na(x, 9)
      }
    )
  ) %>% 
  filter(
    if_all(
      .cols = d0901a:d0901k, 
      .fns = ~ .x != 9
    )
  )


dat_malt <- 
  dat_malt %>% 
  mutate(
    across(
      .cols = starts_with("d09"),
      .fns = ~ factor(.x, levels = c(2, 1), labels = c("No", "Sí"))
    )
  )


# Confounders -------------------------------------------------------------

# smoke = d0101
# diabetes = d02o1a
# high blood pressure = d02o1b
# alcohol = d0109
# region = region
# school = escuela
# work = trabajo

datos_conf <- 
  datos %>% 
  select(FOLIO_I, 
         FOLIO_INT,
         edad,
         sexo,
         fumar = d0101,
         diabetes = d02o1a, 
         pres_alta = d02o1b, 
         forz_sex = d0309, 
         alcohol = d0109, 
         region) %>% 
  mutate(
    across(
      .cols = fumar:region, 
      .fns = as.numeric
    )
  )


datos_conf <- 
  datos_conf %>% 
  mutate(
    fumar = case_when(
      fumar %in% c(1, 2) ~ "Sí", 
      fumar == 3 ~ "No", 
      fumar == 9 ~ "NR"
    ), 
    diabetes = case_when(
      diabetes == 1 ~ "Sí",
      diabetes == 2 ~ "No", 
      diabetes == 9 ~ "NR"
    ),
    pres_alta = case_when(
      pres_alta == 1 ~ "Sí",
      pres_alta == 2 ~ "No", 
      pres_alta == 9 ~ "NR"
    ), 
    forz_sex = case_when(
      forz_sex == 1 ~ "Sí", 
      forz_sex == 2 ~ "No", 
      forz_sex == 9 ~ "NR",
    ),
    alcohol = ifelse(is.na(alcohol), "NR", "Sí"))

# joining with clinical variables
datos_conf <-
  datos_conf %>% 
  inner_join(dat_malt) %>% 
  select(-forz_sex)


# selecting demographic variables
vars_demo <-
  dat_demo %>% 
  select(
    FOLIO_I, 
    FOLIO_INT,
    edad = h0303,
    sexo = h0302,
    escuela = h0313,
    trabajo = h0321
    #programa_jovenes = h0601j # ninguno tuvo acceso al programa
  )

# joining demographic and clinical variables in one dataframe
datos_conf <-
  datos_conf %>% 
  inner_join(vars_demo) 

# if adolescents go to school 1 = yes, 2 = no
datos_conf <- 
  datos_conf %>% 
  relocate(c(escuela, trabajo), .after = sexo) %>% 
  mutate(
    across(
      .cols = c(escuela, trabajo), 
      .fns = function(x) {
        
        x <- as.numeric(x)
        
        case_when(
          x == 1 ~ "Sí",
          x == 2 ~ "No", 
          is.na(x) ~ "NR"
        )
        
      }
    )
  ) %>% 
  mutate(
    sexo = factor(sexo, levels = c(1, 2), labels = c("H", "M")), 
    escuela = factor(escuela, levels = c("Sí", "No")),
    region = as.factor(region)
  ) %>% 
  mutate(
    across(
      .cols = c(trabajo, fumar, diabetes, pres_alta, alcohol), 
      .fns = ~ factor(.x, levels = c("No", "NR", "Sí"))
    )
  )

# long format data for each depressive symptom
datos_conf_long <- 
  datos_conf %>% 
  pivot_longer(
    cols = starts_with("d06"), 
    names_to = "item",
    values_to = "response"
  )

datos_conf_long


# modeling ----------------------------------------------------------------


# ordinary least squares models
datos_conf_long %>% 
  split(.$item) %>% 
  map_df(
    .f = ~ lm(response ~ d0901g + scale(edad) + sexo + escuela + trabajo + 
                fumar + diabetes + pres_alta + alcohol + region, 
              data = .x) %>% broom::tidy(), 
    .id = "item"
  ) %>% 
  filter(term == "d0901gSí")






# ordinal bayesian models -------------------------------------------------


bayesian_models_UN <- 
  datos_conf_long %>% 
  split(.$item) %>% 
  map(
    .f = function(df) {
      df <- 
        df %>% 
        mutate(response = factor(response, ordered = TRUE), 
               edad = edad - mean(edad))
      
      fit_naive <- 
        brm(
          response ~ d0901g + edad + sexo + escuela + trabajo + 
            fumar + diabetes + pres_alta + alcohol + region,
          data = df, 
          family = cumulative(), 
          sample_prior = "yes",
          prior = prior(normal(0, .5), class = b),
          chains = 2, 
          cores = 12
        ) 
      
      
      
    }
  )


# checking models -------------------------------------------------

# trace plots for convergence
map(
  .x = bayesian_models_UN,
  .f = plot
)

# posteriors predictive checks 
map(
  .x = bayesian_models_UN,
  .f = ~ pp_check(.x, type = "bars", ndraws = 100)
)

# main effect for all models 
map(
  .x = bayesian_models_UN,
  .f = ~ conditional_effects(.x, effects = "d0901g", categorical = TRUE)
)




# results -----------------------------------------------------------------

# D0601A Durante la última semana ¿Sentías como si no pudieras quitarte de encima la tristeza?
# shaking off the sadness
bayesian_models_UN$d0601a


# D0601B Durante la última semana ¿Te costaba concentrarte en lo que estabas haciendo?
# concentrating problems
bayesian_models_UN$d0601b

# D0601C Durante la última semana ¿Te sentiste deprimido/a?
# feeling depressed
bayesian_models_UN$d0601c


# D0601D Durante la última semana ¿Te parecía que todo lo que hacías era un esfuerzo?
# everything is an effort
bayesian_models_UN$d0601d


# D0601E Durante la última semana ¿No dormiste bien?
# insomnia
bayesian_models_UN$d0601e

# D0601F Durante la última semana ¿Disfrutaste de la vida?
# enjoying life
bayesian_models_UN$d0601f

# D0601G Durante la última semana ¿Te sentiste triste?
# feeling sad
bayesian_models_UN$d0601g




# figures -----------------------------------------------------------------


# figure 1

rosas <-  bayesplot::color_scheme_get("pink", i = c(1, 3, 4, 6)) %>% unlist()
names(rosas) <- NULL

fig_0 <- 
  dat_malt %>% 
  pivot_longer(
    cols = d0601a:d0601g, 
    names_to = "item", 
    values_to = "resp"
  ) %>% 
  mutate(resp = as.factor(resp),
         d0901g = factor(d0901g, 
                         levels = c("No", "Sí"),
                         labels = c("No", "Yes")),
         item = factor(item, 
                       levels = paste0("d0601", letters[1:7]),
                       labels = c("sadness",
                                  "concentration",
                                  "depression",
                                  "effort",
                                  "sleep",
                                  "enjoyment",
                                  "feeling sad")
         )
  ) %>% 
  ggplot(aes(x = item, fill = resp, group = resp)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = rosas, 
                    breaks = c("1", "2", "3", "4"), 
                    labels = c("Never", "Sometimes", "Frequently", "Always")) +
  labs(
    fill = NULL, 
    group = NULL,
    x = NULL,
    y = "Proportion",
    title = "In the last month were you beaten?")  +
  ggdist::theme_ggdist() +
  coord_flip() +
  theme(axis.title.x = element_text(hjust = .5),
        title = element_text(hjust = .5, face = "bold"),
        strip.text.y = element_text(face = "bold"),
        legend.position = "top")  +
  facet_grid(d0901g ~ .) 


fig_0

# ggsave("Fig0.png", fig_0, device = "png", units = "cm", dpi = 350, 
#       width = 15, height = 13)

# figure 2 and 3
theme_set(theme_ggdist())


plots_posteriors <- 
  pmap(
    .l = list(fit = bayesian_models_UN, 
              tag = c("A", "B", "C", "D", "E", "F", "G"), 
              item = c("Did you couldn't shake off the sadness?",
                       "Did you have trouble concentrating?",
                       "Did you feel depressed?",
                       "Did everything you do feel like an effort?",
                       "Did you not sleep well?", 
                       "Did you enjoy life?",
                       "Did you feel sad?")), 
    .f = function(fit, tag, item) {
      
      fit %>% 
        as_tibble() %>% 
        select(
          `Parental violence` = b_d0901gSí, 
          Age = b_edad,
          Woman = b_sexoM, 
          Smoke = b_fumarSí,
          Alcohol = b_alcoholSí
        ) %>% 
        pivot_longer(
          cols = everything(), 
          names_to = "param",
          values_to = "z"
        ) %>% 
        mutate(param = factor(param, levels = c("Parental violence",
                                                "Age", 
                                                "Woman", 
                                                "Smoke", 
                                                "Alcohol"))) %>% 
        ggplot(aes(x = param, y = z,  fill = after_stat(pdf))) + 
        geom_hline(yintercept = 0, col = "grey") +
        stat_slabinterval(aes(fill = after_stat(level)), .width = c(.66, .95, 1)) +
        scale_fill_manual(values = c("#dcbccc",  "#b97c9b", "#8f275b")) +
        scale_y_continuous(limits = c(-2, 2)) +
        labs(
          x = NULL,
          y = "Score",
          fill = "Posterior Interval",
          tag = tag,
          title = item
        ) +
        coord_flip() +
        theme(legend.position = "bottom", 
              plot.tag = element_text(face = "bold"),
              plot.title = element_text(hjust = .5))
      
      
      
    }
  )

plots_posteriors 



# model for the sum of sypmtoms -----------------------------------------------

# it calculates the sum for each adolescent
dat_sum <- 
  datos_conf_long %>% 
  group_by(FOLIO_I, FOLIO_INT) %>%
  summarize(sum_response = sum(response), 
            .groups = "drop") 

# joining with data
datos_conf <- 
  datos_conf %>% 
  inner_join(dat_sum)

# model fit
fit_total <- 
  brm(
    sum_response ~ d0901g + scale(edad) + sexo + escuela + trabajo + 
      fumar + diabetes + pres_alta + alcohol + region,
    data = datos_conf, 
    family = gaussian(),
    sample_prior = "yes",
    prior = prior(normal(0, .5), class = b),
    chains = 2, 
    cores = 12
  )  

fit_total

# model checking
plot(fit_total)
pp_check(fit_total, type = "hist", ndraws = 2)
conditional_effects(fit_total, effects = "d0901g")

# prior updating
prior_b <- prior_draws(fit_total) %>% as_tibble()


fit_total %>% 
  as_tibble() %>% 
  select(2) %>% 
  bind_cols(prior_b) %>% 
  pivot_longer(
    cols = starts_with("b"), 
    names_to = "parameter",
    values_to = "value"
  ) %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density()



# figure 3
fig3 <- 
  fit_total %>% 
  as_tibble() %>% 
  select(
    No = b_Intercept, 
    Yes =  b_d0901gSí
  ) %>% 
  mutate(Yes = No + Yes) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "cond",
    values_to = "Score"
  ) %>% 
  ggplot(aes(x = Score, fill = cond, color = cond)) +
  stat_slab(alpha = .7) +
  stat_pointinterval(position = position_dodge(width = .3, preserve = "single")) +
  scale_y_continuous(breaks = NULL) +
  #geom_histogram(col = "white", 
  #               bins = 20,
  #               position = position_dodge(.1)) +
  scale_fill_manual(values = c("#dcbccc", "#8f275b")) +
  scale_color_manual(values = c("#dcbccc", "#8f275b")) +
  labs(y = NULL, 
       x = "Depression: total score of CESD-7",
       fill = "Parental violence", 
       tag = "H",
       color = NULL) +
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold")) +
  guides(color = "none")


#ggsave("Fig3.png", plot = fig3, device = "png", dpi = 350, 
#       units = "cm", height = 11, width = 14)


fig1 <- 
  gridExtra::grid.arrange(plots_posteriors$d0601a, 
                          plots_posteriors$d0601b,
                          plots_posteriors$d0601c,
                          plots_posteriors$d0601d, 
                          ncol = 2)

fig2 <-
  gridExtra::grid.arrange(plots_posteriors$d0601e, 
                          plots_posteriors$d0601f,
                          plots_posteriors$d0601g,
                          fig3,
                          ncol = 2)



#map2(
#  .x = list(fig1, fig2),
#  .y = c("Fig1.png", "Fig2.png"), 
#  .f = ~ ggsave(
#    filename = .y, 
#    plot = .x,
#    device = "png", 
#    dpi = 350, 
#    units = "cm", 
#    width = 25,
#    height = 18
#  )
#)
