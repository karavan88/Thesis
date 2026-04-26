#-------------------------------------------------------------------
# Project: PhD Thesis
# Script:  RLMS 2007-2023 Descriptive Data Analysis
# Author:  Garen Avanesian
# Date:    27 April 2025
#-------------------------------------------------------------------

# Variables for non-cognitive skills
ncs_vars <- 
  c("j445_3", "j445_11", "j445_14",
    "j445_2", "j445_12", "j445_17",
    "j445_1", "j445_4",  "j445_20",
    "j445_9", "j445_16", "j445_19",
    "j445_5", "j445_10", "j445_18")

ncs_opt <- 
  c("j445_6", "j445_8", "j445_13", "j445_21", # 6, 8, 21 conscientiousness
    "j445_23", # agreeableness
    "j445_7", "j445_15", "j445_22", "j445_24" # emotional stability
    )

openness <- c("o1", "o2", "o3") 
con <- c("c1", "c2", "c3")
ex <- c("e1", "e2", "e3")
ag <- c("a1", "a2", "a3")
em_st <- c("es1", "es2", "es3") 

descr_ind_2016_2019_empl <- 
  readRDS(file.path(processedData, "rlms_ind_sel_2001_2023.rds")) %>%
  filter(id_w %in% c("25", "28")) %>%
  mutate(across(all_of(c(ncs_vars, ncs_opt)), ~ ifelse(. >= 88888888, NA, .))) %>%
  # produce NCS measures
  mutate(o1 = 5 - j445_3,
         o2 = 5 - j445_11,
         o3 = 5 - j445_14,
         c1 = 5 - j445_2,
         c2 = j445_12,
         c3 = 5 - j445_17,
         g1 = 5 - j445_6,
         g2 = 5 - j445_8,
         g3 = 5 - j445_13,
         e1 = 5 - j445_1,
         e2 = j445_4, 
         e3 = 5 - j445_20,
         a1 = 5 - j445_9,
         a2 = 5 - j445_16,
         a3 = 5 - j445_19,
         a4 = 5 - j445_23,
         es1 = 5 - j445_5,
         es2 = j445_10,
         es3 = j445_18,
         h1 = 5 - j445_7,
         h2 = 5 - j445_22, 
         d1 = 5 - j445_15,
         d2 = 5 - j445_21,
         d3 = 5 - j445_23,
         d4 = 5 - j445_24) %>%
  mutate(O = scale(rowMeans(select(., all_of(openness)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         C = scale(rowMeans(select(., all_of(con)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         E = scale(rowMeans(select(., all_of(ex)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         A = scale(rowMeans(select(., all_of(ag)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         ES = scale(rowMeans(select(., all_of(em_st)), na.rm = T),center = TRUE, scale = TRUE)[,1]) %>%
  # select(-all_of(ncs_vars), -all_of(openness), -all_of(con), -all_of(ex), -all_of(ag), -all_of(em_st)) %>%
  group_by(idind) %>%
  arrange(id_w) %>%
  fill(O, C, E, A, ES, .direction = "downup") %>%
  ungroup() %>%
  drop_na(O, C, E, A, ES) %>%
  # create age groups to differentiate in the analysis
  mutate(age_10_29 = ifelse(age >= 10 & age < 30, "Age: Children and Youth (10-29)", as.character(NA)),
         age_15_24 = ifelse(age >= 15 & age < 25, "Age: Youth (15-24)",              as.character(NA)),
         age_15_29 = ifelse(age >= 15 & age < 30, "Age: Youth (15-29)",              as.character(NA)),
         age_16_65 = ifelse(age >= 16 & age < 66, "Age: Working Population (16-65)", as.character(NA)))


# summary(descr_ind_2016_2019_empl$age)
# summary(descr_ind_2016_2019_empl$age_group)


likert_data <-
  descr_ind_2016_2019_empl %>%
  # filter(id_w == "28") %>%
  select(all_of(ncs_vars)) %>%
  mutate(across(everything(), ~ case_when(. == 1 ~ "1. Почти всегда", 
                                           . == 2 ~ "2. Чаще всего",
                                           . == 3 ~ "3. Иногда",
                                           . == 4 ~ "4. Почти никогда",
                                           TRUE ~ as.character(.))))

# Add a font that supports Cyrillic (e.g., Times New Roman)
font_add(family = "Times New Roman", regular = "/System/Library/Fonts/Supplemental/Times New Roman.ttf") 

showtext_auto()

likert_plot_ncs <-
  gglikert(likert_data,
         variable_labels = c(
           # Openness
           j445_3  = "O1: Вам приходят в голову идеи, до которых другие не додумались раньше?",
           j445_11 = "O2: Вам очень интересно узнавать что-то новое?",
           j445_14 = "O3: Вы получаете удовольствие от красивого, например, природы, искусства и музыки?",
           # Conscientiousness
           j445_2  = "C1: Выполняя какое-то задание, Вы очень аккуратны?",
           j445_12 = "C2: Вам больше нравится расслабляться, чем усердно трудиться?",
           j445_17 = "C3: Вы работаете очень хорошо и быстро?",
           # Extraversion
           j445_1  = "E1: Вы разговорчивы?",
           j445_4  = "E2: Вы предпочитаете держать свое мнение при себе?",
           j445_20 = "E3: Вы открыты и общительны, например, Вы очень легко заводите друзей?",
           # Agreeableness
           j445_9  = "A1: Вы легко прощаете других людей?",
           j445_16 = "A2: Вы очень вежливы с другими людьми?",
           j445_19 = "A3: Вы щедро делитесь с другими людьми своим временем и деньгами?",
           # Emotional Stability
           j445_5  = "ES1: Вы спокойны в стрессовых ситуациях?",
           j445_10 = "ES2: Вы склонны к беспокойству?",
           j445_18 = "ES3: Вас легко заставить нервничать?"
         )) +
  theme(text = element_text(family = "Times New Roman"))


chronbach_data <-
  descr_ind_2016_2019_empl %>%
  filter(id_w == "28") %>%
  # filter(age >= 15 & age < 66) %>%
  select(o1, o2, o3, #all_of(ncs_vars), 
         c1, c2, c3, 
         g1, g2, g3, 
         e1, e2, e3,
         a1, a2, a3, 
         es1, es2, es3,
         h1, h2,
         d1, d2, d3, d4)

summary(factor(chronbach_data$g1))

# Openness
attr(chronbach_data$o1, "label") <- "Вам приходят в голову идеи,
до которых другие не додумались раньше?"
attr(chronbach_data$o2, "label") <- "Вам очень интересно узнавать что-то новое?"
attr(chronbach_data$o3, "label") <- "Вы получаете удовольствие от красивого,
например, природы, искусства и музыки?"

# Conscientiousness
attr(chronbach_data$c1, "label") <- "Выполняя какое-то задание, Вы очень аккуратны?"
attr(chronbach_data$c2, "label") <- "Вам больше нравится расслабляться, чем усердно трудиться?"
attr(chronbach_data$c3, "label") <- "Вы работаете очень хорошо и быстро?"

# Extraversion
attr(chronbach_data$e1, "label") <- "Вы разговорчивы?"
attr(chronbach_data$e2, "label") <- "Вы предпочитаете держать свое мнение при себе?"
attr(chronbach_data$e3, "label") <- "Вы открыты и общительны, например, Вы очень легко заводите друзей?"

# Agreeableness
attr(chronbach_data$a1, "label") <- "Вы легко прощаете других людей?"
attr(chronbach_data$a2, "label") <- "Вы очень вежливы с другими людьми?"
attr(chronbach_data$a3, "label") <- "Вы щедро делитесь с другими людьми своим временем и деньгами?"

# Emotional Stability
attr(chronbach_data$es1, "label") <- "Вы спокойны в стрессовых ситуациях?"
attr(chronbach_data$es2, "label") <- "Вы склонны к беспокойству?"
attr(chronbach_data$es3, "label") <- "Вас легко заставить нервничать?"

# openness  
opn_items      <- tab_itemscale(chronbach_data[, c("o1", "o2", "o3")],
                                factor.groups.titles = "Открытость") 


# conscientiousness
cons_items      <- tab_itemscale(chronbach_data[, c("c1", "c2", "c3")],
                                 factor.groups.titles = "Добросовестность")


cons_items_alt  <- tab_itemscale(chronbach_data[, c("c1", "c2", "c3", "g1", "g2", "g3", "d2")])

# extraversion
extr_items     <- tab_itemscale(chronbach_data[, c("e1", "e2", "e3")],
                                factor.groups.titles = "Экстраверсия")

# agreeableness
agr_items      <- tab_itemscale(chronbach_data[, c("a1", "a2", "a3")],
                                factor.groups.titles = "Дружелюбие")

agr_items_alt  <- tab_itemscale(chronbach_data[, c("a1", "a2", "a3", "d3")])

# emotional stability
em_st_items     <- tab_itemscale(chronbach_data[, c("es1", "es2", "es3")],
                                 factor.groups.titles = "Эмоциональная стабильность")
em_st_items_alt <- tab_itemscale(chronbach_data[, c("es1", "es2", "es3", "h1", "h2", "d1", "d4")])



sanitize_tabscale_output <- function(tabscale_obj) {
  # Fix data frame content
  df <- tabscale_obj$df.list[[1]]
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x <- gsub("&", "\\\\&", x)
      x <- gsub("%", "\\\\%", x)
      x <- gsub("_", "\\\\_", x)
      x <- gsub("#", "\\\\#", x)
    }
    x
  })
  colnames(df) <- gsub("&alpha;", "$\\\\alpha$", colnames(df))
  tabscale_obj$df.list[[1]] <- df
  
  # Fix HTML-like outputs
  sanitize_html <- function(text) {
    text <- gsub("&alpha;", "$\\\\alpha$", text)
    text <- gsub("&middot;", "$\\\\cdot$", text)
    text <- gsub("&", "\\\\&", text)
    text <- gsub("%", "\\\\%", text)
    text <- gsub("#", "\\\\#", text)
    return(text)
  }
  
  tabscale_obj$page.content  <- sanitize_html(tabscale_obj$page.content)
  tabscale_obj$page.complete <- sanitize_html(tabscale_obj$page.complete)
  tabscale_obj$knitr         <- sanitize_html(tabscale_obj$knitr)
  
  return(tabscale_obj)
}

# opn_items      <- sanitize_tabscale_output(opn_items)
# cons_items     <- sanitize_tabscale_output(cons_items)
# extr_items     <- sanitize_tabscale_output(extr_items)
# agr_items      <- sanitize_tabscale_output(agr_items)
# em_st_items    <- sanitize_tabscale_output(em_st_items)

# opn_items      
# cons_items     
# extr_items     
# agr_items      
# em_st_items   

### correlation matrix of NCS amongst youth

correlation_matrix_data <-
  descr_ind_2016_2019_empl %>%
  filter(id_w == "28") %>%
  select(O, C, E, A, ES) %>%
  rename(
    `Открытость` = O,
    `Добросовестность` = C,
    `Экстраверсия` = E,
    `Доброжелательность` = A,
    `Эмоциональная стабильность` = ES
  ) 

# build a correlation plot

make_scale_table <- function(data, items, scale_name) {
  cat("Шкала:", scale_name, "\n")
  cat("Переменные:", paste(items, collapse = ", "), "\n\n")
  
  df <- data[, items, drop = FALSE]
  
  # alpha
  a <- psych::alpha(df, check.keys = FALSE, warnings = FALSE)
  
  # mean inter-item correlation считаем вручную
  cor_mat <- stats::cor(df, use = "pairwise.complete.obs")
  mean_inter_item_corr <- mean(cor_mat[lower.tri(cor_mat)], na.rm = TRUE)
  
  tibble(
    trait = scale_name,
    item = items,
    missings = sapply(df, \(x) mean(is.na(x)) * 100),
    mean = sapply(df, \(x) mean(x, na.rm = TRUE)),
    sd = sapply(df, \(x) sd(x, na.rm = TRUE)),
    skew = sapply(df, \(x) e1071::skewness(x, na.rm = TRUE, type = 2)),
    item_difficulty = sapply(df, \(x) mean(x, na.rm = TRUE) / max(x, na.rm = TRUE)),
    item_discrimination = a$item.stats$r.drop,
    alpha_if_deleted = a$alpha.drop$raw_alpha,
    cronbach_alpha = a$total$raw_alpha,
    mean_inter_item_corr = mean_inter_item_corr
  )
}

scales <- list(
  "Открытость" = c("o1", "o2", "o3"),
  "Добросовестность" = c("c1", "c2", "c3"),
  "Экстраверсия" = c("e1", "e2", "e3"),
  "Доброжелательность" = c("a1", "a2", "a3"),  # или g1:g3, если это твоя agreeableness
  "Эмоциональная стабильность" = c("es1", "es2", "es3")
)

lapply(scales, \(x) x %in% names(chronbach_data))


big5_item_table <- purrr::imap_dfr(
  scales,
  ~ make_scale_table(chronbach_data, items = .x, scale_name = .y)
)

# make_scale_table(chronbach_data, c("o1", "o2", "o3"), "Открытость")

big5_item_table

