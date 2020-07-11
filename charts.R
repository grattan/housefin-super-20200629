options("dplyr.summarise.inform" = FALSE)
options(digits = 3)
options(scipen = 99)
library(hutilscpp)
library(taxstats)
library(grattanCharts)
library(tidyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(grattantheme)
library(grattandata)
library(hutils)

"%between%" <- data.table::`%between%`


#' Stacked charts with labels at right
#'
#' @param .data A data frame, containing entries for \code{x}, \code{y}, and \code{fill}. \code{x} and \code{fill} must be ordered factors.
#' @param geom The type of chart ("bar", "area").
#' @param barwidth Passed to the \code{width} argument of \code{geom_bar}
#' @param verbose Report the margin used (in grid:: 'lines').
#' @param right_margin The amount of padding at right to use. The whole point of this function is to select a good right margin to allow space. But if the margin provided is wrong, it can be changed manually here.
#' @param reverse (logical) Use the reverse palette.
#' @param scale_fill_manual_args Arguments passed to \code{ggplot2::scale_fill_manual}.
#' @param scale_y_args A list of arguments passed to r \code{ggplot2::scale_y_continuous}.
#' @param x_continuous Should the x axis be continuous?
#' @param scale_x_args A list of arguments passed to \code{ggplot2::scale_x_discrete}. If \code{x_continuous}, then the arguments passed to \code{ggplot2::scale_x_continuous}.
#' @param coord_cartesian_args A list of arguments passed to \code{ggplot2::coord_cartesian}.
#' @param text_family Text family for theme and geom text.
#' @param Annotate_Args A list of list of arguments passed to \code{ggplot2::annotate}. Each element of the top-level list is an additional layer of \code{annotate}.
#' @param theme_grattan.args Arguments passed to \code{theme_hugh}, an alias for \code{theme_grattan}. (For example, the \code{base_size}.)
#' @param theme.args A list of arguments passed to \code{ggplot2::theme}.
#' @param nudge_up A numeric vector to be added every text y-coordinate.
#' @param nudge_right Move text right in units of \code{x}.
#' @param extra_left_spaces Number of space characters \code{" "} preceding the text labels. Extra space characters are added before every newline.
#' @return A chart with the labels in the right gutter
#' @importFrom graphics strwidth
#' @examples
#' library(data.table)
#' dat <- data.table::CJ(
#'   x = factor(1:10, ordered = TRUE),
#'   fill = factor(c("A long but not\ntoo long label", letters[2:3]),
#'                 levels = c("A long but not\ntoo long label", letters[2:3]),
#'                 ordered = TRUE)
#' )
#' dat$y <- abs(rnorm(1:nrow(dat)))
#'
#' stacked_bar_with_right_labels(dat)
#'
#'
#' @export



stacked_bar_with_right_labels <- function(.data,
                                          geom = "bar",
                                          barwidth,
                                          verbose = FALSE,
                                          right_margin = 0.5,
                                          reverse = FALSE,
                                          scale_fill_manual_args,
                                          scale_y_args,
                                          x_continuous = FALSE,
                                          scale_x_args,
                                          coord_cartesian_args,
                                          text_family = NULL,
                                          Annotate_Args,
                                          theme_grattan.args,
                                          theme.args,
                                          nudge_up = 0,
                                          nudge_right = 0.5,
                                          extra_left_spaces = 0L){
  stopifnot(all(c("x", "y", "fill") %in% names(.data)))
  x = y = fill = text.label = text.x = text.y = NULL
  if(!is.factor(.data$fill) || !is.ordered(.data$fill)){
    stop("'fill' must be an ordered factor.")
  }
  if (!x_continuous){
    if (!is.factor(.data$x) || !is.ordered(.data$x)){
      stop("'x' must be an ordered factor.")
    }
  } else {
    if (!is.numeric(.data$x)){
      stop("x must be numeric")
    }
  }
  if (is.null(text_family)) {
    if (requireNamespace("sysfonts", quietly = TRUE) &&
        "helvet" %in% sysfonts::font_families()) {
      text_family = "helvet"
    } else {
      text_family = ""
    }
  }


  .plot.data <-
    .data %>%
    as.data.table %>%
    # our label should only appear at the last x
    .[, text.label := if_else(x == max(x),
                              paste0(paste0(rep(" ", extra_left_spaces), collapse = ""),
                                     gsub("\n",
                                          # Add extra white space (push to right margin)
                                          paste0("\n", paste0(rep(" ", extra_left_spaces), collapse = "")),
                                          as.character(fill),
                                          fixed = TRUE)),
                              NA_character_)] %>%
    # it should be as high as the corresponding bar:
    # all the way up the previous, then half of the corresponding height
    setorder(-fill) %>%
    .[, text.y := -y/2 + cumsum(y) + nudge_up, by = x] %>%
    .[, text.x := max(as.numeric(.data$x)) + nudge_right]


  label_max_width <-
    # longest spell between '\n <---> \n'
    strsplit(as.character(unique(.data$fill)), split = "\n") %>%
    unlist %>%
    # actual character size in bold `Arial'
    strwidth(., units = "inches", font = 2, family = "sans") %>%
    max

  # To convert to lines, use "X" as approximation
  eX <- strwidth("X", units = "inches")
  # 1.01 actually seems too wide for Helvetica.
  label_max_width <- 1.00 * label_max_width / eX
  if (verbose){
    message('I chose ', label_max_width, ' for the right margin.\n',
            'If my choice of margin is unsuitable for the label,\n',
            'you can use\n',
            '  right_margin = ',
            '\nas a replacement for ', label_max_width, '\n',
            'It is my job to select a good margin; so please\n',
            'report any bad choices of mine as a bug.')
  }

  ## Need to check whether the texts will overlap

  if (geom == "bar"){
    if (missing(barwidth)){
      p <-
        grplot(.plot.data, reverse = reverse) +
        theme_hugh(base_size = 18, base_family = text_family) +
        ggplot2::geom_bar(ggplot2::aes(x = x, y = y, fill = fill),
                          color = "white",
                          stat = "identity") +
        ggplot2::geom_text(ggplot2::aes(label = text.label,
                                        x = text.x,
                                        y = text.y,
                                        colour = fill),
                           na.rm = TRUE,
                           hjust = 0,
                           lineheight = 0.9,
                           family = text_family,
                           size = 18/(14/5),
                           fontface = "bold")
    } else {
      p <-
        grplot(.plot.data, reverse = reverse) +
        theme_hugh(base_size = 18, base_family = text_family) +
        ggplot2::geom_bar(ggplot2::aes(x = x, y = y, fill = fill),
                          stat = "identity",
                          color = "white",
                          width = barwidth) +
        ggplot2::geom_text(ggplot2::aes(label = text.label,
                                        x = text.x,
                                        y = text.y,
                                        colour = fill),
                           na.rm = TRUE,
                           hjust = 0,
                           lineheight = 0.9,
                           family = text_family,
                           size = 18/(14/5),
                           fontface = "bold")
    }
    if (!missing(scale_fill_manual_args)){
      if (!missing(reverse)){
        warning("Both 'scale_fill_manual_args' and 'reverse' provided; 'reverse' will be ignored.")
      }
      p <- p + do.call(ggplot2::scale_fill_manual, args = scale_fill_manual_args)
      # To match with the text labels!
      p <- p + do.call(ggplot2::scale_color_manual, args = scale_fill_manual_args)
    }

    if (!missing(scale_x_args)){
      if (x_continuous){
        p <- p + do.call(ggplot2::scale_x_continuous, args = scale_x_args)
      } else {
        p <- p + do.call(ggplot2::scale_x_discrete, args = scale_x_args)
      }
    }

    if (!missing(scale_y_args)){
      p <- p + do.call(ggplot2::scale_y_continuous, args = scale_y_args)
    }

    if (!missing(coord_cartesian_args)){
      p <- p + do.call(ggplot2::coord_cartesian, args = coord_cartesian_args)
    }

    if (!missing(Annotate_Args)){
      for (aa in seq_along(Annotate_Args)){
        p <- p + do.call(ggplot2::annotate, args = Annotate_Args[[aa]])
      }
      rm(aa)
    }

    if (!missing(theme_grattan.args)){
      p <- p + do.call(theme_hugh, theme_grattan.args)
    }

    if (missing(right_margin)){
      p <- p + ggplot2::theme(plot.margin = grid::unit(c(0.7, label_max_width, 0.5, 0),
                                                       "lines"))
    } else {
      p <- p + ggplot2::theme(plot.margin = grid::unit(c(0.7, right_margin, 0.5, 0),
                                                       "lines"))
    }

    if (!missing(theme.args)){
      p <- p + do.call(theme, theme.args)
    }
  } else {
    stop("You've asked for a geom which is not supported.")
  }
  grid::grid.newpage()
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.draw(gt)
}





s1718 <-
  tryCatch(read_microdata("ato_2018_sample_file.csv", fast = TRUE),
           error = function(e) {
             readr::read_csv("~/taxstats1718/2018_sample_file.csv")
           })

s2021 <-
  s1718 %>%
  project_to(to_fy = "2020-21",
             fy.year.of.sample.file = "2017-18",
             lf.series = 0,
             r_super_balance = 1.07) %>%
  apply_super_caps_and_div293(cap = 25e3,
                              age_based_cap = FALSE,
                              div293_threshold = 250e3)

TaxExpenditure2021 <-
s1718 %>%
  project_to(to_fy = "2020-21",
             fy.year.of.sample.file = "2017-18",
             lf.series = 0,
             r_super_balance = 1.07) %>%
  model_new_caps_and_div293(fy.year = "2020-21",
                            prv_cap = 25e3,
                            new_cap = 0,
                            new_age_based_cap = FALSE,
                            prv_age_based_cap = FALSE,
                            new_contr_tax = "mr - 0%",
                            prv_div293_threshold = 250e3,
                            new_div293_threshold = 250e3)
grattan_save_all <- function(filename, object) {
  grattantheme::grattan_save(filename = filename,
                             object = object,
                             type = "all",
                             save_pptx = TRUE,
                             save_data = TRUE)
}

# Figure 3.
grattan_save_all(provide.file("Figure-3-1/Figure31.pdf"), {
  TaxExpenditure2021 %>%
    as_tibble %>%
    filter(old_concessional_contributions > 0) %>%
    mutate_ntile("old_Taxable_Income", n = 10L) %>%

    group_by(old_Taxable_IncomeDecile) %>%
    summarise(TotalBenefits = sum(new_revenue - prv_revenue)) %>%
    mutate(Decile = factor(old_Taxable_IncomeDecile)) %>%
    mutate(Percentage = 100 * TotalBenefits / sum(TotalBenefits)) %>%
    ggplot(aes(x = Decile, y = Percentage)) +
    geom_col() +
    scale_y_continuous_grattan() +
    theme_grattan() +
    ggtitle("Superannuation tax breaks primarily benefit high-income earners",
            subtitle = "Percentage of total tax breaks")
})

# Figure 4.3
grattan_save_all(provide.file("Figure-4-3/Figure43.pdf"), {
  s2021 %>%
    as_tibble %>%
    mutate(TaxBracket = case_when(Taxable_Income <= 18200 ~ "Less than\n$18,200",
                                  Taxable_Income <= 37000 ~ "$18,200 - $37,000",
                                  Taxable_Income <= 90000 ~ "$37,000 - $90,000",
                                  Taxable_Income <= 180e3 ~ "$90,000 - $180,000",
                                  TRUE ~ "180,000+")) %>%
    mutate(TaxBracket = factor(TaxBracket,
                               levels = c("Less than\n$18,200",
                                          "$18,200 - $37,000",
                                          "$37,000 - $90,000",
                                          "$90,000 - $180,000",
                                          "180,000+"))) %>%
    group_by(TaxBracket) %>%
    summarise(minIncome = min(Taxable_Income),
              avg_SG_contributions = mean(SG_contributions),
              avg_salary_sacrifice_contributions = mean(salary_sacrifice_contributions),
              avg_personal_deductible_contributions = mean(personal_deductible_contributions),
              avg_non_concessional_contributions = mean(non_concessional_contributions)) %>%
    pivot_longer(cols = grep("^avg", names(.), value = TRUE)) %>%
    mutate(name = trim_common_affixes(name)) %>%
    ggplot(aes(x = TaxBracket, y = value)) +
    geom_col(aes(fill = name)) +
    theme_grattan() +
    grattan_fill_manual() +
    scale_y_continuous_grattan(labels = grattanCharts::grattan_dollar)
})

# Figure 4.4
# Average concessional contributions for people over 50
grattan_save_all(provide.file("Figure-4-4/Figure44.pdf"), {
  s2021 %>%
    as_tibble %>%
    filter(age_range <= 4) %>%
    mutate_ntile("Taxable_Income", n = 10L) %>%
    group_by(Taxable_IncomeDecile) %>%
    summarise(avg_SG_contributions = mean(SG_contributions),
              avg_salary_sacrifice_contributions = mean(salary_sacrifice_contributions),
              avg_personal_deductible_contributions = mean(personal_deductible_contributions)) %>%
    pivot_longer(cols = grep("^avg", names(.), value = TRUE)) %>%
    mutate(fill = trim_common_affixes(name)) %>%
    mutate(fill = Switch(trim_common_affixes(fill),
                         "SG" = "Compulsory\ncontributions",
                          "salary_sacrifice" = "Salary-\nsacrificed\nvoluntary\ncontributions",
                         "personal_deductible" = "Personal\nvoluntary\ncontributions",
                         DEFAULT = ""),
           fill = factor(fill,
                         levels = rev(c("Compulsory\ncontributions",
                                        "Salary-\nsacrificed\nvoluntary\ncontributions",
                                        "Personal\nvoluntary\ncontributions")),
                         ordered = TRUE),
           x = factor(Taxable_IncomeDecile, levels = 1:10, ordered = TRUE),
           y = value) %>%
    arrange(desc(fill)) %>%
  # stacked_bar_with_right_labels(scale_fill_manual_args = list(values = grattantheme::grattan_pal(n = 3)),
  #                               scale_y_args = list(expand = c(0, 0), labels = grattanCharts::grattan_dollar),
  #                               scale_x_args = list(name = "Taxable income decile"))
    group_by(Taxable_IncomeDecile) %>%
    mutate(y2 = cumsum(dplyr::lag(y, default = 0)) + y / 2) %>%
    mutate(label = if_else(Taxable_IncomeDecile == 10, as.character(fill), NA_character_),
           x_text = 10.5) %>%
    ggplot(aes(x = x, y = y, fill = fill)) +
    geom_col() +
    theme_grattan() +
    grattan_fill_manual() +
    grattan_colour_manual() +
    geom_text(aes(x = x_text, y = y2, color = fill, label = label),
              hjust = 0,
              lineheight = 0.8,
              fontface = "bold",
              na.rm = TRUE) +
    scale_x_discrete(name = "Taxable income decile", expand = expansion(add = c(0.5, 2.5))) +
    scale_y_continuous_grattan(labels = scales::dollar) +
    ggtitle("Contributions among over 50s are heavily skewed towards high-income earners",
            subtitle = "Average concessional contribution for taxfilers over 50 (2020-21)")
})
# Figure 4.5
grattan_save_all(provide.file("Figure-4-5/Figure45.pdf"), {
  s2021 %>%
    as_tibble %>%
    mutate(Age = if_else(age_range <= 3, "55+", "<55")) %>%
    mutate_ntile("Taxable_Income", n = 10L) %>%
    group_by(Age, Taxable_IncomeDecile) %>%
    summarise(tot = sum(concessional_contributions - SG_contributions)) %>%
    ungroup %>%
    mutate(p = tot / sum(tot),
           ordering = if_else(Age != "55+", Taxable_IncomeDecile, 21L - Taxable_IncomeDecile)) %>%

    arrange(desc(ordering)) %>%
    mutate(text_y = cumsum(dplyr::lag(p, default = 0)) + p / 2,
           text_label = if_else(Taxable_IncomeDecile <= 5, "", as.character(Taxable_IncomeDecile))) %>%

    ggplot(aes(x = 1,
               y = p,
               fill = Age,
               group = interaction(ordering, Age))) +
    geom_col() +
    coord_polar(theta = "y", direction = 1) +
    scale_y_continuous_grattan(expand_top = 0,
                               breaks = c(0, 10, 16, 22, 29, 38, 48, 60, 72, 82, 88) / 100,
                               labels = function(x) paste0(as.integer(x * 100), "%")) +
    geom_text(aes(label = text_label,
                  y = text_y,
                  group = interaction(ordering, Age))) +
    theme_grattan() +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    grattan_fill_manual(n = 2)
})
# Figure 4.6: but for 11k (rather than 10k)
grattan_save_all(provide.file("Figure-4-6/Figure46.pdf"), {
  s2021 %>%
    mutate_ntile("Taxable_Income", n = 10L) %>%
    filter(concessional_contributions > 11e3) %>%
    mutate(Sex = if_else(Gender == 1, "Female", "Male")) %>%
    group_by(Taxable_IncomeDecile, Sex) %>%
    summarise(n_taxfilers = sum(WEIGHT),
              n_taxfilers_SG = sum(WEIGHT * (SG_contributions > 11e3))) %>%
    mutate(n_taxfilers_nonSG = n_taxfilers - n_taxfilers_SG) %>%
    select(-n_taxfilers) %>%
    pivot_longer(cols = c(grep("^n_", names(.), value = TRUE))) %>%
    mutate(name = trim_common_affixes(name),
           fill = if_else(name != "non", "All", "Voluntary only"),
           Taxable_IncomeDecile = factor(Taxable_IncomeDecile),
           Sex = factor(Sex, levels = c("Male", "Female"))) %>%
    ggplot(aes(x = Taxable_IncomeDecile, y = value, fill = fill)) +
    geom_col() +
    facet_wrap(~Sex) +
    theme_grattan() +
    labs(x = "Taxable income decile") +
    scale_y_continuous_grattan(labels = scales::comma) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) +
    grattan_fill_manual(n = 2) +
    ggtitle("Few people other than high-income earners contribute over $11,000 a year",
            subtitle = paste("Number of taxfilers with more than $11,000 in concessional contributions, 2020-21"))

})
# Figure 4.7
# Value of pre-tax voluntary contributions to superannuation in excess of
# $10,000 in a year, 2012-13, $2015-16 (by age)
grattan_save_all(provide.file("Figure-4-7/Figure47.pdf"), {
  s2021 %>%
    merge(age_range_decoder, by = "age_range") %>%
    as_tibble %>%
    mutate(voluntary_concessional_contributions = concessional_contributions - SG_contributions) %>%
    filter(voluntary_concessional_contributions > 11e3) %>%
    arrange(age_range_description) %>%
    mutate(Age = if_else(age_range <= 3, "55+", "<55"),
           age_range_description = if_else(age_range >= 7, "Under\n40", as.character(age_range_description)),
           age_range_description = if_else(age_range == 0, "70+", age_range_description),
           age_range_description = forcats::fct_inorder(age_range_description)) %>%
    group_by(Age, age_range_description) %>%
    summarise(tot = sum(voluntary_concessional_contributions)) %>%
    ungroup %>%
    mutate(p = tot / sum(tot)) %>%
    arrange(desc(age_range_description)) %>%
    mutate(text_y = cumsum(dplyr::lag(p, default = 0)) + p / 2,
           text_label =  as.character(age_range_description),
           text_x = seq(0.8, 1.15, length.out = n())) %>%

    ggplot(aes(x = 1,
               y = p,
               fill = Age,
               group = interaction(age_range_description, Age))) +
    geom_col() +
    coord_polar(theta = "y", direction = 1) +
    scale_y_continuous_grattan(expand_top = 0,
                               breaks = c(0, 10, 16, 22, 29, 38, 48, 60, 72, 82, 88) / 100,
                               labels = function(x) paste0(as.integer(x * 100), "%")) +
    geom_text(aes(x = text_x,
                  label = text_label,
                  y = text_y,
                  group = interaction(age_range_description, Age)),
              lineheight = 0.85) +
    theme_grattan() +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    grattan_fill_manual(n = 2)
})

# Figure 5.1
grattan_save_all(provide.file("Figure-5-1/Figure51.pdf"), {
  s2021 %>%
    as_tibble %>%
    mutate(Age = if_else(age_range <= 3, "55+", "<55")) %>%
    mutate(TaxBracket = case_when(Taxable_Income <= 18200 ~ "Less than\n$18,200",
                                  Taxable_Income <= 37000 ~ "$18,200 - $37,000",
                                  Taxable_Income <= 90000 ~ "$37,000 - $90,000",
                                  Taxable_Income <= 180e3 ~ "$90,000 - $180,000",
                                  TRUE ~ "180,000+")) %>%
    mutate(TaxBracket = factor(TaxBracket,
                               levels = c("Less than\n$18,200",
                                          "$18,200 - $37,000",
                                          "$37,000 - $90,000",
                                          "$90,000 - $180,000",
                                          "180,000+"))) %>%
    group_by(Age, TaxBracket) %>%
    summarise(tot = sum(concessional_contributions - SG_contributions)) %>%
    ungroup %>%
    mutate(p = tot / sum(tot),
           ordering = if_else(Age != "55+", as.integer(TaxBracket), 21L - as.integer(TaxBracket))) %>%

    arrange(desc(ordering)) %>%
    mutate(text_y = cumsum(dplyr::lag(p, default = 0)) + p / 2,
           text_label = if_else(TaxBracket %in% levels(TaxBracket)[1:2], "", as.character(TaxBracket)),
           text_label = gsub(",000", "k", text_label)) %>%

    ggplot(aes(x = 1,
               y = p,
               fill = Age,
               group = interaction(ordering, Age))) +
    geom_col() +
    coord_polar(theta = "y", direction = 1) +
    scale_y_continuous_grattan(expand_top = 0,
                               breaks = c(0, 10, 37, 55, 60, 65, 82, 97) / 100,
                               labels = function(x) paste0(as.integer(x * 100), "%")) +
    geom_text(aes(x = 1.15,
                  label = text_label,
                  y = text_y,
                  group = interaction(ordering, Age))) +
    theme_grattan() +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          legend.title = element_text(hjust = 0),
          legend.position = "right",
          legend.direction = "vertical") +
    guides(fill = guide_legend("Age")) +
    grattan_fill_manual(n = 2) +
    ggtitle("Voluntary post-tax contributions are mostly made by those who are older and on high incomes",
            subtitle = "Percentage of voluntary post-tax contributions, 2020-21")

})
# Figure 5.2
grattan_save_all(provide.file("Figure-5-2/Figure52.pdf"), {
  s2021 %>%
    mutate(Balance = cut(MCS_Ttl_Acnt_Bal,
                         breaks = c(-Inf, 100e3, 250e3, 500e3, 750e3, 1e6, 2e6, Inf),
                         labels = c("Less than $100,000",
                                    "$100,000 to $250,000",
                                    "$250,000 to $500,000",
                                    "$500,000 to $750,000",
                                    "$750,000 to $1 million",
                                    "$1 million to $2 million",
                                    "More than $2 million"),
                         ordered_result = TRUE)) %>%
    group_by(Balance) %>%
    summarise(n_taxfilers = n(),
              v_posttax_contributions = sum(non_concessional_contributions)) %>%
    mutate(p_taxfilers = n_taxfilers / sum(n_taxfilers),
           p_posttax_contributions = v_posttax_contributions / sum(v_posttax_contributions)) %>%
    pivot_longer(grep("^p_", names(.), value = TRUE)) %>%
    mutate(name = trim_common_affixes(name),
           name = factor(if_else(name %ein% "taxfiler", "Taxfilers", "Post-tax contributions"),
                         levels = c("Taxfilers", "Post-tax contributions"),
                         ordered = TRUE)) %>%
    arrange(name, Balance) %>%
    group_by(name) %>%
    mutate(text_y = cumsum(dplyr::lag(value, default = 0)) + value / 2,
           text_color = if_else(Balance %ein% c("More than $2 million",
                                                "$1 million to $2 million"),
                                "white",
                                "black"),
           text_color = if_else(name == "Taxfilers", NA_character_, text_color)) %>%
    ggplot(aes(x = name, y = value, fill = Balance)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    geom_text(aes(x = name, y = text_y, label = Balance,
                  color = text_color),
              position = position_identity(),
              na.rm = TRUE) +
    scale_color_identity() +
    scale_fill_manual(values = c(grattantheme::grattan_lightyellow, grattantheme::grattan_yellow,
                                 grattantheme::grattan_lightorange, grattantheme::grattan_darkorange,
                                 grattantheme::grattan_red, grattantheme::grattan_darkred,
                                 "black")) +
    theme_grattan() +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous_grattan(labels = function(x) paste0(as.integer(x * 100), "%"))
})

# Figure 6.1
# Superannuation earnings by 60+ year old, 2015-16
# grattan_save_all(provide.file("Figure-6-1/Figure61.pdf"), {
#   s2021 %>%
#
# })


# Figure 6.2
# Average additional tax paid by 60+ year olds under reform proposals, by
# total income decile (including super earnings), $2015-16
grattan_save_all(provide.file("Figure-6-2/Figure62.pdf"), {
  s2021 %>%
    .[, MarginalRate := grattan:::marginal_rate(.SD, fy.year = "2020-21")] %>%
    mutate(tax = income_tax(Taxable_Income, "2020-21", .dots.ATO = .)) %>%
    as_tibble %>%
    # select()s hereinafter are just for debugging (to view files)
    select(age_range, Partner_status,
           MCS_Ttl_Acnt_Bal,
           MarginalRate,
           Taxable_Income, Tot_inc_amt, WEIGHT, tax) %>%
    filter(age_range <= 2) %>%

    # tax free threshold assumed to depend only on the following:
    group_by(age_range, Partner_status) %>%

    # Use 'most common taxable income of people paying small but nozero tax
    # as proxy for true taxfree threshold
    mutate(taxfree_threshold = Mode(Taxable_Income[tax %between% c(1, 100)]),
           # This it the amount available if earnings were taxed at 15%
           minTaxable_Income_blw_15pc = min(Taxable_Income[MarginalRate > 0.15])) %>%
    ungroup %>%
    mutate(unused_taxfree_threshold = pmax0(taxfree_threshold - Taxable_Income),
           unused_income_blw_15pc = pmax0(minTaxable_Income_blw_15pc - Taxable_Income)) %>%
    mutate(earnings = 0.05 * MCS_Ttl_Acnt_Bal,
           earnings_tax = 0.15 * earnings,

           # Allow earnings to be transferred to Taxable Income only to the point
           # minTaxable_Income_blw_15pc -- any more and the personal income tax
           # paid would exceed the benefit of behaviour change
           # 'transfer from super earnings to personal income tax'

           earnings_w_behaviour_change = pmax0(earnings - unused_income_blw_15pc),
           earnings_tax_w_behaviour_change = 0.15 * earnings_w_behaviour_change,
           Taxable_Income_w_behaviour_change = Taxable_Income + pminV(earnings, unused_income_blw_15pc),

           # Same thing except using earnings - 20k as the earnings at risk
           earnings_abv_20k = pmax0(earnings - 20e3),
           earnings_tax_abv_20k = 0.15 * earnings_abv_20k,
           earnings_w_behaviour_change_20k_threshold = pmax0(earnings_abv_20k - unused_income_blw_15pc),
           earnings_tax_w_behaviour_change_20k_threshold = 0.15 * earnings_w_behaviour_change_20k_threshold,
           Taxable_Income_w_behaviour_change_20k_threshold = Taxable_Income + pminV(earnings_abv_20k, unused_income_blw_15pc),


           TotalIncomePlusEarnings = Tot_inc_amt + earnings,
           TotalIncomePlusEarningsDecile = weighted_ntile(TotalIncomePlusEarnings, n = 10L)) %>%
    mutate(tax_behaviour_change = income_tax(Taxable_Income_w_behaviour_change,
                                             "2020-21",
                                             .dots.ATO = s2021[age_range <= 2]),
           tax_behaviour_change_20k_threshold = income_tax(Taxable_Income_w_behaviour_change_20k_threshold,
                                                           "2020-21",
                                                           .dots.ATO = s2021[age_range <= 2])) %>%
    group_by(TotalIncomePlusEarningsDecile) %>%
    summarise(avg_extra_tax_ante_behaviour_change = mean(earnings_tax),
              avg_extra_tax_post_behaviour_change = mean(tax_behaviour_change - tax + earnings_tax_w_behaviour_change),
              avg_extra_tax_over20k = mean(earnings_tax_abv_20k),
              avg_extra_tax_over20k_post_behaviour_change = mean(tax_behaviour_change_20k_threshold - tax + earnings_tax_w_behaviour_change_20k_threshold)) %>%
    pivot_longer(grep("^avg_", names(.), value = TRUE)) %>%
    mutate(Decile = factor(TotalIncomePlusEarningsDecile),
           name = trim_common_affixes(name),
           name = Switch(name,
                         "ante_behaviour_change" = "15% tax on all super earnings",
                         "post_behaviour_change" = "15% tax on all super earnings after behaviour change",
                         "over20k" = "15% tax on super earnings over $20,000",
                         "over20k_post_behaviour_change" = "15% tax on super earnings over $20,000\nafter behaviour change",
                         DEFAULT = ""),
           name = forcats::fct_inorder(name)) %>%
    ggplot(aes(x = Decile, y = value, fill = name)) +
    geom_col(position = position_dodge()) +
    scale_y_continuous_grattan(labels = grattan_dollar) +
    theme_grattan() +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.direction = "vertical") +
    grattan_fill_manual(n = 4, palette = "dark") +
    ggtitle("A tax on earnings in retirement would mostly affect those with higher incomes",
            subtitle = "Average additional tax paid by 60+ year olds under reform proposals, by total income decile (including super earnings), 2020-21")
})
# Figure 6.2





