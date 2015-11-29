#' Very compact Japanese tokenizer
#'
#' R version of TinySegmenter, compact Japanese tokenizer
#'
#' rTinySegmenter is a R version of TinySegmenter, which is an extremely compact Japanese tokenizer originally written in JavaScript by Mr. Taku Kudo.
#'
#' @name rTinySegmenter-package
#' @aliases rTinySegmenter-package
#' @rdname rTinySegmenter_package
#' @docType package
#' @keywords documentation
#'
#' @author
#' Author: Yoshiaki AMANO. \cr
#' Maintainer: Yoshiaki AMANO. \cr
#' Twitter: @@yamano357
#' @seealso
#' \code{\link[openNLP:Parse_Annotator]{openNLP::Parse_Annotator}} \cr
#' \code{\link[NLP:annotators]{NLP::annotators}} \cr
#' \code{\link[tm:tokenizer]{tm::tokenizer}} \cr
#' \url{http://chasen.org/~taku/software/TinySegmenter/}
#' @import
#'  dplyr
#'  jsonlite
#'  iterators
#'  foreach
#'  @importFrom
#'  tidyr spread_
#'  @importFrom
#'  stringi stri_detect_charclass stri_detect
#'  @importFrom
#'  stringr str_length str_split str_split str_split_fixed str_replace_all str_detect

#' @exportPattern '^[[:alpha:]]+'
NULL

library(R6)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)
library(jsonlite)
library(iterators)
library(foreach)


tiny_segmenter <- R6::R6Class(
  classname = "TinySegmenter",
  portable = TRUE,
  public = list(
    text = NA,
    patterns = NA,
    segment = NA,
    model = NA,
    model_file = NA,
    model_attr = list(score = NULL, unit = NULL),
    initialize = function (text, patterns, model_file) {
      if (!missing(text)) {
        self$text <- text
      } else {
        self$text <- ""
      }
      if (!missing(patterns)) {
        self$patterns <- patterns
      } else {
        self$patterns <- list(
          "M" = "[一二三四五六七八九十百千万億兆]",
          "H" = "[一-龠々〆ヵヶ]",
          "I" = "[ぁ-ん]",
          "K" = "[(ァ-ヴ)|(ーｰ\uff9e)|(ｱ-ﾝ)]",
          "A" = "[a-zA-Zａ-ｚＡ-Ｚ]",
          "N" = "[0-9０-９]"
        )
      }
      if (!missing(model_file)) {
        self$model_file <- model_file
      } else{
        self$model_file <- system.file(package = "rTinySegmenter", "extdata", "const-model.json")
      }
      self$model <- jsonlite::fromJSON(txt = self$model_file)
    },

    tokenize = function (){

      if (!is.na(x = self$segment)) {
        return(NULL)
      }
      if (stringr::str_length(string = self$text) < 2) {
        return(NULL)
      }

      private$chars  <- stringr::str_split(string = self$text, pattern = "")[[1]]
      private$types <- private$detectCharType(ch = private$chars)

      if (length(private$chars) != length(private$types)) {
        return(NULL)
      }

      model_format <- private$parseModelTemplate()
      char_and_word_result <- model_format %>%
        dplyr::filter(is.element(set = c("C", "W"), el = .$attr_type)) %>%
        dplyr::group_by(attr_type) %>%
        dplyr::do(private$createModelFixAttribute(each_attr_model_format = .)) %>%
        dplyr::ungroup(x = .) %>%
        dplyr::select(-attr_type)

      private$itarativeCreateModelAttributeAndSegment(
        each_attr_model_format = model_format %>%
          dplyr::filter(is.element(set = c("P", "Q"), el = .$attr_type)),
        char_and_word_result = char_and_word_result
        )

      return(
        stringr::str_c(
          mapply(
            private$chars,
            stringr::str_replace_all(
              string = c(self$segment, "O"), pattern = c("B" = " | ", "O" = "")
            ),
            FUN = stringr::str_c, sep = ""
          ),
          collapse = ""
        )
      )
    }
  ),

  private = list(
    chars = NA,
    types = NA,
    detectCharType = function (ch) {
      return(
        apply(
          X = sapply(X = self$patterns, FUN = stringi::stri_detect_charclass, str = ch),
          MARGIN = 1,
          FUN = function (types) {
            pattern_type <- names(x = which(x = types))
            if (length(x = pattern_type) > 0) {
              return(pattern_type)
            } else {
              return("O")
            }
          }
        )
      )
    },
    parseModelTemplate = function () {
      return(
        data.frame(
          stringr::str_split_fixed(string = names(self$model$TEMPLATE), pattern = "", n = 3),
          stringsAsFactors = FALSE
        ) %>%
          dplyr::rename_(
            .dots = setNames(
              object = stringr::str_c("X", seq(from = 1, to = 3)),
              nm = c("gram_type", "attr_type", "counter")
            )
          )
      )
    },
    searchModelPattern = function(search_model_str) {
      search_model <- stringr::str_c(search_model_str, collapse = "")
      return(
        dplyr::data_frame(
          model = search_model,
          P = as.integer(x = self$model$TEMPLATE[[search_model]]$PATTERN$P),
          W = as.integer(x = self$model$TEMPLATE[[search_model]]$PATTERN$W),
          C = as.integer(x = self$model$TEMPLATE[[search_model]]$PATTERN$C)
        )
      )
    },
    createModelFixAttribute = function(each_attr_model_format) {
      apply_attr <- unique(x = each_attr_model_format$attr_type)
      if (all(apply_attr == "C") | all(apply_attr == "W")) {  # fix element
        model_by_n <- dplyr::bind_rows(
          apply(
            X = each_attr_model_format,
            MARGIN = 1,
            FUN = private$searchModelPattern
          )
        )

        bos <- eos <- "O"
        if (apply_attr == "C") { # char_type
          max_c <- max(model_by_n$C, na.rm = TRUE)
          if (max_c > 2) {
            b_window <- (as.integer(x = max_c / 2) + max_c %% 2) - 1
            e_window <- as.integer(x = max_c / 2) - 1
            unit <- c(
              rep(x = bos, b_window),
              private$types,
              rep(x = eos, e_window)
            )
          } else {
            unit <- private$types
          }
        } else if (apply_attr == "W") { # word
          max_w <- max(model_by_n$W, na.rm = TRUE)

          if (max_w > 2) {
            b_window <- (as.integer(x = max_w / 2) + max_w %% 2) - 1
            e_window <- as.integer(x = max_w / 2) - 1
            unit <- c(
              stringr::str_c("B", seq(from = b_window, to = 1)),
              private$chars,
              stringr::str_c("E", seq(from = 1, to = e_window))
            )
          } else {
            unit <- private$chars
          }
        }

        return(
          dplyr::bind_rows(
            lapply(
              X = split(x = each_attr_model_format, f = each_attr_model_format$gram_type),
              FUN = private$createModelNgram,
              unit = unit
            )
          )
        )
      } else {
        return(NULL)
      }
    },
    createModelNgram = function (
      each_gram_model_format, unit
    ) {

      apply_window_size <- dplyr::bind_rows(
        lapply(
          X = as.character(
            x = apply(X = each_gram_model_format, MARGIN = 1, FUN = stringr::str_c, collapse = "")
          ),
          FUN = function (each_attr_template_str) {
            return(
              dplyr::data_frame(
                min = as.integer(
                  x = sapply(
                    X = self$model$TEMPLATE[[each_attr_template_str]]$PATTERN,
                    FUN = min
                  )
                ),
                max = as.integer(
                  x = sapply(
                    X = self$model$TEMPLATE[[each_attr_template_str]]$PATTERN,
                    FUN = max
                  )
                )
              )
            )
          }
        )
      ) %>%
        na.omit() %>%
        unlist %>%
        range

      ngram_unit <- embed(
        x = unit[seq(from = apply_window_size[1], to = length(x = unit))],
        dimension = apply_window_size[2]
      )
      ngram_unit <- ngram_unit[, seq(from = ncol(x = ngram_unit), to = 1), drop = FALSE]

      return(
        dplyr::bind_rows(
          apply(
            X = each_gram_model_format,
            MARGIN = 1,
            FUN = private$searchModelScore,
            ngram_unit = ngram_unit,
            apply_window_min = (apply_window_size[1] - 1)
          )
        )
      )
    },

    searchModelScore = function (
      each_attr_template_str, ngram_unit, apply_window_min
    ) {
      search_model_name <- stringr::str_c(each_attr_template_str, collapse = "")

      model_score <- self$model$TEMPLATE[[search_model_name]]$SCORE

      #
      if (each_attr_template_str["attr_type"] == "Q") {
        each_apply_window_size <- self$model$TEMPLATE[[search_model_name]]$PATTERN[["C"]] - apply_window_min
      } else {
        each_apply_window_size <- self$model$TEMPLATE[[search_model_name]]$PATTERN[[each_attr_template_str["attr_type"]]] - apply_window_min
      }

      return(
        dplyr::bind_rows(
          apply(
            X = as.matrix(x = ngram_unit[, each_apply_window_size]),
            MARGIN = 1,
            FUN = function (search_unit) {
              search_unit <- stringr::str_c(search_unit, collapse = "")
              if (is.element(set = names(x = model_score), el = search_unit)) {
                return(
                  dplyr::data_frame(
                    score = as.numeric(model_score[search_unit]),
                    unit = search_unit
                  )
                )
              } else {
                return(
                  dplyr::data_frame(
                    score = 0,
                    unit = search_unit
                  )
                )
              }
            }
          )
        ) %>%
          dplyr::mutate(
            counter = seq(from = 1, to = n()),
            model = search_model_name
          )
      )
    },

    itarativeCreateModelAttributeAndSegment = function(
      each_attr_model_format,
      char_and_word_result
    ) {
      apply_attr <- unique(x = each_attr_model_format$attr_type)

      # "P": predict
      # "Q": combination(only predict + char_type)
      if (all(is.element(el = apply_attr, set = c("P", "Q")))) {

        combination_model_attr_len <- dplyr::bind_rows(
          apply(
            X = each_attr_model_format %>%
              dplyr::filter(attr_type == "Q"),
            MARGIN = 1,
            FUN = private$searchModelPattern
          )
        ) %>%
          dplyr::group_by(model) %>%
          dplyr::summarize(
            p_min = min(P, na.rm = TRUE), p_max = max(P, na.rm = TRUE),
            w_min = min(W, na.rm = TRUE), w_max = max(W, na.rm = TRUE),
            c_min = min(C, na.rm = TRUE), c_max = max(C, na.rm = TRUE)
          )

        each_window <- combination_model_attr_len %>%
          dplyr::mutate(c_diff = .$c_max - .$c_min) %>%
          .$c_diff %>%
          max(., na.rm = TRUE)
        bos <- "O"
        unit <- c(rep(x = bos, each_window), private$types)

        combination_q_unit <- private$createModelNgram(
          each_gram_model_format = each_attr_model_format %>%
            dplyr::filter(attr_type == "Q"),
          unit = unit
        )

        predict_window <- dplyr::bind_rows(
          apply(
            X = each_attr_model_format %>%
              dplyr::filter(attr_type == "P"),
            MARGIN = 1,
            FUN = private$searchModelPattern
          )
        ) %>%
          .$P %>%
          max(., na.rm = TRUE) + 1

        variable_score <- numeric(length = length(x = private$types) - 1)
        char_and_model_diff <- length(x = private$types) - (predict_window + 1)
        if (char_and_model_diff <= 0) {
          self$segment <- rep(x = "U", length = predict_window + char_and_model_diff)
        } else{
          self$segment <- c(
            rep(x = "U", predict_window),
            rep(x = "", length = length(x = private$types) - (predict_window + 1))
          )
        }

        self$segment <- foreach::foreach(
          i = iterators::iter(
            obj = seq(from = predict_window, to = length(self$segment) + predict_window - 1)
          ),
          .combine = "c",
          .export = c("self$segment", "self$model_attr")
        ) %do% {

          # predict
          predict_model_result <- dplyr::bind_rows(
            lapply(
              X = split(
                x = each_attr_model_format %>%
                  dplyr::filter(attr_type == "P"),
                f = each_attr_model_format %>%
                  dplyr::filter(attr_type == "P") %>%
                  .$gram_type
              ),
              FUN = private$createModelNgram,
              unit = self$segment[seq(from = (i - predict_window + 1), to = i)]
            )
          ) %>%
            dplyr::filter(counter == 1)

          combination_unit <- dplyr::left_join(
            x = dplyr::bind_rows(
              apply(
                X = dplyr::left_join(
                  x = combination_model_attr_len %>%
                    dplyr::select(model, p_min, p_max),
                  y = combination_q_unit %>%
                    dplyr::filter(counter == (i - predict_window + 1)) %>%
                    dplyr::select(-score, -counter),
                  by = c("model" = "model")
                ) %>%
                  dplyr::mutate(
                    p_diff = (p_max - p_min) + 1
                  ),
                MARGIN = 1,
                FUN = function (each_p){
                  each_counter <- seq(from = as.integer(x = each_p["p_min"]), to = as.integer(x = each_p["p_max"]))
                  return(
                    dplyr::bind_cols(
                      data.frame(t(x = replicate(n = length(each_counter), each_p)), stringsAsFactors = FALSE),
                      dplyr::data_frame(
                        P = each_counter
                      )
                    )
                  )
                }
              )
            ) %>%
              dplyr::select(q_model = model, q_unit = unit, P),
            y = dplyr::left_join(
              x = predict_model_result %>%
                dplyr::filter(stringr::str_detect(string = .$model, pattern = "^U")) %>%
                dplyr::select(model, unit),
              y = dplyr::bind_rows(
                apply(
                  X = each_attr_model_format %>%
                    dplyr::filter(attr_type == "P"),
                  MARGIN = 1,
                  FUN = private$searchModelPattern
                )
              ) %>%
                dplyr::select(model, P),
              by = c("model" = "model")
            ) %>%
              dplyr::select(p_model = model, p_unit = unit, P),
            by = c("P" = "P")
          ) %>%
            dplyr::mutate(unit = stringr::str_c(.$p_unit, .$q_unit, sep = "")) %>%
            dplyr::select(model = q_model, unit)

          combination_result <- dplyr::bind_rows(
            apply(
              X = combination_unit,
              MARGIN = 1,
              FUN = function (each_combination){
                model_score <- self$model$TEMPLATE[[each_combination["model"]]]$SCORE
                if (is.element(set = names(model_score), el = each_combination["unit"])) {
                  return(
                    dplyr::data_frame(
                      score = model_score[[each_combination["unit"]]],
                      unit =  each_combination["unit"],
                      model = each_combination["model"]
                    )
                  )
                } else {
                  return(
                    dplyr::data_frame(
                      score = 0,
                      unit =  each_combination["unit"],
                      model = each_combination["model"]
                    )
                  )
                }
              }
            )
          )

          each_model_attr <- dplyr::bind_rows(
            char_and_word_result %>%
              dplyr::filter(counter == (i - predict_window + 1)),
            predict_model_result %>%
              dplyr::mutate(counter = (i - predict_window + 1)),
            combination_result %>%
              dplyr::mutate(counter = (i - predict_window + 1))
          )

          if (is.null(x = self$model_attr$unit)) {
            self$model_attr$unit <- each_model_attr %>%
              dplyr::select(-score) %>%
              tidyr::spread_(key_col = "model", value_col = "unit", fill = "") %>%
              dplyr::select(-counter)
          } else {
            self$model_attr$unit <- dplyr::bind_rows(
              self$model_attr$unit,
              each_model_attr %>%
                dplyr::select(-score) %>%
                tidyr::spread_(key_col = "model", value_col = "unit", fill = "") %>%
                dplyr::select(-counter)
            )
          }

          if (is.null(x = self$model_attr$score)) {
            self$model_attr$score <- each_model_attr %>%
              dplyr::select(-unit) %>%
              tidyr::spread_(key_col = "model", value_col = "score", fill = 0) %>%
              dplyr::select(-counter)
          } else {
            self$model_attr$score <- dplyr::bind_rows(
              self$model_attr$score,
              each_model_attr %>%
                dplyr::select(-unit) %>%
                tidyr::spread_(key_col = "model", value_col = "score", fill = 0) %>%
                dplyr::select(-counter)
            )
          }

          variable_score[(i - predict_window) + 1] <- (
            each_model_attr %>%
              dplyr::select(-unit) %>%
              tidyr::spread_(key_col = "model", value_col = "score", fill = 0) %>%
              dplyr::select(-counter) %>%
              rowSums
            ) + self$model$BIAS %>%
            as.numeric
          self$segment[i] <- ifelse(test = variable_score[(i - predict_window) + 1] > 0, yes = "B", no = "O")
        }
        self$segment <- self$segment[seq(from = 1, to = length(private$chars)- 1)]
        return(NULL)
      } else{
        return(NULL)
      }
    }
  )
)



#' Tokenizing Japanese text
#'
#' Tokenizing Japanese text
#'
#' Given a `text` string, `tokenize` attempts to segment using the TinySegmenter algorithm.
#'
#' @name tokenize
#' @aliases tokenize
#' @rdname tokenize
#' @docType methods
#'
#' @usage \S4method{tokenize}{tiny_segmenter}(text)
#' @param \code{text} input single character vector
#' @return
#'  \describe{segment Japanese text}
#' @examples
#' library(rTinySegmenter)
#' segmentr <- tiny_segmenter$new(text = "私の名前は中野です")
#' tokenized_text <- segmentr$tokenize()
#' # feature
#' segmentr$model_attr$unit
#' # feature value
#' segmentr$model_attr$score
NULL
