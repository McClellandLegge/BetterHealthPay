# chat    mesg    worksheet    journal phone    video    noshow
# live       X                             X        X         X (2 or more)
# you        X
# them       X            X          X

# Words from different subscribers are counted together. So, for example, if Subscriber A writes 500 words to you, and then Subscriber B writes 500 words, you would be paid $10 for this 1,000 word total.


library("R6")
library("futile.logger")
library("magrittr")


BHPatient <- R6Class(
    classname = "BHPatient"
  , public = list(
       initialize = function(log_level = "info") {
         self$setLogLevel(log_level)
         invisible(self)
       }
     , setLogLevel = function(log_level) {
       futile.logger::flog.threshold(log_level)
       invisible(self)
     }
     , audioMessage = function(words) {
       # For audio messages, words are transcribed and added to your subscriber's word count for the month.

       private$numberishCheck(words)

       purrr::walk(words, ~{
         private$interactions %<>% append(values = list(type = "mesg", medium = "audio", words = .))
         private$mesg$words$subscriber %<>% add(words)
       })

       invisible(self)
     }
     , liveSession = function(minutes, medium) {

       # Live sessions (chat, video, phone) are considered as 50 words per full minute.
       #     For video messages, you will be compensated at a rate of 50 words per minute.
       #     Live chat minutes are counted while either you or the subscriber are active/typing.

       private$mediumCheck(medium)
       private$numberishCheck(minutes)

       purrr::walk(minutes, ~{
         private$interactions %<>% append(values = list(type = "live", medium = medium, minutes = .))
         private$live$minutes %<>% add(50 * minutes)
       })

       invisible(self)
     }
     , journalEntry = function(words) {
       # Journal entries that are shared with you are added to your subscriber's word count for the month.
       private$numberishCheck(words)

       purrr::walk(words, ~{
         private$interactions %<>% append(values = list(type = "mesg", medium = "journal", words = .))
         private$mesg$words$subscriber %<>% add(words)
       })

       invisible(self)
     }
     , workSheet = function(words) {
       # When a subscriber fills a worksheet, this will be counted as 500 words plus any words written by the subscriber in the worksheet.

       private$numberishCheck(words)

       purrr::walk(words, ~{
         private$interactions %<>% append(values = list(type = "worksheet", words = .))
         private$mesg$words$subscriber %<>% add(words) %>% add(500L)
       })

       invisible(self)
     }
     , noShow = function() {
       # Client No Shows can be reported in your dashboard. The 2nd and each subsequent no show from the same client is compensated as a 30 min live session.

       prev_noshows <- purrr::keep(private$interactions, ~.$type == "noshow") %>%
         length()

       if (prev_noshows >= 1L) {
         private$interactions %<>% append(values = list(type = "noshow", medium = NA, minutes = 30))
         private$live$minutes %<>% add(30L)
       }

       invisible(self)
     }
     , messageBack = function(words) {
       private$numberishCheck(words)

       purrr::walk(words, ~{
         private$interactions %<>% append(values = list(type = "response", medium = "mesg", words = .))
         private$mesg$words$you %<>% add(words)
       })

       invisible(self)
     }
     , messaged = function(words) {
       private$numberishCheck(words)

       purrr::walk(words, ~{
         private$interactions %<>% append(values = list(type = "text", medium = "mesg", words = .))
         private$mesg$words$subscriber %<>% add(words)
       })

       invisible(self)
     }
   ) #/ public
   , private = list(
        interactions = list()
      , live = list(words = 0, minutes = 0)
      , mesg = list(words = list(you = 0, subscriber = 0))
      , mediumCheck = function(x) {

        if (!inherits(x, "character")) {
          rlang::abort("'medium' must be a character string")
        }

        if (length(medium) != 1L) {
          rlang::abort("'medium' must be of length 1")
        }

        allowed <- c("video", "chat", "phone")
        if (!medium %in% allowed) {
          mesg <- paste0(allowed, collapse = ", ")
          rlang::abort(paste("'medium' must be one of:", mesg))
        }
      }
      , numberishCheck = function(x) {
        if (!inherits(x, "integer") & !inherits(x, "numeric")) {
          rlang::abort("must be a numeric or integer value")
        }

        if (any(x <= 0L)) {
          rlang::abort("must be a positive value")
        }
      }
   )
  , active = list(
    owed = function() {
      # Total words per subscriber are counted up to a limit of 15,000 words (7,500 from each side) for each month.
      # Your words to each subscriber will be counted up to 2X of the words that your subscriber wrote to you.
      # For example, if you wrote to your subscriber 3,000 words and the subscriber wrote 1,000 words to you,
      # only 2,000 of your words will be counted. These limits are calculated monthly.

      # Payments are cut off on a monthly basis.
      your_words     <- private$mesg$words$you
      their_words    <- private$mesg$words$subscriber
      ttl_msg_words  <- your_words + their_words

      msg_words_lim  <- 2L * their_words
      flog.debug(paste("msg word limit", msg_words_lim))
      paid_msg_words <- ifelse(ttl_msg_words > msg_words_lim, msg_words_lim, ttl_msg_words)
      flog.debug(paste("msg words paid", paid_msg_words))

      live_words     <- private$live$minutes * 50L + private$live$words
      flog.debug(paste("live words", live_words))
      total_words    <- live_words + your_words + their_words
      flog.debug(paste("total words", total_words))
      running_words  <- paid_msg_words + live_words
      flog.debug(paste("running words", running_words))
      paid_words     <- min(15000L, running_words)

      list(
          total_words = total_words
        , paid_words  = paid_words
        , capped_msg  = max(your_words - msg_words_lim, 0)
        , perc_paid   = round(100 * paid_words / total_words, 2)
        , total_paid  = scales::dollar(total_words / 100L)
      ) %>% return()

    }
  )
)


