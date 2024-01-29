

#' Launch the SST test as a standalone
#'
#' @param item_bank
#' @param app_name
#' @param musicassessr_aws
#' @param get_p_id
#' @param data_collection_method
#' @param no_repetitions
#' @param max_goes_forced
#' @param no_items
#'
#' @return
#' @export
#'
#' @examples
SST_standalone <- function(item_bank = paste0("custom-assets/stimuli/berkowitz_musicxml/", list.files(system.file('www/stimuli/berkowitz_musicxml', package = 'SST'), pattern = "\\.musicxml$")),
                           app_name,
                           musicassessr_aws = FALSE,
                           get_p_id = FALSE,
                           data_collection_method = "audio",
                           no_repetitions = 5L,
                           max_goes_forced = FALSE,
                           no_items = 10L) {


  shiny::addResourcePath(
    prefix = "custom-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www/", package = "SST") # path to resource in your package
  )


  welcome_pg <- psychTestR::one_button_page("Welcome!")

  final_page_ui <- "You have finished!"

  item_bank_smp <- sample(item_bank, no_items)

  item_bank_with_reps <- rep(item_bank_smp, each = no_repetitions)

  names(item_bank_with_reps) <- paste0("Item ", rep(1:10, each = no_repetitions), " Repetition ", rep(1:5, no_items))



  items <- purrr::imap(item_bank_with_reps, function(item, name) {

    psychTestR::join(

      psychTestR::module(label = stringr::str_remove_all(name, " "),

        musicassessr::present_stimuli(stimuli_type = "musicxml_file",
                                      stimuli = item,
                                      page_label = "record_audio",
                                      page_title = name,
                                      answer_meta_data = tibble::tibble(
                                        item = basename(item),
                                        meta = name
                                      ),
                                      display_modality = "visual",
                                      page_type = "record_audio_page",
                                      get_answer = musicassessr::get_answer_pyin,
                                      show_record_button = TRUE,
                                      sheet_music_start_hidden = FALSE,
                                      sound_only_first_melody_note = TRUE,
                                      show_sheet_music = TRUE),

        psychTestR::elt_save_results_to_disk(complete = FALSE),

        psychTestR::NAFC_page(label = paste0("happy_", name),
                              prompt = "Were you happy with that take?",
                              choices = c("Yes", "No", "Skip")
                              ),

        psychTestR::elt_save_results_to_disk(complete = FALSE)

      )

    )
  })

  tl <- function() {

        psychTestR::join(
          items,
          psychTestR::elt_save_results_to_disk(complete = TRUE)
        )
  }

  musicassessr::make_musicassessr_test(

      welcome_page = welcome_pg,
      final_page = psychTestR::final_page(final_page_ui),
      elts = tl,
      title = "Sight Reading Test",
      admin_password = "demo",
      opt = musicassessr::musicassessr_opt(app_name = app_name,
                                           get_p_id = get_p_id,
                                           midi_input = data_collection_method == "midi",
                                           record_audio = data_collection_method == "audio",
                                           musicassessr_aws = musicassessr_aws,
                                           visual_notation = TRUE,
                                           setup_options = musicassessr::setup_pages_options(input_type = if(data_collection_method == "midi") "midi_keyboard" else "microphone",
                                                                                             headphones = TRUE,
                                                                                             skip_setup = "except_microphone",
                                                                                             get_instrument_range = FALSE,
                                                                                             SNR_test = FALSE,
                                                                                             concise_wording = TRUE)))

}

