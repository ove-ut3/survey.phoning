#' app_phoning
#'
#' @param tbl_users \dots
#' @param chemin_sqlite_suivi \dots
#' @param chemin_cron_suivi \dots
#' @param chemin_sqlite_suivi_launch \dots
#' @param tbl_vacataires \dots
#' @param chemin_sqlite_diplomes \dots
#' @param chemin_sqlite_diplomes_log \dots
#' @param type_suivi \dots
#' @param titre \dots
#' @param options \dots
#' @param connexion_limesurvey \dots
#'
#' @import shiny shinydashboard rhandsontable
#' @importFrom dplyr %>%
#' @export
app_phoning <- function(
  tbl_diplomes,
  chemin_sqlite_diplomes,
  tbl_users,
  chemin_sqlite_suivi,
  chemin_cron_suivi = NA_character_,
  chemin_sqlite_suivi_launch = NA_character_,
  tbl_vacataires = NULL,
  chemin_sqlite_diplomes_log = NA_character_,
  type_suivi = c("Message laissé", "RDV téléphonique fixé", "Pas de messagerie", "Invitation Linkedin", "Faux numéro", "Email de relance", "Autre"),
  titre = "Application de relance téléphonique",
  options = list("host" = "127.0.0.1", "port" = getOption("shiny.port")),
  connexion_limesurvey = list("lime_api" = NULL, "lime_username" = NULL, "lime_password" = NULL)
  ) {

  if (is.na(chemin_cron_suivi) & is.null(tbl_vacataires)) {
    stop("Au moins un des deux paramètres \"chemin_cron_suivi\" ou \"tbl_vacataires\" doit être saisi.", call. = FALSE)
  }

  ui <- dashboardPage(

    dashboardHeader(title = titre, disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(tags$style(".table{margin: 0 auto;}"),
                tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                            type="text/javascript"),
                includeScript(paste0(find.package("shinyauthr"), "/shiny-examples/shinyauthr_example/returnClick.js"))
      ),
      tags$script('window.onload = function() {
                  function fixBodyHeight() {
                  var el = $(document.getElementsByClassName("content-wrapper")[0]);
                  var h = el.height();
                  el.css("min-height", h + 50 + "px");
                  };
                  window.addEventListener("resize", fixBodyHeight);
                  fixBodyHeight();
};'),
    tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: Gainsboro !important;}')),

    tags$script("Shiny.addCustomMessageHandler('launch-modal', function(d) {$('#' + d).modal().focus();})"),
    tags$script("Shiny.addCustomMessageHandler('remove-modal', function(d) {$('#' + d).modal('hide');})"),

    tags$div(
      id = "chargement_application",
      class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
      tags$div(
        class="modal-dialog",
        tags$div(
          class = "modal-content",
          tags$div(class="modal-header", tags$h4(class="modal-title", "Chargement de l'application"))
        )
      )
    ),
    tags$div(
      id = "chargement_individu",
      class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
      tags$div(
        class="modal-dialog",
        tags$div(
          class = "modal-content",
          tags$div(class="modal-header", tags$h4(class="modal-title", "Chargement de l'individu"))
        )
      )
    ),
    tags$div(
      id = "sauvegarde",
      class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
      tags$div(
        class="modal-dialog",
        tags$div(
          class = "modal-content",
          tags$div(class="modal-header", tags$h4(class="modal-title", "Sauvegarde des données"))
        )
      )
    ),
    tags$div(
      id = "mailing",
      class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
      tags$div(
        class="modal-dialog",
        tags$div(
          class = "modal-content",
          tags$div(class="modal-header", tags$h4(class="modal-title", "Envoi du mail de relance"))
        )
      )
    ),

    shinyauthr::loginUI("login", title = "Veuillez vous identifier", user_title = "Identifiant", pass_title = "Mot de passe",
                        login_title = "Se connecter", error_message = "Identifiant ou mot de passe invalide!"),
    uiOutput("testUI"),
    HTML('<div data-iframe-height></div>')
      )
    )

  server <- function(input, output, session) {

    credentials <- callModule(shinyauthr::login, "login",
                              data = tbl_users,
                              user_col = user,
                              pwd_col = password,
                              log_out = reactive(logout_init()))

    logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))

    user_info <- reactive({credentials()$info})

    output$testUI <- renderUI({

      req(credentials()$user_auth)

      fluidPage(
        title = titre,
        theme = shinythemes::shinytheme("cerulean"),
        absolutePanel(
          width = "96%",
          fluidRow(

            column(7, offset = 0, style='padding-left: 30px; padding-right: 50px;',
                   fluidRow(div(DT::dataTableOutput("formations"), style = "font-size:90%")),
                   fluidRow(div(style = "height: 20px;", "")),
                   fluidRow(div(DT::dataTableOutput("individus"), style = "font-size:90%"))),

            column(5,
                   h3("Coordonnées"),
                   fluidRow(
                     column(4, textInput("tel_portable", "Tél. portable", "", width = '100%')),
                     column(4, textInput("tel_etudiant", "Tél. fixe", "", width = '100%')),
                     column(4, textInput("tel_parents", "Tél. parents", "", width = '100%'))
                   ),
                   fluidRow(column(12, textInput("email", "Email(s)", "", width = '100%'))),

                   h3("Suivi téléphonique"),
                   rHandsontableOutput("hot"),
                   div(br(), style = "font-size:10pt"),

                   fluidRow(
                     column(6,
                            p(strong("Liste noire (Ne plus contacter)")),
                            uiOutput("switch_emailstatus")
                     ),
                     column(6, uiOutput("button_sauver"))
                   ),

                   h3("Actions"),
                   fluidRow(
                     column(6, uiOutput("button_questionnaire")),
                     column(6, uiOutput("selectUI"))),
                   fluidRow(
                     column(6, uiOutput("button_linkedin")),
                     column(6, uiOutput("button_mailing"))
                   ),

                   div(br(), style = "font-size:20pt"),

                   h3("Coordonnées invalides"),
                   fluidRow(
                     column(2, textInput("tel_portable_invalide", "Tél. portable", "", width = '100%')),
                     column(2, textInput("tel_etudiant_invalide", "Tél. fixe", "", width = '100%')),
                     column(2, textInput("tel_parents_invalide", "Tél. parents", "", width = '100%')),
                     column(6, textInput("email_invalide", "Email(s)", "", width = '100%'))
                   )

            )

          )

        )

      )

    })

    observe({

      req(credentials()$user_auth)

      session$sendCustomMessage(type = 'launch-modal', "chargement_application") # launch the modal

      if (!file.exists(chemin_sqlite_diplomes)) {

        base_sqlite <- impexp::sqlite_create(chemin_sqlite_diplomes)

        impexp::sqlite_export(
          chemin_sqlite_diplomes,
          tbl_diplomes)

      }

      table_diplomes <- chemin_sqlite_diplomes %>%
        stringr::str_extract("([^/]+?)$") %>%
        tools::file_path_sans_ext()

      table_suivi <- chemin_sqlite_suivi %>%
        stringr::str_extract("([^/]+?)$") %>%
        tools::file_path_sans_ext()

      if (!file.exists(chemin_sqlite_suivi_launch)) {

        base_sqlite <- impexp::sqlite_create(chemin_sqlite_suivi_launch)

        impexp::sqlite_export(
          chemin_sqlite_suivi_launch,
          dplyr::tibble(
            vacataire = character(0),
            identifiant = character(0),
            date_heure = character(0)
          ))

      }

      #### Import des diplômés ####

      if (!is.na(chemin_cron_suivi)) {

        if (Sys.info()[1] == "Windows") {

          #file <- impexp::ssh_import_file(chemin_cron_suivi, ssh_key_file)
          file <- "cron_suivi.RData"
          cron_diplomes <- impexp::r_import(file)
          #suppression <- file.remove(file)

        } else if (Sys.info()[1] == "Linux") {

          cron_diplomes <- impexp::r_import(chemin_cron_suivi)

        }

        if (user_info()$user != "admin") {
          cron_diplomes <- dplyr::filter(cron_diplomes, vacataire == user_info()$user)
        }

        diplomes <- impexp::sqlite_import(chemin_sqlite_diplomes) %>%
          dplyr::select(-code_etape) %>%
          dplyr::right_join(cron_diplomes, by = "token") %>%
          dplyr::mutate(repondant = dplyr::if_else(completed == "Oui", 1, 0, 0),
                        npc = dplyr::if_else(optout == "Oui", 1, 0, 0))


      } else if (!is.null(tbl_vacataires)) {

        if (!any(sapply(connexion_limesurvey, is.null))) {
          options("lime_api" = connexion_limesurvey$lime_api)
          options("lime_username" = connexion_limesurvey$lime_username)
          options("lime_password" = connexion_limesurvey$lime_password)
        }

        key <- limer::get_session_key()

        if (user_info()$user != "admin") {
          tbl_vacataires <- dplyr::filter(tbl_vacataires, vacataire == user_info()$user)
        }

        tbl_vacataires <- dplyr::select(tbl_vacataires, code_etape = token, type_diplome, vacataire)

        code_enquete <- get_id_survey(unique(tbl_vacataires$type_diplome), session = FALSE)
        names(code_enquete) <- NULL

        attributes_enquete <- purrr::map(code_enquete, limer::get_attributes_descriptions) %>%
          unlist()
        attributes_enquete <- attributes_enquete[unique(names(attributes_enquete))]

        attributes_attendus <- c("Code étape")

        attributes_manquant <- setdiff(attributes_attendus, attributes_enquete)
        if (length(attributes_manquant) >= 1) {
          stop("Les attributs ", paste0(attributes_manquant, collapse = ", "), " sont manquants", call. = FALSE)
        }

        attributes <- names(attributes_enquete[which(attributes_enquete %in% attributes_attendus)])

        conditions <- split(tbl_vacataires$code_etape, tbl_vacataires$type_diplome)

        attribute_etape <- names(attributes_enquete)[which(attributes_enquete == "Code étape")]

        diplomes <- purrr::map2_df(
          code_enquete,
          conditions,
          ~ limer::get_participants(.x, aAttributes = as.list(c("emailstatus", "completed", attributes)), aConditions = stats::setNames(list(.y), attribute_etape)), .id = "id_join") %>%
          dplyr::as_tibble() %>%
          dplyr::left_join(dplyr::tibble(id_survey = code_enquete) %>%
                             dplyr::mutate(id_join = as.character(dplyr::row_number())),
                           by = "id_join") %>%
          dplyr::select(-id_join, -email, -tid, -id_survey) %>%
          dplyr::rename(prenom = firxtname, nom = lastname)

        names(diplomes)[which(names(diplomes) %in% attributes)] <- attributes_enquete[which(names(attributes_enquete) %in% attributes)]

        n_groupes_q <- code_enquete %>%
          purrr::map_int(
            ~ limer::call_limer("list_groups", list("iSurveyID" = .)) %>%
              dplyr::pull(gid) %>%
              unique() %>%
              length()
          ) %>%
          dplyr::tibble(n_groupes_q = .) %>%
          dplyr::mutate(id_survey = code_enquete)

        if (!file.exists(chemin_sqlite_diplomes)) {
          base_diplomes_relance_tel(chemin_sqlite_diplomes)
        }

        diplomes <- diplomes %>%
          dplyr::left_join(impexp::sqlite_import(chemin_sqlite_diplomes), by = "token") %>%
          dplyr::left_join(dplyr::select(tbl_vacataires, code_etape, vacataire), by = "code_etape") %>%
          dplyr::left_join(n_groupes_q, by = "id_survey") %>%
          dplyr::left_join(ip.limesurvey::reponses(code_enquete, perimetre = "incompletes") %>%
                             dplyr::select(token, page = lastpage),
                           by = "token") %>%
          dplyr::mutate(repondant = ifelse(completed != "N", 1, 0),
                        npc = ifelse(emailstatus == "OptOut", 1, 0),
                        taux_avancement = page / n_groupes_q)

      }

      diplomes <- diplomes %>%
        dplyr::select(type_diplome, code_composante, code_etape, lib_etape, ordre, id_survey, tid, identifiant = token, prenom, nom, sexe, age, profil_questionnaire, type_diplome_pre, npc, repondant, taux_avancement, vacataire, email, tel_portable, tel_etudiant, tel_parents, lien_limesurvey, lien_desinscription, email_invalide, tel_portable_invalide, tel_etudiant_invalide, tel_parents_invalide) %>%
        dplyr::arrange(nom, prenom)

      # Initialisation à 0 du bouton NPC pour éviter l'exécution du départ
      switch_npc <<- 0

      #### Base SQLite suivi ####

      if (!file.exists(chemin_sqlite_suivi)) {

        base_sqlite <- impexp::sqlite_create(chemin_sqlite_suivi)

        impexp::sqlite_export(
          chemin_sqlite_suivi,
          dplyr::tibble() %>%
            dplyr::mutate_at(c("Identifiant", "Num", "Date", "Type", "Commentaire", "Vacataire"), ~ character(0)) %>%
            dplyr::mutate_at("Num", as.integer))

      }

      if (!is.na(chemin_sqlite_diplomes_log) & !file.exists(chemin_sqlite_diplomes_log)) {
        creation <- impexp::sqlite_create(chemin_sqlite_diplomes_log)

        impexp::sqlite_export(chemin_sqlite_diplomes_log,
                              dplyr::tibble(identifiant = character(0),
                                            champ = character(0),
                                            valeur_old = character(0),
                                            valeur_new = character(0),
                                            date_heure = character(0),
                                            user = character(0)))
      }

      diplomes_synthese_suivi <- function(suivi_detail) {

        suivi_detail %>%
          dplyr::arrange(Identifiant, Num, Date) %>%
          dplyr::select(Identifiant, Date) %>%
          unique() %>%
          dplyr::mutate(date = lubridate::dmy_hms(Date),
                        date_suivi = lubridate::as_date(date)) %>%
          dplyr::group_by(Identifiant) %>%
          dplyr::summarise(n_suivi = dplyr::n_distinct(date_suivi),
                           date_dernier_suivi = max(date)) %>%
          dplyr::ungroup() %>%
          dplyr::select(identifiant = Identifiant, n_suivi, date_dernier_suivi)
      }

      diplomes_suivi <- impexp::sqlite_import(chemin_sqlite_suivi) %>%
        dplyr::semi_join(diplomes, by = c("Identifiant" = "identifiant")) %>%
        diplomes_synthese_suivi() %>%
        dplyr::left_join(diplomes, ., by = "identifiant") %>%
        dplyr::mutate(n_suivi = patchr::patch_vector(n_suivi, 0L),
                      n_suivi = dplyr::if_else(repondant == 1 | npc == 1, NA_integer_, n_suivi),
                      date_dernier_suivi = dplyr::if_else(repondant == 1 | npc == 1, lubridate::as_datetime(NA_character_), date_dernier_suivi))

      #### Table des formations ####

      formations_synthese_suivi <- function(suivi_detail) {

        formations_synthese_suivi <- suivi_detail %>%
          dplyr::group_by(code_etape, lib_etape, vacataire, ordre) %>%
          dplyr::summarise(diplomes = n(),
                           repondants = sum(repondant),
                           npc = sum(npc),
                           n_suivi = min(n_suivi, na.rm = TRUE),
                           date_dernier_suivi = suppressWarnings(min(date_dernier_suivi, na.rm = TRUE))) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(taux_reponse = repondants / diplomes,
                        reste = diplomes - repondants - npc,
                        reste = ifelse(reste < 0, 0, reste),
                        n_suivi = ifelse(n_suivi == Inf, 0, n_suivi)) %>%
          dplyr::filter(reste >= 1) %>%
          dplyr::mutate(annee = purrr::map_dbl(date_dernier_suivi, lubridate::year)) %>%
          dplyr::arrange(desc(n_suivi), date_dernier_suivi, taux_reponse)

        dplyr::bind_rows(
          tidyr::drop_na(formations_synthese_suivi, annee),
          dplyr::filter(formations_synthese_suivi, is.na(annee))
        ) %>%
          dplyr::arrange(ordre)

      }

      tbl_formations <- formations_synthese_suivi(diplomes_suivi)

      if (user_info()$user == "admin") {
        tbl_formations <- dplyr::select(tbl_formations, code_etape, c('Formation' = 'lib_etape', 'Diplômés' = 'diplomes', 'Répondants' = 'repondants', '%age' = 'taux_reponse', 'Reste' = 'reste', 'Suivis' = 'n_suivi', 'Date' = 'date_dernier_suivi', 'Vacataire' = 'vacataire'))
      } else {
        tbl_formations <- dplyr::select(tbl_formations, code_etape, c('Formation' = 'lib_etape', 'Diplômés' = 'diplomes', 'Répondants' = 'repondants', '%age' = 'taux_reponse', 'Reste' = 'reste', 'Suivis' = 'n_suivi', 'Date' = 'date_dernier_suivi'))
      }

      output$formations <- DT::renderDataTable({

        if (user_info()$user == "admin") {
          dom <- 'rft'
          scrollY <- '40vh'
        } else {
          dom <- 'rt'
          scrollY <- '42vh'
        }

        tbl_formations %>%
          dplyr::select(-code_etape) %>%
          dplyr::mutate_at(dplyr::vars(Suivis), as.factor) %>%
          DT::datatable(selection = list(mode = 'single', selected = 1),
                        rownames = FALSE,
                        options = list(
                          scrollY = scrollY,
                          pageLength = -1,
                          dom = dom,
                          autoWidth = TRUE,
                          language = list(search = "Recherche"),
                          columnDefs = list(list(width = '50%', targets = 0),
                                            list(className = 'dt-right', targets = 5))
                        )
          ) %>%
          DT::formatPercentage('%age', 1, dec.mark = ",") %>%
          DT::formatStyle(
            '%age',
            target = 'row',
            backgroundColor = DT::styleInterval(c(0.499999, 0.649999, 0.749999), c("rgb(251, 145, 131)", "rgb(255, 210, 128)", "rgb(191, 255, 128)", "rgb(0, 179, 0)"))
          ) %>%
          DT::formatDate('Date', method = 'toLocaleDateString', params = list('fr-FR'))

      })

      proxy_tbl_formations <- DT::dataTableProxy("formations")
      proxy_data_tbl_formations <- dplyr::tibble(code_etape = character(0),
                                                 n_suivi = integer(0),
                                                 date_dernier_suivi = lubridate::as_datetime(character(0)))

      #### Formation filtrée (reactive) ####

      formation_filtree <- eventReactive(input$formations_rows_selected, {

        diplomes %>%
          dplyr::filter(code_etape == tbl_formations$code_etape[input$formations_rows_selected]) %>%
          dplyr::filter(repondant == 0,
                        npc == 0)

      })

      #### Table des individus ####

      tbl_individus <- reactive({

        diplomes_suivi %>%
          dplyr::semi_join(formation_filtree(), by = "identifiant") %>%
          dplyr::select(identifiant, c('Prénom' = 'prenom', 'Nom' = 'nom', 'Sexe' = 'sexe', 'Age' = 'age', 'Profil' = 'profil_questionnaire', 'Pre' = 'type_diplome_pre', 'Suivis' = 'n_suivi', 'Date' = 'date_dernier_suivi', 'Avancement' = 'taux_avancement'))
        # identifiant gardé pour maj proxy

      })

      proxy_tbl_individus <- DT::dataTableProxy("individus")

      proxy_data_tbl_individus <- dplyr::tibble(identifiant = character(0),
                                                n_suivi = integer(0),
                                                date_dernier_suivi = lubridate::as_datetime(character(0)))

      output$individus <- DT::renderDataTable({

        session$sendCustomMessage(type = 'remove-modal', "chargement_application") # hide the modal programmatically

        tbl_individus() %>%
          dplyr::left_join(proxy_data_tbl_individus, by = "identifiant") %>%
          dplyr::mutate(Suivis = ifelse(!is.na(n_suivi), n_suivi, Suivis),
                        Date = dplyr::if_else(!is.na(n_suivi), date_dernier_suivi, Date)) %>%
          dplyr::select(-identifiant, -n_suivi, -date_dernier_suivi) %>%
          DT::datatable(selection = list(mode = 'single', selected = 1),
                        rownames = FALSE,
                        options = list(
                          ordering = FALSE,
                          scrollY = '42vh',
                          #pageLength = -1,
                          dom = 'Brtp',
                          language = list(paginate = list(previous = "Précédent", `next` = 'Suivant'))
                        )
          ) %>%
          DT::formatPercentage('Avancement', 1, dec.mark = ",") %>%
          DT::formatDate('Date', method = 'toLocaleDateString', params = list('fr-FR'))

      })

      #### Individu filtré (reactive) ####

      individu_filtre <- eventReactive({
        input$formations_rows_selected
        input$individus_rows_selected
      }, {

        individu_filtre <- formation_filtree()

        if (input$individus_rows_selected <= nrow(individu_filtre)) {

          individu_filtre <- dplyr::filter(individu_filtre, identifiant == individu_filtre$identifiant[input$individus_rows_selected])

        } else {

          individu_filtre <- dplyr::filter(individu_filtre, identifiant == individu_filtre$identifiant[1])

        }

        sqlite <- impexp::sqlite_import(chemin_sqlite_diplomes)

        individu_filtre <- dplyr::select(individu_filtre, which(!names(individu_filtre) %in% names(sqlite))) %>%
          dplyr::left_join(sqlite, by = c("identifiant" = "token"))

        individu_filtre
      })

      deux_derniers_individus_filtres <- reactiveVal(diplomes[1, ])

      observeEvent({
        input$formations_rows_selected
        input$individus_rows_selected
      }, priority = 2, {

        newValue <- individu_filtre()

        newValue <- dplyr::bind_rows(deux_derniers_individus_filtres(), newValue) %>%
          tail(2)

        deux_derniers_individus_filtres(newValue)
      })

      #### Affichage des coordonnées ####

      observeEvent({
        input$formations_rows_selected
        input$individus_rows_selected
      }, {

        attach(individu_filtre(), warn.conflicts = FALSE)

        purrr::walk2(stringr::str_subset(names(session$input), "^(email|tel)"),
                     purrr::map_chr(stringr::str_subset(names(session$input), "^(email|tel)"), get),
                     ~ updateTextInput(session, .x, value = .y))

        # Actions

        output$switch_emailstatus <- renderUI({
          shinyWidgets::switchInput(
            inputId = "npc",
            value = ifelse(individu_filtre()$npc == 1, TRUE, FALSE),
            onLabel = "Oui", offLabel = "Non",
            onStatus = "danger", offStatus = "success", inline = TRUE)
        })

        choix_email <- stringr::str_split(email, ";") %>%
          .[[1]] %>%
          trimws()

        output$selectUI <- renderUI({
          selectInput("choix_email", NULL, choices = choix_email, width = '100%')
        })

        output$button_questionnaire <- renderUI({
          actionButton("questionnaire", "Commencer le questionnaire", icon = icon("pencil-square-o"), onclick = paste0("window.open('", paste0("https://enquetes.univ-tlse3.fr/index.php/", individu_filtre()$id_survey, "?token=", individu_filtre()$identifiant), "', '_blank')"))
        })

        output$button_sauver <- renderUI({
          actionButton("sauver", "Sauvegarder", icon = icon("save"))
        })

        output$button_linkedin <- renderUI({
          actionButton("linkedin", "Recherche Linkedin", icon = icon("linkedin"), onclick = paste0("window.open('", paste0("https://www.linkedin.com/search/results/index/?keywords=", individu_filtre()$prenom, "%20", individu_filtre()$nom, "%20toulouse"), "', '_blank')"))
        })

        output$button_mailing <- renderUI({
          actionButton("mailing", "Envoyer un mail de relance", icon = icon("envelope-o"))
        })

        detach()

      })

      #### Suivi ####

      output$hot <- renderRHandsontable({

        DF <- get_suivi_individu(individu_filtre()$identifiant, chemin_sqlite_suivi)

        if (nrow(DF) >= 1) {
          DF <- dplyr::mutate(DF, Date = stringr::str_extract(Date, "([\\d/]+)"))
        }

        if (nrow(DF) == 0 || !is.na(tail(DF, 1)$Type) || !is.na(tail(DF, 1)$Commentaire)) {

          DF <- DF %>%
            dplyr::add_row(Identifiant = individu_filtre()$identifiant,
                           Num = as.integer(nrow(DF) + 1),
                           Date = lubridate::today() %>%
                             format("%d/%m/%Y"))
        }

        if (user_info()$user == "admin") {
          colWidths <- c(0.1, 0.1, 100, 200, 350, 100)
        } else {
          colWidths <- c(0.1, 0.1, 100, 200, 400, 0.1)
        }

        DF %>%
          rhandsontable(rowHeaders = NULL, height = 250) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_rows(rowHeights = 35) %>%
          hot_cols(colWidths = colWidths, valign = "htMiddle") %>%
          hot_col(col = "Vacataire", type = "dropdown", source = c("alemouele", "ctintinaglia", "ddinguima", "jcommene", "mguerreiro", "plsutton", "rataayi", "sgil", "slaubies", "slenormand")) %>%
          hot_col(col = "Type", type = "dropdown", source = type_suivi) %>%
          hot_col("Date", type = "date", dateFormat = "DD/MM/YYYY", language = "fr-FR")

      })

      observeEvent(input$hot, {

        if (!is.null(input$hot)) {

          ajout <<- input$hot %>%
            hot_to_r() %>%
            dplyr::as_tibble() %>%
            dplyr::filter(!is.na(Type))  # pour le changement de ligne (d'individu)

          if (nrow(ajout) >= 1) {
            identifiant <- ajout$Identifiant[1]
            ajout$Identifiant <- ajout$Identifiant[1] # Insert row below
          } else {
            identifiant <- individu_filtre()$identifiant
          }

          if (!is.null(input$hot$changes)) {

            if (input$hot$changes$event == "afterRemoveRow") {

              impexp::sqlite_execute_sql(
                chemin_sqlite_suivi,
                paste0("DELETE FROM ", table_suivi, " WHERE Identifiant = '", identifiant, "' and Num = '", input$hot$changes$ind + 1,"';")
              )

              # Ré-initialisation de la numérotation si une ancienne ligne en début de tableau est supprimée

              verif_num <- get_suivi_individu(identifiant, chemin_sqlite_suivi)

              if (nrow(verif_num) >= 1) {

                if (!all(verif_num$Num == 1:nrow(verif_num))) {

                  impexp::sqlite_execute_sql(
                    chemin_sqlite_suivi,
                    paste0("DELETE FROM ", table_suivi, " WHERE Identifiant = '", identifiant, "';")
                  )

                  verif_num$Num <- 1:nrow(verif_num)

                  impexp::sqlite_append_rows(chemin_sqlite_suivi, verif_num)

                }

              }

            }

            if (!is.null(input$hot$changes$changes)) {

              if (input$hot$changes$changes[[1]][[1]] + 1 <= nrow(get_suivi_individu(identifiant, chemin_sqlite_suivi))) {

                value <- input$hot$changes$changes[[1]][[4]]

                if (names(ajout)[input$hot$changes$changes[[1]][[2]] + 1] == "Date") {
                  value <- paste(value, format(lubridate::now(tzone = "CET"), "%H:%M:%S"))
                }

                impexp::sqlite_execute_sql(
                  chemin_sqlite_suivi,
                  paste0("UPDATE ", table_suivi, " SET ", names(ajout)[input$hot$changes$changes[[1]][[2]] + 1], " = '", value,"' WHERE Identifiant = '", identifiant, "' and Num = '", input$hot$changes$changes[[1]][[1]] + 1,"';")
                )
              } else {

                # Ajout

                if (user_info()$user == "admin" & !is.na(ajout$Vacataire[1])) {
                  ajout_vacataire <- ajout$Vacataire[1]
                } else {
                  ajout_vacataire <- credentials()$info$user
                }

                ajout %>%
                  tail(1) %>%
                  dplyr::mutate(Num = nrow(ajout),
                                Date = paste(Date, format(lubridate::now(tzone = "CET"), "%H:%M:%S")),
                                Vacataire = ajout_vacataire) %>%
                  dplyr::anti_join(get_suivi_individu(identifiant, chemin_sqlite_suivi), by = "Num") %>%
                  impexp::sqlite_append_rows(chemin_sqlite_suivi, ., "suivi")

              }

            }

          }

        }

      })


      #### launch ####

      observeEvent(input$questionnaire, {

        impexp::sqlite_append_rows(
          chemin_sqlite_suivi_launch,
          dplyr::tibble(
            vacataire = credentials()$info$user,
            identifiant = individu_filtre()$identifiant,
            date_heure = format(lubridate::now(tzone = "CET"), "%d/%m/%Y %H:%M:%S")
          )
        )


      })

      #### NPC ####

      observeEvent(input$npc, { # , ignoreInit = TRUE (ne fonctionne pas...)


        if (input$npc == TRUE & switch_npc == 0) {

          session$sendCustomMessage(type = 'launch-modal', "sauvegarde") # launch the modal

          key <- limer::get_session_key()

          set_participant_properties <- limer::call_limer("set_participant_properties", params = list("iSurveyID" = individu_filtre()$id_survey, "aTokenQueryProperties" = individu_filtre()$tid, "aTokenData" = list("emailstatus" = "OptOut")))

          switch_npc <<- 1

          release <- limer::release_session_key()

          session$sendCustomMessage(type = 'remove-modal', "sauvegarde") # hide the modal programmatically

        } else if (input$npc == FALSE & switch_npc == 1) {

          session$sendCustomMessage(type = 'launch-modal', "sauvegarde") # launch the modal

          key <- limer::get_session_key()

          set_participant_properties <- limer::call_limer("set_participant_properties", params = list("iSurveyID" = individu_filtre()$id_survey, "aTokenQueryProperties" = individu_filtre()$tid, "aTokenData" = list("emailstatus" = "OK")))
          switch_npc <<- 0

          release <- limer::release_session_key()

          session$sendCustomMessage(type = 'remove-modal', "sauvegarde") # hide the modal programmatically

        }

      })

      #### Déselection ####

      deselect <- reactive({is.null(input$formations_rows_selected) | is.null(input$individus_rows_selected)})

      observe({

        if (deselect()) {

          # Pas d'individu sélectionné

          purrr::walk(stringr::str_subset(names(session$input), "^(email|tel)"), ~ updateTextInput(session, ., value = ""))

          output$selectUI <- renderUI({
            selectInput("choix_email", NULL, choices = "", width = '100%')
          })

          #supprimer npc, sauver, envoi mail
          output$switch_emailstatus <- renderUI({})
          output$button_mailing <- renderUI({})
          output$button_sauver <- renderUI({})

        }

      })

      #### Sauvegarde (et passer au suivant) ####

      observeEvent(input$sauver, {

        if (!is.null(input$hot)) {

          inputs <- dplyr::tibble(champ = names(input),
                                  input = purrr::map(names(input), ~ input[[.]])) %>%
            dplyr::filter(stringr::str_detect(champ, "^(email|tel)") | champ == "emailstatus") %>%
            dplyr::mutate(input = purrr::map(input, ~ ifelse(length(.) == 0, NA_character_, .)) %>%
                            purrr::map_chr(1) %>%
                            dplyr::na_if(""))

          dernier_indivivu_charge <- deux_derniers_individus_filtres()[2, ]

          maj <- tidyr::gather(dernier_indivivu_charge, "champ", "valeur", -identifiant)  %>%
            dplyr::right_join(inputs, by = "champ") %>%
            dplyr::filter(purrr::map2_lgl(valeur, input, ~ !.x %in% .y | !.y %in% .x))

          if (nrow(maj) >= 1 & !is.null(session$input$npc)) {

            session$sendCustomMessage(type = 'launch-modal', "sauvegarde") # launch the modal

            impexp::sqlite_execute_sql(
              chemin_sqlite_diplomes,
              paste0("UPDATE ", table_diplomes, " SET ", maj$champ, " = '", maj$input, "' WHERE token = '", maj$identifiant, "';")
            )

            if (!is.na(chemin_sqlite_diplomes_log)) {

              impexp::sqlite_append_rows(
                chemin_sqlite_diplomes_log,
                maj %>%
                  dplyr::mutate(date_heure = format(lubridate::now(tzone = "CET"), "%d/%m/%Y %H:%M:%S"),
                                user = credentials()$info$user) %>%
                  dplyr::select(identifiant, champ, valeur_old = valeur, valeur_new = input, date_heure, user))

            }

            session$sendCustomMessage(type = 'remove-modal', "sauvegarde") # hide the modal programmatically

          }

          # Proxy individu

          data_maj_individu <- ajout %>%
            dplyr::mutate(Date = paste(Date, format(lubridate::now(tzone = "CET"), "%H:%M:%S"))) %>%
            diplomes_synthese_suivi()

          if (nrow(data_maj_individu) == 0) {
            data_maj_individu <- dplyr::tibble(identifiant = dernier_indivivu_charge$identifiant,
                                               n_suivi = 0,
                                               date_dernier_suivi = lubridate::as_datetime(NA))
          }

          proxy_data_tbl_individus <<- patchr::anti_join_bind(data_maj_individu, proxy_data_tbl_individus, by = "identifiant")

          data <- tbl_individus() %>%
            dplyr::left_join(proxy_data_tbl_individus, by = "identifiant") %>%
            dplyr::mutate(Suivis = ifelse(!is.na(n_suivi), n_suivi, Suivis),
                          Date = dplyr::if_else(!is.na(n_suivi), date_dernier_suivi, Date))

          data %>%
            dplyr::select(-identifiant, -n_suivi, -date_dernier_suivi) %>%
            DT::replaceData(proxy = proxy_tbl_individus, rownames = FALSE, resetPaging = FALSE, clearSelection = "none")

          # Proxy formation
          data_maj_formation <- data %>%
            dplyr::select(identifiant, n_suivi = Suivis, date_dernier_suivi = Date) %>%
            dplyr::left_join(dplyr::select(diplomes, identifiant, code_etape, lib_etape, repondant, npc, ordre),
                             by = "identifiant") %>%
            dplyr::mutate(vacataire = credentials()$info$user) %>%
            formations_synthese_suivi() %>%
            dplyr::select(code_etape, n_suivi, date_dernier_suivi)

          proxy_data_tbl_formations <<- patchr::anti_join_bind(data_maj_formation, proxy_data_tbl_formations, by = "code_etape")

          data <- tbl_formations %>%
            dplyr::left_join(data_maj_formation, by = "code_etape") %>%
            dplyr::mutate(Suivis = ifelse(!is.na(n_suivi), n_suivi, Suivis),
                          Date = dplyr::if_else(!is.na(n_suivi), date_dernier_suivi, Date))

          data %>%
            dplyr::select(-code_etape, -n_suivi, -date_dernier_suivi) %>%
            dplyr::mutate_at(dplyr::vars(Suivis), as.factor) %>%
            DT::replaceData(proxy = proxy_tbl_formations, rownames = FALSE, clearSelection = "none")

        }

      })

      #### Sauvegarde d'email pour la relance ####

      observe({

        # Mailing liste email

        if (!is.null(input$email)) {
          email <- input$email %>%
            stringr::str_split(";") %>%
            .[[1]] %>%
            trimws()
        } else {
          email <- NA_character_
        }

        output$selectUI <- renderUI({
          selectInput("choix_email", NULL, choices = email, width = '100%')
        })

      })

      #### Mailing ####

      observeEvent(input$mailing, {

        session$sendCustomMessage(type = 'launch-modal', "mailing") # launch the modal

        ajout <- diplomes %>%
          dplyr::filter(identifiant == individu_filtre()$identifiant) %>%
          dplyr::select(type_diplome, code_composante) %>%
          dplyr::mutate(type_diplome = dplyr::recode(type_diplome, "Licence" = "L"),
                        code_composante = apogee::hier_composante_parent(code_composante))

        participants_mailing <- ip.limesurvey::mailing_participants(
          type_diplome = ajout$type_diplome,
          type_mailing = "questionnaire_diplomes",
          composante = ajout$code_composante,
          identifiant = individu_filtre()$identifiant
        ) %>%
          dplyr::mutate(email = input$choix_email)

        message <- ip.limesurvey::mailing_message(
          type_diplome = ajout$type_diplome,
          ip.donnees::annee_en_cours(),
          type_mailing = "questionnaire_diplomes",
          fichier_message = "relance_telephonique",
          composante = ajout$code_composante)

        expediteur <- ip.limesurvey::mailing_expediteur(
          type_mailing = "questionnaire_diplomes",
          composante = ajout$code_composante)

        limer::mailing(expediteur, participants_mailing, message$sujet, message$corps, sleep = 0, delete = TRUE)

        session$sendCustomMessage(type = 'remove-modal', "mailing") # hide the modal programmatically

      })

    })

  }

  shiny::shinyApp(ui, server, options = options)

}
