#' @title bibtex_2academic
#' @description import publications from a bibtex file to a hugo-academic website
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @modified Peter Paul Pichler (2019) <pichler@pik-potsdam.de>
#' @modified R.Andres Castaneda (2019) <acastanedaa@worldbank.org>

bibtex_2academic <- function(bibfile,
                             outfold,
                             abstract = FALSE,
                             overwrite = FALSE) {
  library(RefManageR)
  library(dplyr)
  library(stringr)
  library(anytime)
  library(tibble)
  library(toOrdinal)

  # Load data and transform to dataframe -----------------------------------------

  mypubs   <-
    ReadBib(bibfile, check = "warn", .Encoding = "UTF-8") %>%
    as.data.frame() %>%
    rownames_to_column() %>% # retain rownames (as labels for bibtex re-export)
    mutate_all(~ str_remove_all(., "[{}\"\\n]")) %>%   ### remove {}" from bibtext entries
    mutate_all(~ str_replace_all(., '\\\\%', '%')) %>%  ### some replace double escaped % for markdown
    mutate_all(~ str_replace_all(., '\\\\&', '&')) %>%   ### some replace double escaped % for markdown
    mutate_all(~ str_replace_all(., '\\\\\\$', '$')) %>%   ### some replace double escaped % for markdown
    mutate(rowname = str_replace_all(rowname, '/', '_'))


  # make bibtype the name of the type column (default for WriteBib)

  if (has_name(mypubs, "document_type") &
      !(has_name(mypubs, "bibtype"))) {
    mypubs <- mypubs %>% rename(bibtype = document_type)
  }

  # assign "categories" to the different types of publications
  mypubs   <- mypubs %>%
    dplyr::mutate(
      pubtype = dplyr::case_when(
        bibtype == "Article"          ~ "2",
        bibtype == "Article in Press" ~ "2",
        bibtype == "InProceedings"    ~ "1",
        bibtype == "Proceedings"      ~ "1",
        bibtype == "Conference"       ~ "1",
        bibtype == "Conference Paper" ~ "1",
        bibtype == "MastersThesis"    ~ "3",
        bibtype == "PhdThesis"        ~ "3",
        bibtype == "Manual"           ~ "4",
        bibtype == "TechReport"       ~ "4",
        bibtype == "Book"             ~ "5",
        bibtype == "InCollection"     ~ "6",
        bibtype == "InBook"           ~ "6",
        bibtype == "Misc"             ~ "0",
        TRUE                          ~ "0"
      )
    )

  # Define variables -------------------------------------------------------------

  create_md <- function(x) {
    x <- as.data.frame(t(x), stringsAsFactors = FALSE)
    # Date
    if (length(x[["year"]])) {
      date <- paste0(x[["year"]], "-01-01")
    } else {
      date <- "2999-01-01"
    }

    # Authors. Comma separated list, e.g. `["Bob Smith", "David Jones"]`.
    author <- str_replace_all(x["author"], " and ", "\", \"")
    author <-
      stringi::stri_trans_general(author, "latin-ascii")


    vars <- c(
      "title",
      "pubtype",
      "address",
      "url",
      "number",
      "institution",
      "journal",
      "volume",
      "pages",
      "doi",
      "abstract",
      "rowname",
      "year",
      "edition",
      "series",
      "booktitle",
      "isbn",
      "publisher"
    )

    for (z in vars) {
      if (length(x[[z]])) {
        y <- x[[z]]
      } else {
        y <- NA
      }
      assign(z, y)
    }

    # editor
    if (length(x[["editor"]])) {
      editor <- str_replace_all(x["editor"], " and ", "\", \"")
    } else {
      editor <- NA
    }

    # Publication according to type ------------------------------------------------

    # ARticles
    if (x[["pubtype"]] %in% c("2")) {

      publication <- journal
      if (!is.na(volume)) {
        publication <- paste0(publication,
                              ", (", volume, ")")
      }
      if (!is.na(number)) {
        publication <- paste0(publication,
                              ", ", number)
      }
      if (!is.na(pages))
        publication <- paste0(publication,
                              ", _pp. ", pages, "_")
      if (!is.na(doi))
        publication <- paste0(publication,
                              ", ", paste0("https://doi.org/",
                                           doi))
    } # end of articles

    # Reports
    if (x[["pubtype"]] %in% c("4")) {

      publication <- institution
      if (!is.na(number)) {
        publication <- paste0(publication,
                              "; ", number)
      }
      if (!is.na(address)) {
        publication <- paste0(publication,
                              ". ", address)
      }

    } # End of Reports

    # Books
    if (x[["pubtype"]] %in% c("5", "6")) {

      if(is.na(edition)) {
        edition <- "1st ed."
      } else {
        if (!is.na(as.numeric(edition))) {
          edition <- paste(toOrdinal(as.numeric(edition)), "ed")
        }
      }

      publication <- edition
      if (!is.na(address)) {
        publication <- paste0(publication,
                              ". ", address)
      }
      if (!is.na(publisher)) {
        publication <- paste0(publication,
                              ": ", publisher)
      }
    }  # End of books



    # create folders ---------------------------------------------------------------

    # name of folder
    if (!is.na(rowname)) {
      foldername <- rowname %>%
        str_remove_all(fixed(":"))
    } else {
      foldername <- paste(
        year, title %>%
          str_replace_all("[\\s/]", "_") %>%
          str_remove_all(fixed(":")) %>%
          str_sub(1, 20),
        sep = "_"
      )
    }

    # create directory for each publication
    dir.create(file.path(outfold, foldername), showWarnings = FALSE)
    filename = "index.md"

    # start writing
    outsubfold = paste(outfold, foldername, sep = "/")

    if (!file.exists(file.path(outsubfold, filename)) | overwrite) {
      fileConn <- file.path(outsubfold, filename)

      # Start writing ----------------------------------------------------------------
      write("+++", fileConn)

      # Title and date
      write(paste0("title = \"", title, "\""), fileConn, append = T)
      write(paste0("date = \"", anydate(date), "\""), fileConn, append = T)
      write(paste0("authors = [\"", author, "\"]"), fileConn, append = T)
      if (x[["pubtype"]] == 6 & !is.na(editor)) {
        write(paste0("editor = [\"", editor, "\"]"), fileConn, append = T)
      }

      if (x[["pubtype"]] == 6 & !is.na(series)) {
        write(paste0("series = \"", series, "\""), fileConn, append = T)
      }



      # Publication type. Legend:
      write(paste0("publication_types = [\"", pubtype, "\"]"),
            fileConn,
            append = T)

      write(paste0("publication = \"", publication, "\""),
            fileConn,
            append = T)
      write(paste0("publication_short = \"", publication, "\""),
            fileConn,
            append = T)

      # Abstract and optional shortened version.
      if (!is.na(abstract)) {
        write(paste0("abstract = \"", abstract, "\""),
              fileConn,
              append = T)
      } else {
        write("abstract = \"\"", fileConn, append = T)
      }
      write(paste0("abstract_short = \"", "\""), fileConn, append = T)

      # other possible fields are kept empty. They can be customized later by
      # editing the created md

      write("image_preview = \"\"", fileConn, append = T)
      write("selected = false", fileConn, append = T)
      write("projects = []", fileConn, append = T)
      write("tags = []", fileConn, append = T)

      #links
      if (!is.na(url)) {
        write(paste0("url_pdf = \"", url, "\""), fileConn, append = T)
      } else {
        write("url_pdf = \"\"", fileConn, append = T)
      }

      write("url_preprint = \"\"", fileConn, append = T)
      write("url_code = \"\"", fileConn, append = T)
      write("url_dataset = \"\"", fileConn, append = T)
      write("url_project = \"\"", fileConn, append = T)
      write("url_slides = \"\"", fileConn, append = T)
      write("url_video = \"\"", fileConn, append = T)
      write("url_poster = \"\"", fileConn, append = T)
      write("url_source = \"\"", fileConn, append = T)
      #other stuff
      write("math = true", fileConn, append = T)
      write("highlight = true", fileConn, append = T)
      # Featured image
      write("[header]", fileConn, append = T)
      write("image = \"\"", fileConn, append = T)
      write("caption = \"\"", fileConn, append = T)

      write("+++", fileConn, append = T)
    }
    # convert entry back to data frame
    df_entry <-
      as.data.frame(as.list(x), stringsAsFactors = FALSE) %>%
      column_to_rownames("rowname")

    # write cite.bib file to outsubfolder
    WriteBib(as.BibEntry(df_entry[1,]),
             paste(outsubfold, "cite.bib", sep = "/"))
  }
  # apply the "create_md" function over the publications list to generate
  # the different "md" files.
  apply(
    mypubs,
    FUN = function(x)
      create_md(x),
    MARGIN = 1
  )
}
my_bibfile <- "./mypubs.bib"
out_fold   <- "./content/publication"

bibtex_2academic(bibfile  = my_bibfile,
                 outfold   = out_fold,
                 abstract  = FALSE,
                 overwrite = TRUE)
