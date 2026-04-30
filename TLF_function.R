
# ===============================
# Paths
# ===============================
tab_path  <- file.path(study_path, "dev", "data","tables")
lis_path  <- file.path(study_path, "dev", "data","listings")
fig_path  <- file.path(study_path, "dev", "data","figures")

tab_patho  <- file.path(study_path, "dev", "output","tables")
lis_patho  <- file.path(study_path, "dev", "output","listings")
fig_patho  <- file.path(study_path, "dev", "output","figures")

title_path <- file.path(study_path, "doc", "Specifications","TITLES.xlsx")

# ===============================
# Create ONLY two envs
# ===============================
create_env("table", tab_path)
create_env("listing", lis_path)
create_env("figure", fig_path)

TLF_output <- function(data = NULL,
                       tlf_name = NULL,
                       target = NULL,
                       type = NULL,
                       n_col = NULL,
                       groups = NULL,
                       by_vars = NULL,
                       bl_vars = NULL,
                       max_rows = 25,
                       db_vars = NULL,
                       cont = "N",
                       cont_vars = NULL,
                       cont_ord = NULL,
                       sort_vars = NULL) {  
  
  tlf_name <- tolower(tlf_name)
  data <- clean_names(data)
  
  if (!is.data.frame(data) & type %in% c("table", "listing")) stop("Inputted argument is not a dataset.")
  
  target <- tolower(target)
  if (!target %in% c("dev", "val")) stop("`target` must be either 'dev' or 'val'")
  
  type <- tolower(type)
  if (!type %in% c("table", "listing")) stop("`type` must be either 'table' or 'listing' or 'figure'")
  
  if (is.null(n_col)) stop("Input number of columns.")
  
  if (cont == "Y" & (missing(cont_vars) | missing(cont_ord))) stop("Input variable or order variable for `Continue` record.")
  
  if (missing(sort_vars)) stop("Input sort variables for data.")
  
  if (target == "dev") {
    if (type == "table") { env <- table; path <- tab_path; patho <- tab_patho }
    if (type == "listing") { env <- listing; path <- lis_path; patho <- lis_patho }
    if (type == "figure") { env <- figure; path <- fig_path; patho <- fig_patho }
  } else {
    if (type == "table") { env <- vtable; path <- vtab_path }
    if (type == "listing") { env <- vlisting; path <- vlis_path }
    if (type == "figure") { env <- vfigure; path <- vfig_path }
  }
  
  # -------------------------------
  # Strip spaces in display columns (alignment issue)
  # -------------------------------
  
  file_xpt <- file.path(path, paste0(tlf_name, ".xpt"))
  
  outdt <- data %>% arrange(across(all_of(sort_vars))) %>% 
    select(matches("^col", perl = TRUE))
  
  write_xpt(outdt, file_xpt, version = 8)
  
  outdt[do.call(cbind, lapply(outdt, "%in%", c("")))] <- NA
  assign(tlf_name, outdt, envir = env)
  
  message("TLF: ", toupper(tlf_name), " written to ", path, " and environment updated.")
  
  tt <- read_excel(title_path, sheet = tolower(type)) %>% clean_names() %>%
    filter(tolower(name) == tolower(tlf_name)) %>%
    mutate(
      pop = recode(toupper(pop),
                   "ALL"  = "All Subjects",
                   "RAND" = "Randomized Population",
                   "SAF"  = "Safety Population",
                   "FAS"  = "FAS Population",
                   .default = NULL),
      foot = gsub("\\|", "\n", foot)
    )
  
  title <- paste(sep = "\n",
                 paste(sep = " ", str_to_title(type), tt$number),
                 tt$title,
                 tt$pop)
  
  footn <- gsub("NA\n", "", 
                paste(sep = "\n", 
                      paste0("\\ ", tt$foot), 
                      tt$source, 
                      paste0("Program Source: ", gsub("_", ".", tlf_name), "     Generated: ", Sys.Date())))
  
  # ==========================================================
  # HEADER POSITIONING (EDIT ONLY THESE 2 CONSTANTS)
  # ==========================================================
  HEADER_LEFT_INDENT_TWIPS <- 240     # moves study_neame block right/left (1440 twips = 1 inch)
  HEADER_RIGHT_TAB_TWIPS   <- 12600   # moves "Page x of y" left/right (bigger = more right)
  
  # How to change:
  # - Move study_name more RIGHT: increase HEADER_LEFT_INDENT_TWIPS (e.g., 180, 240)
  # - Move study_name more LEFT : decrease it (e.g., 60, 0)
  # - Move Page more RIGHT   : increase HEADER_RIGHT_TAB_TWIPS (e.g., 11200)
  # - Move Page more LEFT    : decrease it (e.g., 10200)
  # ==========================================================
  
  hdr_text <- paste0(
    "study_name\\line ",
    "Protocol Number: study_name Science\\tab ",
    "Page \\pagenumber of \\pagefield"
  )
  
  # helper: inject tab stop + indent into header paragraph
  .apply_header_tabs <- function(rtf_txt) {
    
    # Add left indent + right-aligned tab stop inside header paragraph definition
    rtf_txt <- gsub(
      "(\\\\header[^\\{]*\\{\\\\pard)",
      paste0("\\1\\\\li", HEADER_LEFT_INDENT_TWIPS, "\\\\ri0 \\\\tqr\\\\tx", HEADER_RIGHT_TAB_TWIPS, " "),
      rtf_txt,
      perl = TRUE
    )
    
    # Clean any accidental spacing around tab markers
    rtf_txt <- gsub("\\\\tab\\s+", "\\\\tab ", rtf_txt)
    
    rtf_txt
  }
  
  
  if ((tolower(type) == "table" | tolower(type) == "listing") & target == "dev") {
    
    mcr_dt <- data
    
    mcr_dt <- mcr_dt %>% arrange(across(all_of(bl_vars))) %>%
      group_by(across(all_of(bl_vars))) %>%
      group_modify(~ add_blank_row(.x)) %>% ungroup()
    
    mcr_dt <- mcr_dt[do.call(order, mcr_dt[by_vars]), ]
    
    mcr_dt$grp_id <- dense_rank(mcr_dt[by_vars])
    grp_sizes <- as.data.frame(table(mcr_dt$grp_id))
    
    grp_sizes <- grp_sizes %>% 
      mutate(rowsum = cumsum(Freq),
             pagex = ceiling(rowsum / max_rows),
             grp_id = as.integer(Var1)
      )
    
    
    view(grp_sizes)
    
    mcr_dt <- left_join(mcr_dt, 
                        select(grp_sizes, grp_id, pagex), 
                        by = "grp_id")
    
    if (!is.null(db_vars)) {
      
      db_vars2 <- c("pagex", db_vars)
      
      mcr_dt$db_id <- dense_rank(interaction(mcr_dt[db_vars2], drop = TRUE))
      
      mcr_dt <- mcr_dt %>%
        group_by(pagex, db_id) %>%
        mutate(across(all_of(db_vars), ~ if_else(row_number() == 1, ., NA_character_))) %>%
        ungroup()
    }
    
    if (cont == "Y") {
      cont_ord <- ensym(cont_ord)
      cont_vars <- ensym(cont_vars)
      
      mcr_dt <- mcr_dt %>% mutate(!!cont_ord := !!cont_ord * 10)
      
      continue <- mcr_dt %>%
        group_by(pagex) %>%
        filter(row_number() == 1 & trimws(col1) != trimws(!!cont_vars) ) %>%
        mutate(col1 = paste0(!!cont_vars, " (Con't)"),
               !!cont_ord := !!cont_ord - 5) %>%
        ungroup()
      
      mcr_dt <- bind_rows(mcr_dt, continue)
    }
    
    mcr_dt <- arrange(mcr_dt, pagex, across(all_of(sort_vars)))
    
    rtf_dat <- select(mcr_dt, pagex, matches("^col|^ord", perl = TRUE))
    
    View(rtf_dat)
    # ==========================================================
    # PAGINATION (your chunking approach)
    # ==========================================================
    idx <- mcr_dt$pagex
    pages <- split(mcr_dt, idx)
    
    tmp_files <- character(length(pages))
    
    for (p in seq_along(pages)) {
      part <- pages[[p]]
      tmp_files[p] <- tempfile(fileext = ".rtf")
      
      page_tbl <- part %>% select(matches("^col", perl = TRUE)) %>% 
        rtf_page(
          orientation = "landscape",
          width = 11,
          height = 8.5,
          margin = c(1.0, 1.0, 0.8, 0.65, 0.45, 0.45),
          nrow = 28,
          border_first = "single",
          border_last = ""
        ) %>%
        rtf_page_header(
          text = hdr_text,
          text_justification = "l",
          text_font_size = 9,
          text_space_before = 0,
          text_space_after = 0
        ) %>%
        rtf_title(
          title = title,
          text_format = "b",
          text_font_size = 9,
          text_justification = "c"
        )
      
      for (i in seq_along(groups)) {
        rule <- c()
        for (j in seq_along(groups[[i]])) {
          if (j == 1) {
            gr <- groups[[i]][[j]]
            for (n in 0:str_count(groups[[i]][[j]], "\\|") + 1) {
              rule[n] <- ifelse(scan_r(groups[[i]][[j]], n, "\\|") == " ", "", "single")
            }
          } else if (j == 2) {
            grn <- groups[[i]][[j]]
          } else if (j == 3) {
            grj <- groups[[i]][[j]]
          }
        }
        
        if (i == 1 & length(groups) > 1) rule[1] <- ""
        
        grn_wide <- grn
        if (length(grn_wide) >= 2) grn_wide[-1] <- grn_wide[-1] * 1.15
        
        page_tbl <- page_tbl %>%
          rtf_colheader(
            colheader = as.character(gr),
            col_rel_width = grn_wide,
            text_justification = grj,
            border_left = NULL,
            border_right = NULL,
            border_top = if (i == 1) "single" else NULL,   # line above treatment columns
            border_bottom = rule,
            border_width = 1
          )
      }
      
      page_tbl <- page_tbl %>%
        rtf_body(
          col_rel_width = grn_wide,
          text_justification = grj,
          border_left = NULL,
          border_right = NULL,
          border_top = NULL,
          border_bottom = NULL,
          border_first = NULL,
          border_last = NULL,
          border_width = 1
        ) %>%
        rtf_footnote(
          footnote = footn,
          text_justification = "l",
          text_font_size = 9,
          border_left = NULL,
          border_right = NULL,
          border_top = "single",   # line between table and footnote
          border_bottom = NULL
        ) %>%
        rtf_encode()
      
      
      page_tbl <- .apply_header_tabs(page_tbl)
      
      writeLines(page_tbl, tmp_files[p])
    }
    
    out_file <- file.path(patho, paste0(tlf_name, ".rtf"))
    
    assemble_rtf(input = tmp_files, output = out_file, landscape = TRUE)
    
    final_txt <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
    
    writeLines(final_txt, out_file)
    
    message("RTF file ouputted.")
  }
  
  invisible(NULL)
}

F_output <- function(data = NULL,
                     fig_vector = NULL,
                     tlf_name = NULL,
                     target = NULL,
                     n_col = NULL,
                     sort_vars = NULL
) {
  
  tlf_name <- tolower(tlf_name)
  data <- clean_names(data)
  
  if (!is.data.frame(data)) stop("Inputted argument is not a dataset.")
  
  target <- tolower(target)
  if (!target %in% c("dev", "val")) stop("`target` must be either 'dev' or 'val'")
  
  if (is.null(n_col)) stop("Input number of columns.")
  
  if (missing(sort_vars)) stop("Input sort variables for data.")
  
  if (target == "dev") { 
    env <- figure; path <- fig_path; patho <- fig_patho
  } else { 
    env <- vfigure; path <- vfig_path 
  }
  
  # data output ----
  file_xpt <- file.path(path, paste0(tlf_name, ".xpt"))
  
  outdt <- data %>% arrange(across(all_of(sort_vars))) %>% 
    select(matches("^col", perl = TRUE))
  
  write_xpt(outdt, file_xpt, version = 8)
  
  outdt[do.call(cbind, lapply(outdt, "%in%", c("")))] <- NA
  assign(tlf_name, outdt, envir = env)
  
  message("Figure : ", toupper(tlf_name), "data written to ", path, " and environment updated.")
  
  tt <- read_excel(title_path, sheet = "figure") %>% clean_names() %>%
    filter(tolower(name) == tolower(tlf_name)) %>%
    mutate(
      pop = recode(toupper(pop),
                   "ALL"  = "All Subjects",
                   "RAND" = "Randomized Population",
                   "SAF"  = "Safety Population",
                   "FAS"  = "FAS Population",
                   .default = NULL),
      foot = gsub("\\|", "\n", foot)
    )
  
  title <- paste(sep = "\n",
                 paste0("Figure ", tt$number),
                 tt$title,
                 tt$pop)
  
  footn <- gsub("NA\n", "", 
                paste(sep = "\n", 
                      paste0("\\ ", tt$foot), 
                      tt$source, 
                      paste0("Program Source: ", gsub("_", ".", tlf_name), "     Generated: ", Sys.Date())))
  
  HEADER_LEFT_INDENT_TWIPS <- 240     # moves "study_name" block right/left (1440 twips = 1 inch)
  HEADER_RIGHT_TAB_TWIPS   <- 12600   # moves "Page x of y" left/right (bigger = more right)
  
  hdr_text <- paste0(
    "study_name\\line ",
    "Protocol Number: study_name Science\\tab ",
    "Page \\pagenumber of \\pagefield"
  )
  
  # helper: inject tab stop + indent into header paragraph
  .apply_header_tabs <- function(rtf_txt) {
    
    # Add left indent + right-aligned tab stop inside header paragraph definition
    rtf_txt <- gsub(
      "(\\\\header[^\\{]*\\{\\\\pard)",
      paste0("\\1\\\\li", HEADER_LEFT_INDENT_TWIPS, "\\\\ri0 \\\\tqr\\\\tx", HEADER_RIGHT_TAB_TWIPS, " "),
      rtf_txt,
      perl = TRUE
    )
    
    # Clean any accidental spacing around tab markers
    rtf_txt <- gsub("\\\\tab\\s+", "\\\\tab ", rtf_txt)
    
    rtf_txt
  }
  
  # rtf output ----
  rtf_pages <- lapply(seq_along(fig_vector), function(i) {
    
    rtf_page(
      rtf_read_figure(fig_vector[[i]]),
      orientation = "landscape",
      width  = 11,
      height = 8.5,
      margin = c(1.0, 1.0, 0.8, 0.65, 0.45, 0.45)
    ) %>%
      rtf_page_header(
        text = hdr_text,
        text_justification = "l",
        text_font_size = 9,
        text_space_before = 0,
        text_space_after = 0
      ) %>%
      rtf_title(
        title = title,
        text_format = "b",
        text_font_size = 9,
        text_justification = "c"
      ) %>%
      rtf_footnote(
        footnote = footn,
        text_justification = "l",
        text_font_size = 9,
        border_left = NULL,
        border_right = NULL,
        border_top = "single",   # line between table and footnote
        border_bottom = NULL
      ) %>%
      rtf_encode(doc_type = "figure")
  })
  final_rtf <- do.call(c, rtf_pages)
  
  final_rtf <- .apply_header_tabs(final_rtf)
  
  rtf_start <- rtf_pages[[1]]$start
  rtf_end   <- rtf_pages[[1]]$end
  
  rtf_body_all <- paste0(
    vapply(seq_along(rtf_pages), function(i) {
      
      body_txt <- rtf_pages[[i]]$body
      
      # Add page break after every page except last
      if (i < length(rtf_pages)) {
        paste0(body_txt, "\n\\page\n")
      } else {
        body_txt
      }
      
    }, character(1)),
    collapse = "\n"
  )
  
  final_rtf_text <- paste0(
    rtf_start,
    "\n",
    rtf_body_all,
    "\n",
    rtf_end
  )
  
  writeLines(final_rtf_text, paste(sep = "/", fig_patho, paste0(tlf_name, ".rtf")), useBytes = TRUE)
  
  message("Figure file ouputted.")
  
  unlink(fig_vector) # remove temporary folder
}