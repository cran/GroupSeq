
# not used atm
add_custom_icons <- function()
{
    path <- system.file("icons", package = "GroupSeq")
    iconFiles <- list.files(path, full.names = TRUE, pattern = "*.png")
    iconNames <- tools::file_path_sans_ext(basename(iconFiles))

    #gWidgets2::addStockIcons(iconNames, iconFiles)
}

#' Graphical user interface.
#'
#' This function builds the main GUI that appears when `GroupSeq` is started.
#' @return Invoked for its side effects.
#' @import tcltk tcltk2
#' @noRd
gui <- function(root, tabs = c("Test parameters", "Boundaries"))
{
    # Init
    version <- utils::packageVersion(utils::packageName())

    # Make sure cleanup takes place also when user closes window via "X" button
    tkwm.protocol(root, "WM_DELETE_WINDOW", onQuit)
    tkwm.title(root, paste0("[New] - GroupSeq"))

    logofile <- system.file("img", "logo32.gif", package = "GroupSeq")
    tkimage.create("photo", "::image::logoIcon", file = logofile)
    tcl("wm", "iconphoto", root, "-default", "::image::logoIcon")

    # Menu
    menu <- tkmenu(root)
    tkconfigure(root, menu = menu)
    tkadd(menu, "cascade", label = "File", menu = create_file_menu(menu, root))
    tkadd(menu, "cascade", label = "Design", menu = create_design_menu(menu))
    tkadd(menu, "cascade", label = "Help", menu = create_help_menu(menu))

    # Main part
    fr.main <- tkframe(root, relief = "flat", borderwidth = 1)

    # Number of looks
    fr.looks <- tk2labelframe(fr.main, padding = 4, text = "Number of looks")
    nlook.lab <- .tklabel(fr.looks, text = "K = ", justify = "left")
    nlook.selector <- create_combobox(parent = fr.looks,
                                      param.name = "nlook",
                                      width = 4,
                                      choices = as.character(1:10),
                                      state = "readonly")
    tkgrid(nlook.lab, nlook.selector)

    # Notebook
    nb <- tk2notebook(parent = fr.main, tabs = tabs, padding = 5, width = 500, height = 200)
    pack_tabs(nb, tabs)

    tkgrid(fr.looks, sticky = "nw", padx = 8, pady = 8)
    tkgrid(nb, sticky = "w", pady = 2, padx = 3)
    tkgrid(fr.main)

    update_changed_parameters()
    root
}


# not used atm
show_about <- function(...)
{
    msg <- paste0("Computes probabilities related to group sequential ",
                  "designs for normally distributed test statistics. ",
                  "Enables to derive critical boundaries, power, drift, ",
                  "and confidence intervals of such designs. Supports the ",
                  "alpha spending approach by Lan-DeMets as well as the ",
                  "conditional rejection probability principle by Mueller ",
                  "and Schaefer.")
    #gWidgets2::gmessage(title = "About", message = msg, icon = "info")
}

