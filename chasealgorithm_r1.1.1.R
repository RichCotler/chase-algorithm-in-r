# The Chase Algorithm (in R) Rich Cotler March 2018 A simple application to
# execute a basic Chase Algorithm given a premise of join and functional
# dependencies.

options(stringsAsFactors = FALSE)
library(BBmisc)
library(data.table)
library(dplyr)
library(DT)
library(lazyeval)
library(plyr)
library(purrr)
library(stringr)
library(shiny)
library(shinythemes)
library(shinyjs)

# General purpose functions:
source("localtools.R")

# Input validation functions:
source("castvalidate.R")

# Global variables NOTE: assignment to these variables requires '<<-'
jdcolcount.gl <- 5  # number of columns in the dependency tableau
jdrowcount.gl <- 2  # number of rows in the dependency tableau
currentrow.gl <- 1  # what version of the dependency tableau to display - not to exceed length(determinantlist.gl)
thesteps.gl <- list()  # list of data tables to store the step by step history of the dependency tableau
determinantlist.gl <- list()  # list of strings representing the determinants of each FD
dependentlist.gl <- list()  # list of strings representing the dependents of each FD
jdtable.gl <- data.table()  # data table with the active instance of the dependency tableau
fdout.gl <- list()  # list of strings with formatted FDs for display - determinant -> dependent

# Message templates
jdclickinfo <- c("Select columns for all Join Dependencies, click [save JD(s)]")
fdclickinfo <- c("Select columns and click [save FD] for each Functional Dependency")
initialjdstate <- c("Initial chase table values:")
intermediatejdstate <- c("Chase step %i, { %s }:")


# non-Shiny specialized functions

chaseItDown <- function() {
  # Performs the chase algorithm, applying each functional dependency to the
  # dependency tableau.  The initial dependency tableau and each step (except the
  # last) applying a functional dependency is stored as history in thesteps.gl.
  # The final state of the dependency tableau is left in jdtable.gl Args: None
  # Returns: None
  mincolname <- c("mini")
  colnamelist <- c(LETTERS[1:jdcolcount.gl])
  
  for (cidx in 1:length(determinantlist.gl)) {
    thesteps.gl <<- growList(thesteps.gl, jdtable.gl)
    detcols <- str_split(determinantlist.gl[[cidx]], "", simplify = TRUE)
    grpby <- lapply(detcols, as.symbol)
    dpendcol <- dependentlist.gl[[cidx]]
    dpendmin <- str_c("min(", dpendcol, ")")
    selstr <- str_replace(colnamelist, dpendcol, mincolname)
    selcols <- lapply(selstr, as.symbol)
    jdtable.gl <<- jdtable.gl %>% inner_join(jdtable.gl %>% group_by_(.dots = grpby) %>% 
      summarise_(.dots = setNames(dpendmin, mincolname))) %>% select_(.dots = selcols)
    colnames(jdtable.gl) <<- colnamelist
  }
}

dressItUp <- function(sourcetable) {
  # Formats a dependency tableau for display, setting basic DT render options and
  # appending a set of 'valueColumns' to the table to control background color of
  # each cell.  The new columns are hidden from display and control background
  # color highlighting of the visible columns with a styleEqual condition on the
  # hidden values.  The cells with a value 'x<N>' are assigned a background color,
  # cells with 'y<NN> are not.  Args: sourcetable: dependency tableau data table
  # (from thesteps.gl or jdtable.gl) to format Returns: displayjdtable: formatted
  # DT data table
  jdtablelist <- as.list(sourcetable[, 1:eval(jdcolcount.gl)])
  
  fmttablelist <- lapply(jdtablelist, function(x) {
    x <- str_sub(x, 1, 1)
  })
  fmttabledf <- data.frame(fmttablelist)
  fmtrowslist <- convertRowsToList(fmttabledf)
  
  rowsummary <- replicate(jdrowcount.gl, "x")
  
  for (ridx in 1:eval(jdrowcount.gl)) {
    if (str_detect(str_flatten(unlist(fmtrowslist[ridx]), collapse = ""), "y")) {
      rowsummary[ridx] <- c("y")
    }
  }
  
  fmttablelist$summary <- rowsummary
  
  xpandlist <- append(jdtablelist, fmttablelist)
  xpanddt <- setDT(xpandlist)
  displayjdtable <- datatable(xpanddt, selection = "none", editable = FALSE, options = list(paging = FALSE, 
    ordering = FALSE, searching = FALSE, info = FALSE, selection = "none", columnDefs = list(list(visible = FALSE, 
      targets = (eval(jdcolcount.gl + 1):eval((jdcolcount.gl * 2) + 1))))))
  
  displayjdtable <- formatStyle(displayjdtable, 1:eval(jdcolcount.gl), target = "cell", 
    valueColumns = eval(jdcolcount.gl + 1):eval(jdcolcount.gl * 2), textAlign = "center", 
    backgroundColor = styleEqual(c("x"), c("lightgreen")))
  displayjdtable <- formatStyle(displayjdtable, 0, target = "cell", valueColumns = eval((jdcolcount.gl * 
    2) + 1), backgroundColor = styleEqual(c("x"), c("lightgreen")))
  
  return(displayjdtable)
}

# Join Dependency related non-Shiny functions

initializeFromJDs <- function(cell_matrix) {
  # Runs when SAVE JD(S) button is pressed, creating the initial dependency tableau
  # data table based on column count, row count, and join dependencies specified
  # through the GUI.  The data table produced represents the state of the
  # dependency tableau before applying functional dependencies.  The initial value
  # for selected cells is 'x<column number>', non selected cells 'y<row
  # number><column number>' Example: the first row of the the JD R{ABEG} for
  # R{ABCDEFG} would have cell values: x1, x2, y13, y14, x5, y16, x7 Args:
  # cell_matrix: matrix of selected cell coordinates from Shiny Join Dependencies
  # input table Returns: data table with the initial state of the dependency
  # tableau
  jdwork <- setNames(data.table(matrix("", nrow = jdrowcount.gl, ncol = jdcolcount.gl)), 
    c(LETTERS[1:jdcolcount.gl]))
  for (trow in 1:jdrowcount.gl) {
    for (idx in 1:jdcolcount.gl) {
      nondepvalue <- str_c("y", as.character(trow), as.character(idx))
      jdwork[[trow, idx]] <- nondepvalue
    }
  }
  
  for (jdi in 1:nrow(cell_matrix)) {
    row.i <- cell_matrix[jdi, 1]
    col.i <- cell_matrix[jdi, 2]
    jdwork[[row.i, col.i]] <- str_c("x", as.character(col.i))
  }
  
  return(jdwork)
}

formatJDList <- function() {
  # creates formatted list of join dependencies in heath notation for display in
  # the sidebar Args: None Returns: heathjds: string of join dependencies in heath
  # notation
  heathjds <- c("{ ")
  for (jdi in 1:jdrowcount.gl) {
    jdstr <- c("")
    if (jdi > 1) {
      heathjds <- str_c(heathjds, ", ")
    }
    for (jix in 1:jdcolcount.gl) {
      if (str_sub(jdtable.gl[[jdi, jix]], end = -2) == c("x")) {
        jdstr <- str_c(jdstr, LETTERS[jix])
      }
    }
    heathjds <- str_c(heathjds, jdstr)
  }
  heathjds <- str_c(heathjds, " }")
  return(heathjds)
}

formatFDList <- function() {
  # creates formatted list of functional dependencies in heath notation for display
  # in the sidebar Args: None Returns: heathfds: string of functional dependencies
  # in heath notation
  heathfds <- c("")
  for (fdi in 1:length(determinantlist.gl)) {
    fdstr <- str_c(c("  "), fdout.gl[fdi])
    if (fdi < length(determinantlist.gl)) {
      fdstr <- str_c(fdstr, ", ")
    }
    heathfds <- paste(heathfds, fdstr, sep = "\n")
  }
  return(heathfds)
}

# Functional Dependency related non-Shiny functions

mapFDCols <- function(cell_matrix) {
  # Translates the determinant or dependent definition from the GUI (selected cells
  # in a data table) to its column name representation.  Example: if columns B and
  # F are selected, the function returns a string 'BF' Args: cell_matrix: matrix of
  # selected cell coordinates from Shiny determinant or dependent input table
  # Returns: fdcols: string containing a concatenation of the names of the selected
  # columns
  if (nrow(cell_matrix) > 1) {
    # sort the cell matrix so elements are not scrambled in the order they were
    # clicked
    cell_matrix <- cell_matrix[do.call(order, as.data.frame(cell_matrix)), ]
  }
  fdcols <- c("")
  for (cidx in 1:nrow(cell_matrix)) {
    fdcols <- str_c(fdcols, LETTERS[cell_matrix[cidx, 2]])
  }
  return(fdcols)
}

buildFDEntry <- function(cell_matrix, fdlist) {
  # calls functions to translate selected determinant or dependent to column name
  # value and appends the result to the end of the determinant or dependent list
  # Args: cell_matrix: matrix of selected cell coordinates from Shiny determinant
  # or dependent input table fdlist: either determinantlist.gl or dependentlist.gl
  # Returns: fdlist: list passed as argument with additional FD column name value
  # added to the end
  fdcols <- mapFDCols(cell_matrix)
  fdlist <- growList(fdlist, fdcols)
  return(fdlist)
}

### Shiny ui definition
source("chaseui.R")

### Shiny server function
server <- function(input, output, session) {
  
  # Shiny related input and display processing
  
  clearSBMessageTxt <- function() {
    # Clears general purpose message control in GUI sidebar Args: None Returns: None
    output$sbmessage <- renderText(c(""))
  }
  
  clickedJDCell <- function(cell_matrix) {
    # Shows or hides the SAVE JD(S) button during Join Dependency entry.  Since by
    # definition a join dependency must have more than one parameter, the button is
    # visible only when twp or more cells are selected in the dependency tableau.
    # While this doesn't validate the JD entry, it provides a minimum number of data
    # points required before passing the input information to the validation
    # functions.  Args: cell_matrix: matrix of selected cell coordinates from Shiny
    # Join Dependencies input table Returns: None
    if (nrow(cell_matrix) > 1) {
      # a JD has two specs
      show("savejds")
    } else {
      hide("savejds")
    }
  }
  
  hideJDTableControls <- function() {
    # Hides GUI elements used to specify the size of the dependency tableau Args:
    # None Returns: None
    hide("setcolcount")
    hide("setrowcount")
  }
  
  hideJDControls <- function() {
    # Hides GUI elements used to enter join dependencies Args: None Returns: None
    hide("savejds")
    hideJDTableControls()
  }
  
  controlSaveFDButton <- function() {
    # Controls if SAVE FD button is enabled during Functional Dependency entry.  The
    # button is enabled only when one or more cells are selected in the Determinant
    # table AND one cell is selected in the Dependent table.  Otherwise the button is
    # disabled.  Args: None Returns: None
    if (!is.null(input$determinant_cells_selected)) {
      if (nrow(input$determinant_cells_selected) < 1 | nrow(input$dependent_cells_selected) < 
        1) {
        disable("saveFD")
      } else {
        enable("saveFD")
      }
    }
  }
  
  controlFDDisplayAndButtons <- function() {
    # Controls display of the Functional Dependency list in the sidebar and the state
    # of FD entry action buttons based on whether a functional dependency has been
    # defined.  If there is one or more FD, the FD list is made visible, the DELETE
    # LAST FD button and RESET ALL FDS button are enabled, and the START THE CHASE!
    # button is made visible.  If there are no FDs saved, the FD list in the sidebar
    # and chase buton are made not visible and the DELETE LAST FD and RESET ALL FDS
    # buttons are disabled.  Args: None Returns: None
    if (length(determinantlist.gl) < 1) {
      disable("deletelastFD")
      disable("resetFDs")
      hide("runchase")
      hide("fdlist")
    } else {
      enable("deletelastFD")
      enable("resetFDs")
      show("runchase")
      show("fdlist")
    }
  }
  
  showFDControls <- function() {
    # Makes GUI elements used to enter functional dependencies visible.  Args: None
    # Returns: None
    show("saveFD")
    show("deletelastFD")
    show("resetFDs")
    show("fdinfotitle")
    show("determinant")
    show("dependent")
    controlSaveFDButton()
    controlFDDisplayAndButtons()
  }
  
  hideFDControls <- function() {
    # Hides GUI elemments used to enter functional dependencies Args: None Returns:
    # None
    hide("saveFD")
    hide("deletelastFD")
    hide("resetFDs")
    hide("fdinfotitle")
    hide("determinant")
    hide("dependent")
  }
  
  resetFDs <- function() {
    # Clears all saved functional dependencies when RESET ALL FDS button is pressed
    # Args: None Returns: None
    determinantlist.gl <<- list()
    dependentlist.gl <<- list()
    fdout.gl <<- list()
    controlFDDisplayAndButtons()
  }
  
  controlReviewButtons <- function() {
    # Controls if review navigation buttons are enabled or disabled, based on the
    # chase step being displayed.  If the last step is displayed, the NEXT and LAST
    # buttons are diabled, otherwise they are enabled If the first step is displayed,
    # the FIRST and PREVIOUS buttons are diabled, otherwise they are enabled Args:
    # None Returns: None
    if (currentrow.gl == length(determinantlist.gl)) {
      disable("reviewnext")
      disable("reviewlast")
    } else {
      enable("reviewnext")
      enable("reviewlast")
    }
    if (currentrow.gl == 0) {
      disable("reviewback")
      disable("reviewfirst")
    } else {
      enable("reviewback")
      enable("reviewfirst")
    }
  }
  
  showReviewControls <- function() {
    # Makes GUI elements used to navigate the chase history visible.  Args: None
    # Returns: None
    output$reviewmessage <- renderText("Use Previous/Next buttons to review steps in the Chase.")
    show("reviewmessage")
    show("reviewback")
    show("reviewnext")
    show("reviewfirst")
    show("reviewlast")
    controlReviewButtons()
  }
  
  hideReviewControls <- function() {
    # Hides GUI elements used to navigate the chase history visible.  Args: None
    # Returns: None
    hide("reviewmessage")
    hide("reviewback")
    hide("reviewnext")
    hide("reviewfirst")
    hide("reviewlast")
  }
  
  
  renderFormattedDT <- function(srctable, infotitle) {
    # Display formatted dependency tableau and table description title.  The
    # infotitle is displayed at the top of the window above the dependency tableau
    # and has instructions or a description of the table provided by the caller.
    # Args: srctable: data table (from thesteps.gl or jdtable.gl) to be formatted for
    # display infotitle: string containing description of the display table Returns:
    # None
    if (!is.null(srctable$A)) {
      srctable <- dressItUp(srctable)
    }
    output$catable <- renderDataTable(srctable, server = TRUE)
    output$cainfotitle <- renderText(infotitle)
  }
  
  renderReviewTable <- function() {
    # Passes the appropriate dependency tableau data table and title information
    # message describing the table (initial state, step #, or final result) to be
    # formatted and rendered Arg: None Returns: None
    if (currentrow.gl > 0) {
      infomessage <- sprintf(intermediatejdstate, currentrow.gl, fdout.gl[[currentrow.gl]])
    } else {
      infomessage <- initialjdstate
    }
    if (currentrow.gl == length(determinantlist.gl)) {
      infomessage <- str_c("Final Result - ", infomessage)
      renderFormattedDT(jdtable.gl, infomessage)
    } else {
      renderFormattedDT(thesteps.gl[[currentrow.gl + 1]], infomessage)
    }
    showReviewControls()
  }
  
  renderChaseTable <- function() {
    # Display empty reactive data table used to specify the join dependencies.  Args:
    # None Returns: None
    jdcolcount.gl <<- input$setcolcount
    jdrowcount.gl <<- input$setrowcount
    catable <- setNames(data.table(matrix("", nrow = jdrowcount.gl, ncol = jdcolcount.gl)), 
      c(LETTERS[1:jdcolcount.gl]))
    catable <- datatable(catable, class = "cell-border stripe", editable = FALSE, 
      options = list(paging = FALSE, ordering = FALSE, searching = FALSE, info = FALSE, 
        columnDefs = list(list(visible = FALSE, targets = c(0)))), selection = list(mode = "multiple", 
        target = "cell"))
    catable <- formatStyle(catable, 1:eval(jdcolcount.gl), height = 24)
    
    renderFormattedDT(catable, jdclickinfo)
    currentrow.gl <<- 1
    clearSBMessageTxt()
    resetFDs()
    hide("validationerrors")
    hideReviewControls()
    hideFDControls()
  }
  
  renderFDTables <- function() {
    # Display empty single row reactive data tables used to enter the determinant and
    # dependent columns for each functional dependency entry.  Args: None Returns:
    # None
    fd1table <- setNames(data.table(matrix("", nrow = 1, ncol = jdcolcount.gl)), 
      c(LETTERS[1:jdcolcount.gl]))
    output$determinant <- renderDataTable(fd1table, class = "cell-border stripe", 
      editable = FALSE, rownames = c("Determinant"), options = list(paging = FALSE, 
        searching = FALSE, info = FALSE, ordering = FALSE), selection = list(mode = "multiple", 
        target = "cell"))
    fd2table <- setNames(data.table(matrix("", nrow = 1, ncol = jdcolcount.gl)), 
      c(LETTERS[1:jdcolcount.gl]))
    ## restricting to a single dependent for now
    output$dependent <- renderDataTable(fd2table, class = "cell-border stripe", 
      editable = FALSE, rownames = c("Dependent"), options = list(paging = FALSE, 
        searching = FALSE, info = FALSE, ordering = FALSE), selection = list(mode = "single", 
        target = "cell"))
  }
  
  buildFDDisplay <- function() {
    # Format and store functional dependency entries for display.  The formatted FD
    # values are used in the FD list displayed in the sidebar and in the title
    # information message displayed above the dependency tableau in the chase history
    # display (review).  Args: None Return: None
    fddisplay <- list(determinantlist.gl, dependentlist.gl)
    fdout.gl <<- llply(lapply(transpose(fddisplay), function(x) {
      x <- str_c(x[1], " -> ", x[2])
    }))
    output$fdlist <- renderText({
      paste("Functional Dependencies:", str_c("{", formatFDList()), "}", sep = "\n")
    })
    controlFDDisplayAndButtons()
  }
  
  # Shiny event handlers
  
  observeEvent(input$setcolcount, {
    # react to Number of columns slider during join dependency entry rerenders the
    # reactive data table, resets selected cells
    renderChaseTable()
  })
  
  observeEvent(input$setrowcount, {
    # react to Number of rows slider during join dependency entry rerenders the
    # reactive data table, resets selected cells
    renderChaseTable()
  })
  
  observeEvent(input$catable_cells_selected, {
    # react to clicked cell during join dependency entry
    clickedJDCell(input$catable_cells_selected)
  })
  
  observeEvent(input$savejds, {
    # react to clicked SAVE JD(S) button, saves ALL join dependencies Runs validation
    # of join dependency input, returns with validation error message if there's an
    # error, creates and displays initial chase data table and functional dependency
    # entry GUI elements if the JDs pass validation.
    jdtable.gl <<- initializeFromJDs(input$catable_cells_selected)
    # validation exit WIP
    jderrormsgtable <- validateJDInput(jdtable.gl, jdcolcount.gl, jdrowcount.gl)
    if (nrow(jderrormsgtable$x$data) > 0) {
      output$validationerrors <- renderDataTable(jderrormsgtable)
      show("validationerrors")
    } else {
      renderFormattedDT(jdtable.gl, initialjdstate)
      output$jdlist <- renderText({
        paste("Join Dependencies:", formatJDList(), sep = "\n")
      })
      output$fdinfotitle <- renderPrint(fdclickinfo)
      renderFDTables()
      clearSBMessageTxt()
      hide("validationerrors")
      hideJDControls()
      showFDControls()
    }
  })
  
  observeEvent(input$determinant_cells_selected, {
    # react to determinant clicked cell during functional dependency entry
    controlSaveFDButton()
  })
  
  observeEvent(input$dependent_cells_selected, {
    # react to dependent clicked cell during functional dependency entry
    controlSaveFDButton()
  })
  
  observeEvent(input$saveFD, {
    # react to clicked SAVE FD button, saves a SINGLE functional dependency.  Runs
    # validation of functional dependency input, returns with validation error
    # message if there's an error, adds the functional dependency to the FD list if
    # it passes validation.
    determinantwork <- buildFDEntry(input$determinant_cells_selected, determinantlist.gl)
    dependentwork <- buildFDEntry(input$dependent_cells_selected, dependentlist.gl)
    # validation exit WIP
    fderrormsgtable <- validateFDInput(determinantwork, dependentwork)
    if (nrow(fderrormsgtable$x$data) > 0) {
      output$validationerrors <- renderDataTable(fderrormsgtable)
      show("validationerrors")
    } else {
      determinantlist.gl <<- determinantwork
      dependentlist.gl <<- dependentwork
      currentrow.gl <- length(determinantlist.gl)
      buildFDDisplay()
      hide("validationerrors")
      show("runchase")
      renderFDTables()
    }
  })
  
  observeEvent(input$deletelastFD, {
    # react to clicked DELETE LAST FD button, removes the last functional dependency
    # added to the FD list
    determinantlist.gl <<- deleteLastListEntry(determinantlist.gl)
    dependentlist.gl <<- deleteLastListEntry(dependentlist.gl)
    buildFDDisplay()
  })
  
  observeEvent(input$resetFDs, {
    # react to clicked RESET ALL FDS button, removes all functional dependencies from
    # the FD list
    resetFDs()  # in a function because there is also non UI code that needs it
    
  })
  
  observeEvent(input$runchase, {
    # react to START THE CHASE! button Runs the chase algorithm, applying each
    # functional dependency to the dependency tableau and saving the result in step
    # history.  The final result and history review GUI elements are rendered when
    # the chase is completed.
    hideFDControls()
    chaseItDown()
    hide("runchase")
    currentrow.gl <<- length(determinantlist.gl)
    renderReviewTable()
  })
  
  observeEvent(input$reviewback, {
    # react to PREVIOUS button, display chase step immediately before the current
    # step
    if (currentrow.gl > 0) {
      currentrow.gl <<- currentrow.gl - 1
      renderReviewTable()
    }
  })
  
  observeEvent(input$reviewnext, {
    # react to NEXT button, display chase step immediately after the current step
    if (currentrow.gl < length(determinantlist.gl)) {
      currentrow.gl <<- currentrow.gl + 1
      renderReviewTable()
    }
  })
  
  observeEvent(input$reviewfirst, {
    # react to FIRST button, display inital chase table step
    currentrow.gl <<- 0
    renderReviewTable()
  })
  
  observeEvent(input$reviewlast, {
    # react to LAST button, display last chase step with final dependency tableau
    # result
    currentrow.gl <<- length(determinantlist.gl)
    renderReviewTable()
  })
  
  observeEvent(input$aboutdiag, {
    # react to ABOUT button
    showModal(modalDialog(title = "About Chase Algorithm in R", HTML("Revision: 1.1.1 2018-03-06<br>Rich Cotler<br><br>
                                     If it's worth doing, it's worth doing in R!")))
  })
}

shinyApp(ui = ui, server = server)
