# The Chase Algorithm (in R) Rich Cotler March 2018 A simple application to
# execute a basic Chase Algorithm given a premise of join and functional
# dependencies.  local general utility functions

growList <- function(mylist, lentry) {
  # Starts a new list or appends to an existing list, with a sensitivity to some
  # special handling when building a list of data frames/data tables Args: mylist:
  # the list variable to append a new item to lentry: the object to append to the
  # passed list Return: mylist: the passed list with the passed object appended
  if (length(mylist) > 0) {
    if (class(lentry) == "data.frame") {
      mylist <- append(mylist, list(lentry))
    } else {
      mylist <- append(mylist, lentry)
    }
  } else {
    mylist <- list(lentry)
  }
  return(mylist)
}

deleteLastListEntry <- function(mylist) {
  # Deletes the highest numbered entry from a list Args: mylist: the list variable
  # to shorten Return: mylist: the passed list shortened by one entry
  if (length(mylist) < 2) {
    mylist <- list()
  } else {
    mylist <- mylist[1:length(mylist) - 1]
  }
  return(mylist)
}
