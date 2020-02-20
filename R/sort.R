#  sort - R package for fast multi-threaded sorting of vectors
#
#  Copyright (C) 2017-present, Mark AJ Klik
#
#  This file is part of the sort R package.
#
#  The sort R package is free software: you can redistribute it and/or modify it
#  under the terms of the GNU Affero General Public License version 3 as
#  published by the Free Software Foundation.
#
#  The sort R package is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
#  for more details.
#
#  You should have received a copy of the GNU Affero General Public License along
#  with the sort R package. If not, see <http://www.gnu.org/licenses/>.
#
#  You can contact the author at:
#  - sort R package source repository : https://github.com/fstpackage/sort



#' Sort integer vector
#'
#' Fast in-place sorting of an integer vector. Uses multithreading to increase sorting
#' speed.
#'
#' @param vec integer vector to be sorted
#' @param method character string specifying the algorithm used. Valid methods are
#' 'quick' for quicksort and 'radix' for radix sort.
#'
#' @return sorted vector
#' @export
msort <- function(vec, method = "radix") {

  if (is.logical(vec)) {
    sort:::fstsort_radix_logical(vec)
    return(vec)
  }

  if (is.integer(vec)) {
    if (method == "radix") {
      return(fstsort_radix_int(vec))
    } else if (method == "quick") {
      fstsort(vec)
      return(vec)
    } else {
      stop("please select a valid sorting method, allowed values are 'radix' and 'quick'")
    }
  }

  stop("Type not implemented (yet)")
}


#' Fast-sort a vector and it's ordering
#'
#' @param vec vector to be sorted
#' @param order a vector with integer values 1 to length(vec) that represents an ordering of vector _vec_.
#'
#' @return an integer vector containing the same values as _order_ but reordered in the the same way as _vec_.
#' @export
msort_order <- function(vec, order = NULL) {

  if (!is.logical(vec)) {
    stop("Use a logical")
  }

  if (!is.null(order)) {
    if (!is.integer(order)) {
      stop("Use an integer vector for parameter order")
    }

    if (length(order) != length(vec)) {
      stop("parameter order must have the same length as parameter vec")
    }
  }

  sort:::fstsort_radix_logical_order(vec, order)
}
