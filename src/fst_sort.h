/*
 sort - R package for fast multi-threaded sorting of vectors
 
 Copyright (C) 2017-present, Mark AJ Klik
 
 This file is part of the sort R package.
 
 The sort R package is free software: you can redistribute it and/or modify it
 under the terms of the GNU Affero General Public License version 3 as
 published by the Free Software Foundation.
 
 The sort R package is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 for more details.
 
 You should have received a copy of the GNU Affero General Public License along
 with the sort R package. If not, see <http://www.gnu.org/licenses/>.
 
 You can contact the author at:
 - sort R package source repository : https://github.com/fstpackage/sort
*/

#ifndef FST_SORT_H
#define FST_SORT_H


// [[Rcpp::export]]
SEXP bytes(SEXP int_vec);


// [[Rcpp::export]]
SEXP fstsort(SEXP int_vec);


// [[Rcpp::export]]
SEXP fstsort_radix_int(SEXP int_vec);


// [[Rcpp::export]]
SEXP fstsort_radix_logical(SEXP logical_vec);


// [[Rcpp::export]]
SEXP fstsort_radix_logical_order(SEXP logical_vec, SEXP order);
        

// [[Rcpp::export]]
SEXP fstmergesort(SEXP int_vec_left, SEXP int_vec_right);


// [[Rcpp::export]]
SEXP fstsort_combined(SEXP int_vec);

#endif  // FST_SORT_H
