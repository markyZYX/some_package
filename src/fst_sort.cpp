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


#include <Rcpp.h>
#include <Rcpp.h>

#include <sort/sort.h>


SEXP bytes(SEXP int_vec) {
  uint64_t* vec_p = (uint64_t*) INTEGER(int_vec);
  
  vec_p[0] = 1;
  
  return int_vec;
}


SEXP fstmergesort(SEXP int_vec_left, SEXP int_vec_right) {

  int length_left = LENGTH(int_vec_left);
  int length_right = LENGTH(int_vec_right);

  int tot_size = length_left + length_right;

  // create result vector
  SEXP res_vec = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t) tot_size));

  // pointers
  int* left_p = INTEGER(int_vec_left);
  int* right_p = INTEGER(int_vec_right);
  int* res_p = INTEGER(res_vec);

  // do something here
  merge_sort_int(left_p, right_p, length_left, length_right, res_p);

  UNPROTECT(1);

  return res_vec;
}


SEXP fstsort_combined(SEXP int_vec) {

  int* vec = INTEGER(int_vec);
  int length = LENGTH(int_vec);

  if (length == 0) return int_vec;

  int pos = length / 2;

  // create result vector
  SEXP res_vec = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t) length));
  int* res_p = INTEGER(res_vec);

  // split in two and sort
  int pivot = (vec[0] + vec[pos - 1]) / 2;
  quick_sort_int(vec, pos, pivot);

  pivot = (vec[pos] + vec[length - 1]) / 2;
  quick_sort_int(&vec[pos], length - pos, pivot);

  merge_sort_int(vec, &vec[pos], pos, length - pos, res_p);

  UNPROTECT(1);

  return res_vec;
}


SEXP fstsort_radix_int(SEXP int_vec) {
  
  int* vec = INTEGER(int_vec);
  int length = LENGTH(int_vec);

  SEXP buffer_vec = PROTECT(Rf_allocVector(INTSXP, length));
  int* buffer = INTEGER(buffer_vec);

  radix_sort_int(vec, length, buffer);

  // uint64_t base_time = timings[0];

  UNPROTECT(1);

  return buffer_vec;
}


SEXP fstsort_radix_logical(SEXP logical_vec) {
  int* vec = LOGICAL(logical_vec);
  int length = LENGTH(logical_vec);
  
  try {
    radix_sort_logical(vec, length);
  }
  catch (int e) {
   std::cout << "An exception occurred. Exception Nr. " << e << '\n' << std::flush;
  }
  
  return logical_vec;
}


SEXP fstsort_radix_logical_order(SEXP logical_vec, SEXP order) {
  
  int* vec = LOGICAL(logical_vec);
  int* order_p = nullptr;
  int length = LENGTH(logical_vec);

  bool def_order = Rf_isNull(order) == 1;

  if (!def_order) {
    order_p = INTEGER(order);
  }

  // allocate output ordering vector  
  SEXP order_out = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t) length));
  int* order_out_p = INTEGER(order_out);

  try {
    radix_sort_logical_order(vec, length, order_p, order_out_p, def_order);
  }
  catch (int e) {
    std::cout << "An exception occurred: " << e << '\n' << std::flush;
  }

  UNPROTECT(1);

  return order_out;
}


SEXP fstsort(SEXP int_vec) {

  int* vec = INTEGER(int_vec);
  int length = LENGTH(int_vec);

  if (length < 3) {
    if (length == 2) {
      if (vec[0] > vec[1]) {
        int tmp = vec[0];
        vec[0] = vec[1];
        vec[1] = tmp;
      }
    }
    return int_vec;
  }

  // take center value as median estimate
  int pivot = (vec[0] + vec[(length - 1) / 2] + vec[length - 1]) / 3;
  quick_sort_int(vec, length, pivot);

  return int_vec;
}
