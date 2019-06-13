/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#define FREELIST_DEBUG 0
#if FREELIST_DEBUG
#include <stdio.h>
#endif

#include <string.h>

#include "caml/config.h"
#include "caml/custom.h"
#include "caml/freelist.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/memory.h"
#include "caml/major_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

/*************** declarations common to all policies ******************/

/* A block in a small free list is a [value] (integer representing a
   pointer to the first word after the block's header). The end of the
  list is NULL.
*/
#define Val_NULL ((value) NULL)

asize_t caml_fl_cur_wsz = 0;     /* Number of words in the free set,
                                    including headers but not fragments. */

value caml_fl_merge = Val_NULL;  /* Current insertion pointer.  Managed
                                    jointly with [sweep_slice]. */

/* Next in list */
#define Next_small(v) Field ((v), 0)

/* Next in memory order */
static inline value Next_in_mem (value v) {
  return (value) &Field ((v), Whsize_val (v));
}

#ifdef CAML_INSTR
static uintnat instr_size [20] =
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static char *instr_name [20] = {
  NULL,
  "alloc01@",
  "alloc02@",
  "alloc03@",
  "alloc04@",
  "alloc05@",
  "alloc06@",
  "alloc07@",
  "alloc08@",
  "alloc09@",
  "alloc10-19@",
  "alloc20-29@",
  "alloc30-39@",
  "alloc40-49@",
  "alloc50-59@",
  "alloc60-69@",
  "alloc70-79@",
  "alloc80-89@",
  "alloc90-99@",
  "alloc_large@",
};
uintnat caml_instr_alloc_jump = 0;
/* number of pointers followed to allocate from the free set */

#define INSTR_alloc_jump(n) (caml_instr_alloc_jump += (n))

#else

#define INSTR_alloc_jump(n) ((void)0)

#endif /*CAML_INSTR*/


/********************* next-fit allocation policy *********************/

/* The free-list is kept sorted by increasing addresses.
   This makes the merging of adjacent free blocks possible.
   (See [nf_merge_block].)
*/

/* The sentinel can be located anywhere in memory, but it must not be
   adjacent to any heap object. */
static struct {
  value filler1; /* Make sure the sentinel is never adjacent to any block. */
  header_t h;
  value first_field;
  value filler2; /* Make sure the sentinel is never adjacent to any block. */
} nf_sentinel = {0, Make_header (0, 0, Caml_blue), Val_NULL, 0};

#define Nf_head (Val_bp (&(nf_sentinel.first_field)))

static value nf_prev = Nf_head;  /* Current allocation pointer. */
static value nf_last = Val_NULL; /* Last block in the list.  Only valid
                                  just after [caml_fl_allocate] returns NULL. */

#ifdef DEBUG
static void nf_check (void)
{
  value cur, prev;
  int prev_found = 0, merge_found = 0;
  uintnat size_found = 0;

  prev = Nf_head;
  cur = Next_small (prev);
  while (cur != Val_NULL){
    size_found += Whsize_bp (cur);
    CAMLassert (Is_in_heap (cur));
    if (cur == nf_prev) prev_found = 1;
    if (cur == caml_fl_merge) merge_found = 1;
    prev = cur;
    cur = Next_small (prev);
  }
  CAMLassert (prev_found || nf_prev == Nf_head);
  CAMLassert (merge_found || caml_fl_merge == Nf_head);
  CAMLassert (size_found == caml_fl_cur_wsz);
}

#define DEBUG_nf_check() nf_check()

#else

#define DEBUG_nf_check() ((void) 0)

#endif /* DEBUG */

/* [nf_allocate_block] is called by [nf_allocate].  Given a suitable free
   block and the requested size, it allocates a new block from the free
   block.  There are three cases:
   0. The free block has the requested size. Detach the block from the
      free-list and return it.
   1. The free block is 1 word longer than the requested size. Detach
      the block from the free list.  The remaining word cannot be linked:
      turn it into an empty block (header only), and return the rest.
   2. The free block is large enough. Split it in two and return the right
      block.
   In all cases, the allocated block is right-justified in the free block:
   it is located in the high-address words of the free block, so that
   the linking of the free-list does not change in case 2.
*/
static header_t *nf_allocate_block (mlsize_t wh_sz, int flpi, value prev,
                                    value cur)
{
  header_t h = Hd_bp (cur);
  CAMLassert (Whsize_hd (h) >= wh_sz);
  if (Wosize_hd (h) < wh_sz + 1){                        /* Cases 0 and 1. */
    caml_fl_cur_wsz -= Whsize_hd (h);
    Next_small (prev) = Next_small (cur);
    CAMLassert (Is_in_heap (Next_small (prev))
                || Next_small (prev) == Val_NULL);
    if (caml_fl_merge == cur) caml_fl_merge = prev;
#ifdef DEBUG
    nf_last = Val_NULL;
#endif
      /* In case 1, the following creates the empty block correctly.
         In case 0, it gives an invalid header to the block.  The function
         calling [caml_fl_allocate] will overwrite it. */
    Hd_op (cur) = Make_header (0, 0, Caml_white);
  }else{                                                        /* Case 2. */
    caml_fl_cur_wsz -= wh_sz;
    Hd_op (cur) = Make_header (Wosize_hd (h) - wh_sz, 0, Caml_blue);
  }
  nf_prev = prev;
  return (header_t *) &Field (cur, Wosize_hd (h) - wh_sz);
}

static header_t *nf_allocate (mlsize_t wo_sz)
{
  value cur = Val_NULL, prev;
  CAMLassert (sizeof (char *) == sizeof (value));
  CAMLassert (wo_sz >= 1);
#ifdef CAML_INSTR
  if (wo_sz < 10){
    ++instr_size[wo_sz];
  }else if (wo_sz < 100){
    ++instr_size[wo_sz/10 + 9];
  }else{
    ++instr_size[19];
  }
#endif /* CAML_INSTR */

    CAMLassert (nf_prev != Val_NULL);
    /* Search from [nf_prev] to the end of the list. */
    prev = nf_prev;
    cur = Next_small (prev);
    while (cur != Val_NULL){
      CAMLassert (Is_in_heap (cur));
      if (Wosize_bp (cur) >= wo_sz){
        return nf_allocate_block (Whsize_wosize (wo_sz), 0, prev, cur);
      }
      prev = cur;
      cur = Next_small (prev);
#ifdef CAML_INSTR
      ++ caml_instr_alloc_jump;
#endif
    }
    nf_last = prev;
    /* Search from the start of the list to [nf_prev]. */
    prev = Nf_head;
    cur = Next_small (prev);
    while (prev != nf_prev){
      if (Wosize_bp (cur) >= wo_sz){
        return nf_allocate_block (Whsize_wosize (wo_sz), 0, prev, cur);
      }
      prev = cur;
      cur = Next_small (prev);
#ifdef CAML_INSTR
      ++ caml_instr_alloc_jump;
#endif
    }
    /* No suitable block was found. */
    return NULL;
}

/* Location of the last fragment seen by the sweeping code.
   This is a pointer to the first word after the fragment, which is
   the header of the next block.
   Note that [last_fragment] doesn't point to the fragment itself,
   but to the block after it.
*/
static header_t *nf_last_fragment;

static void nf_init_merge (void)
{
#ifdef CAML_INSTR
  int i;
  for (i = 1; i < 20; i++){
    CAML_INSTR_INT (instr_name[i], instr_size[i]);
    instr_size[i] = 0;
  }
#endif /* CAML_INSTR */
  nf_last_fragment = NULL;
  caml_fl_merge = Nf_head;
  DEBUG_nf_check ();
}

static void nf_reset (void)
{
  Next_small (Nf_head) = Val_NULL;
  nf_prev = Nf_head;
  caml_fl_cur_wsz = 0;
  nf_init_merge ();
}

/* Note: the [limit] parameter is unused because we merge blocks one by one. */
static header_t *nf_merge_block (value bp, char *limit)
{
  value prev, cur, adj;
  header_t hd = Hd_val (bp);
  mlsize_t prev_wosz;

  caml_fl_cur_wsz += Whsize_hd (hd);

  /* [merge_block] is now responsible for calling the finalization function. */
  if (Tag_hd (hd) == Custom_tag){
    void (*final_fun)(value) = Custom_ops_val(bp)->finalize;
    if (final_fun != NULL) final_fun(bp);
  }

#ifdef DEBUG
  caml_set_fields (bp, 0, Debug_free_major);
#endif
  prev = caml_fl_merge;
  cur = Next_small (prev);
  /* The sweep code makes sure that this is the right place to insert
     this block: */
  CAMLassert (prev < bp || prev == Nf_head);
  CAMLassert (cur > bp || cur == Val_NULL);

  /* If [last_fragment] and [bp] are adjacent, merge them. */
  if (nf_last_fragment == Hp_bp (bp)){
    mlsize_t bp_whsz = Whsize_val (bp);
    if (bp_whsz <= Max_wosize){
      hd = Make_header (bp_whsz, 0, Caml_white);
      bp = (value) nf_last_fragment;
      Hd_val (bp) = hd;
      caml_fl_cur_wsz += Whsize_wosize (0);
    }
  }

  /* If [bp] and [cur] are adjacent, remove [cur] from the free-list
     and merge them. */
  adj = Next_in_mem (bp);
  if (adj == cur){
    value next_cur = Next_small (cur);
    mlsize_t cur_whsz = Whsize_val (cur);

    if (Wosize_hd (hd) + cur_whsz <= Max_wosize){
      Next_small (prev) = next_cur;
      if (nf_prev == cur) nf_prev = prev;
      hd = Make_header (Wosize_hd (hd) + cur_whsz, 0, Caml_blue);
      Hd_val (bp) = hd;
      adj = Next_in_mem (bp);
#ifdef DEBUG
      nf_last = Val_NULL;
      Next_small (cur) = (value) Debug_free_major;
      Hd_val (cur) = Debug_free_major;
#endif
      cur = next_cur;
    }
  }
  /* If [prev] and [bp] are adjacent merge them, else insert [bp] into
     the free-list if it is big enough. */
  prev_wosz = Wosize_val (prev);
  if (Next_in_mem (prev) == bp && prev_wosz + Whsize_hd (hd) < Max_wosize){
    Hd_val (prev) = Make_header (prev_wosz + Whsize_hd (hd), 0, Caml_blue);
#ifdef DEBUG
    Hd_val (bp) = Debug_free_major;
#endif
    CAMLassert (caml_fl_merge == prev);
  }else if (Wosize_hd (hd) != 0){
    Hd_val (bp) = Bluehd_hd (hd);
    Next_small (bp) = cur;
    Next_small (prev) = bp;
    caml_fl_merge = bp;
  }else{
    /* This is a fragment.  Leave it in white but remember it for eventual
       merging with the next block. */
    nf_last_fragment = (header_t *) bp;
    caml_fl_cur_wsz -= Whsize_wosize (0);
  }
  return Hp_val (adj);
}

/* This is a heap extension.  We have to insert it in the right place
   in the free-list.
   [caml_fl_add_blocks] can only be called right after a call to
   [caml_fl_allocate] that returned Val_NULL.
   Most of the heap extensions are expected to be at the end of the
   free list.  (This depends on the implementation of [malloc].)

   [bp] must point to a list of blocks chained by their field 0,
   terminated by Val_NULL, and field 1 of the first block must point to
   the last block.
*/
static void nf_add_blocks (value bp)
{
  value cur = bp;
  CAMLassert (nf_last != Val_NULL);
  CAMLassert (Next_small (nf_last) == Val_NULL);
  do {
    caml_fl_cur_wsz += Whsize_bp (cur);
    cur = Field(cur, 0);
  } while (cur != Val_NULL);

  if (bp > nf_last){
    Next_small (nf_last) = bp;
    if (nf_last == caml_fl_merge && (char *) bp < caml_gc_sweep_hp){
      caml_fl_merge = Field (bp, 1);
    }
  }else{
    value prev;

    prev = Nf_head;
    cur = Next_small (prev);
    while (cur != Val_NULL && cur < bp){
      CAMLassert (prev < bp || prev == Nf_head);
      prev = cur;
      cur = Next_small (prev);
    }
    CAMLassert (prev < bp || prev == Nf_head);
    CAMLassert (cur > bp || cur == Val_NULL);
    Next_small (Field (bp, 1)) = cur;
    Next_small (prev) = bp;
    /* When inserting blocks between [caml_fl_merge] and [caml_gc_sweep_hp],
       we must advance [caml_fl_merge] to the new block, so that [caml_fl_merge]
       is always the last free-list block before [caml_gc_sweep_hp]. */
    if (prev == caml_fl_merge && (char *) bp < caml_gc_sweep_hp){
      caml_fl_merge = Field (bp, 1);
    }
  }
}

static void nf_make_free_blocks
  (value *p, mlsize_t size, int do_merge, int color)
{
  mlsize_t sz;

  while (size > 0){
    if (size > Whsize_wosize (Max_wosize)){
      sz = Whsize_wosize (Max_wosize);
    }else{
      sz = size;
    }
    *(header_t *)p = Make_header (Wosize_whsize (sz), 0, color);
    if (do_merge) nf_merge_block (Val_hp (p), NULL);
    size -= sz;
    p += sz;
  }
}

/******************** first-fit allocation policy *********************/

#define FLP_MAX 1000
static value flp [FLP_MAX];
static int flp_size = 0;
static value beyond = Val_NULL;

/* The sentinel can be located anywhere in memory, but it must not be
   adjacent to any heap object. */
static struct {
  value filler1; /* Make sure the sentinel is never adjacent to any block. */
  header_t h;
  value first_field;
  value filler2; /* Make sure the sentinel is never adjacent to any block. */
} ff_sentinel = {0, Make_header (0, 0, Caml_blue), Val_NULL, 0};

#define Ff_head (Val_bp (&(ff_sentinel.first_field)))
static value ff_last = Val_NULL; /* Last block in the list.  Only valid
                                  just after [caml_fl_allocate] returns NULL. */


#ifdef DEBUG
static void ff_check (void)
{
  value cur, prev;
  int flp_found = 0, merge_found = 0;
  uintnat size_found = 0;
  int sz = 0;

  prev = Ff_head;
  cur = Next_small (prev);
  while (cur != Val_NULL){
    size_found += Whsize_bp (cur);
    CAMLassert (Is_in_heap (cur));
    if (Wosize_bp (cur) > sz){
      sz = Wosize_bp (cur);
      if (flp_found < flp_size){
        CAMLassert (Next_small (flp[flp_found]) == cur);
        ++ flp_found;
      }else{
        CAMLassert (beyond == Val_NULL || cur >= Next_small (beyond));
      }
    }
    if (cur == caml_fl_merge) merge_found = 1;
    prev = cur;
    cur = Next_small (prev);
  }
  CAMLassert (flp_found == flp_size);
  CAMLassert (merge_found || caml_fl_merge == Ff_head);
  CAMLassert (size_found == caml_fl_cur_wsz);
}
#endif /* DEBUG */

/* [ff_allocate_block] is called by [ff_allocate].  Given a suitable free
   block and the requested size, it allocates a new block from the free
   block.  There are three cases:
   0. The free block has the requested size. Detach the block from the
      free-list and return it.
   1. The free block is 1 word longer than the requested size. Detach
      the block from the free list.  The remaining word cannot be linked:
      turn it into an empty block (header only), and return the rest.
   2. The free block is large enough. Split it in two and return the right
      block.
   In all cases, the allocated block is right-justified in the free block:
   it is located in the high-address words of the free block, so that
   the linking of the free-list does not change in case 2.
*/
static header_t *ff_allocate_block (mlsize_t wh_sz, int flpi, value prev,
                                    value cur)
{
  header_t h = Hd_bp (cur);
  CAMLassert (Whsize_hd (h) >= wh_sz);
  if (Wosize_hd (h) < wh_sz + 1){                        /* Cases 0 and 1. */
    caml_fl_cur_wsz -= Whsize_hd (h);
    Next_small (prev) = Next_small (cur);
    CAMLassert (Is_in_heap (Next_small (prev))
                || Next_small (prev) == Val_NULL);
    if (caml_fl_merge == cur) caml_fl_merge = prev;
#ifdef DEBUG
    ff_last = Val_NULL;
#endif
      /* In case 1, the following creates the empty block correctly.
         In case 0, it gives an invalid header to the block.  The function
         calling [caml_fl_allocate] will overwrite it. */
    Hd_op (cur) = Make_header (0, 0, Caml_white);
    if (flpi + 1 < flp_size && flp[flpi + 1] == cur){
      flp[flpi + 1] = prev;
    }else if (flpi == flp_size - 1){
      beyond = (prev == Ff_head) ? Val_NULL : prev;
      -- flp_size;
    }
  }else{                                                        /* Case 2. */
    caml_fl_cur_wsz -= wh_sz;
    Hd_op (cur) = Make_header (Wosize_hd (h) - wh_sz, 0, Caml_blue);
  }
  return (header_t *) &Field (cur, Wosize_hd (h) - wh_sz);
}

static header_t *ff_allocate (mlsize_t wo_sz)
{
  value cur = Val_NULL, prev;
  header_t *result;
  int i;
  mlsize_t sz, prevsz;
  CAMLassert (sizeof (char *) == sizeof (value));
  CAMLassert (wo_sz >= 1);
#ifdef CAML_INSTR
  if (wo_sz < 10){
    ++instr_size[wo_sz];
  }else if (wo_sz < 100){
    ++instr_size[wo_sz/10 + 9];
  }else{
    ++instr_size[19];
  }
#endif /* CAML_INSTR */

    /* Search in the flp array. */
    for (i = 0; i < flp_size; i++){
      sz = Wosize_bp (Next_small (flp[i]));
      if (sz >= wo_sz){
#if FREELIST_DEBUG
        if (i > 5) fprintf (stderr, "FLP: found at %d  size=%d\n", i, wo_sz);
#endif
        result = ff_allocate_block (Whsize_wosize (wo_sz), i, flp[i],
                                    Next_small (flp[i]));
        goto update_flp;
      }
    }
    /* Extend the flp array. */
    if (flp_size == 0){
      prev = Ff_head;
      prevsz = 0;
    }else{
      prev = Next_small (flp[flp_size - 1]);
      prevsz = Wosize_bp (prev);
      if (beyond != Val_NULL) prev = beyond;
    }
    while (flp_size < FLP_MAX){
      cur = Next_small (prev);
      if (cur == Val_NULL){
        ff_last = prev;
        beyond = (prev == Ff_head) ? Val_NULL : prev;
        return NULL;
      }else{
        sz = Wosize_bp (cur);
        if (sz > prevsz){
          flp[flp_size] = prev;
          ++ flp_size;
          if (sz >= wo_sz){
            beyond = cur;
            i = flp_size - 1;
#if FREELIST_DEBUG
            if (flp_size > 5){
              fprintf (stderr, "FLP: extended to %d\n", flp_size);
            }
#endif
            result = ff_allocate_block (Whsize_wosize (wo_sz), flp_size - 1,
                                        prev, cur);
            goto update_flp;
          }
          prevsz = sz;
        }
      }
      prev = cur;
    }
    beyond = cur;

    /* The flp table is full.  Do a slow first-fit search. */
#if FREELIST_DEBUG
    fprintf (stderr, "FLP: table is full -- slow first-fit\n");
#endif
    if (beyond != Val_NULL){
      prev = beyond;
    }else{
      prev = flp[flp_size - 1];
    }
    prevsz = Wosize_bp (Next_small (flp[FLP_MAX-1]));
    CAMLassert (prevsz < wo_sz);
    cur = Next_small (prev);
    while (cur != Val_NULL){
      CAMLassert (Is_in_heap (cur));
      sz = Wosize_bp (cur);
      if (sz < prevsz){
        beyond = cur;
      }else if (sz >= wo_sz){
        return ff_allocate_block (Whsize_wosize (wo_sz), flp_size, prev, cur);
      }
      prev = cur;
      cur = Next_small (prev);
    }
    ff_last = prev;
    return NULL;

  update_flp: /* (i, sz) */
    /* The block at [i] was removed or reduced.  Update the table. */
    CAMLassert (0 <= i && i < flp_size + 1);
    if (i < flp_size){
      if (i > 0){
        prevsz = Wosize_bp (Next_small (flp[i-1]));
      }else{
        prevsz = 0;
      }
      if (i == flp_size - 1){
        if (Wosize_bp (Next_small (flp[i])) <= prevsz){
          beyond = Next_small (flp[i]);
          -- flp_size;
        }else{
          beyond = Val_NULL;
        }
      }else{
        value buf [FLP_MAX];
        int j = 0;
        mlsize_t oldsz = sz;

        prev = flp[i];
        while (prev != flp[i+1] && j < FLP_MAX - i){
          cur = Next_small (prev);
          sz = Wosize_bp (cur);
          if (sz > prevsz){
            buf[j++] = prev;
            prevsz = sz;
            if (sz >= oldsz){
              CAMLassert (sz == oldsz);
              break;
            }
          }
          prev = cur;
        }
#if FREELIST_DEBUG
        if (j > 2) fprintf (stderr, "FLP: update; buf size = %d\n", j);
#endif
        if (FLP_MAX >= flp_size + j - 1){
          if (j != 1){
            memmove (&flp[i+j], &flp[i+1], sizeof (value) * (flp_size-i-1));
          }
          if (j > 0) memmove (&flp[i], &buf[0], sizeof (value) * j);
          flp_size += j - 1;
        }else{
          if (FLP_MAX > i + j){
            if (j != 1){
              memmove (&flp[i+j], &flp[i+1], sizeof (value) * (FLP_MAX-i-j));
            }
            if (j > 0) memmove (&flp[i], &buf[0], sizeof (value) * j);
          }else{
            if (i != FLP_MAX){
              memmove (&flp[i], &buf[0], sizeof (value) * (FLP_MAX - i));
            }
          }
          flp_size = FLP_MAX - 1;
          beyond = Next_small (flp[FLP_MAX - 1]);
        }
      }
    }
    return result;
}

/* Location of the last fragment seen by the sweeping code.
   This is a pointer to the first word after the fragment, which is
   the header of the next block.
   Note that [ff_last_fragment] doesn't point to the fragment itself,
   but to the block after it.
*/
static header_t *ff_last_fragment;

static void ff_init_merge (void)
{
#ifdef CAML_INSTR
  int i;
  for (i = 1; i < 20; i++){
    CAML_INSTR_INT (instr_name[i], instr_size[i]);
    instr_size[i] = 0;
  }
#endif /* CAML_INSTR */
  ff_last_fragment = NULL;
  caml_fl_merge = Ff_head;
#ifdef DEBUG
  ff_check ();
#endif
}

static void ff_truncate_flp (value changed)
{
  if (changed == Ff_head){
    flp_size = 0;
    beyond = Val_NULL;
  }else{
    while (flp_size > 0 && Next_small (flp[flp_size - 1]) >= changed)
      -- flp_size;
    if (beyond >= changed) beyond = Val_NULL;
  }
}

static void ff_reset (void)
{
  Next_small (Ff_head) = Val_NULL;
  ff_truncate_flp (Ff_head);
  caml_fl_cur_wsz = 0;
  caml_fl_init_merge ();
}

/* Note: the [limit] parameter is unused because we merge blocks one by one. */
header_t *ff_merge_block (value bp, char *limit)
{
  value prev, cur, adj;
  header_t hd = Hd_val (bp);
  mlsize_t prev_wosz;

  caml_fl_cur_wsz += Whsize_hd (hd);

  /* [merge_block] is now responsible for calling the finalization function. */
  if (Tag_hd (hd) == Custom_tag){
    void (*final_fun)(value) = Custom_ops_val(bp)->finalize;
    if (final_fun != NULL) final_fun(bp);
  }

#ifdef DEBUG
  caml_set_fields (bp, 0, Debug_free_major);
#endif
  prev = caml_fl_merge;
  cur = Next_small (prev);
  /* The sweep code makes sure that this is the right place to insert
     this block: */
  CAMLassert (prev < bp || prev == Ff_head);
  CAMLassert (cur > bp || cur == Val_NULL);

  ff_truncate_flp (prev);

  /* If [ff_last_fragment] and [bp] are adjacent, merge them. */
  if (ff_last_fragment == Hp_bp (bp)){
    mlsize_t bp_whsz = Whsize_val (bp);
    if (bp_whsz <= Max_wosize){
      hd = Make_header (bp_whsz, 0, Caml_white);
      bp = (value) ff_last_fragment;
      Hd_val (bp) = hd;
      caml_fl_cur_wsz += Whsize_wosize (0);
    }
  }

  /* If [bp] and [cur] are adjacent, remove [cur] from the free-list
     and merge them. */
  adj = Next_in_mem (bp);
  if (adj == cur){
    value next_cur = Next_small (cur);
    mlsize_t cur_whsz = Whsize_val (cur);

    if (Wosize_hd (hd) + cur_whsz <= Max_wosize){
      Next_small (prev) = next_cur;
      hd = Make_header (Wosize_hd (hd) + cur_whsz, 0, Caml_blue);
      Hd_val (bp) = hd;
      adj = Next_in_mem (bp);
#ifdef DEBUG
      ff_last = Val_NULL;
      Next_small (cur) = (value) Debug_free_major;
      Hd_val (cur) = Debug_free_major;
#endif
      cur = next_cur;
    }
  }
  /* If [prev] and [bp] are adjacent merge them, else insert [bp] into
     the free-list if it is big enough. */
  prev_wosz = Wosize_val (prev);
  if (Next_in_mem (prev) == bp && prev_wosz + Whsize_hd (hd) < Max_wosize){
    Hd_val (prev) = Make_header (prev_wosz + Whsize_hd (hd), 0, Caml_blue);
#ifdef DEBUG
    Hd_val (bp) = Debug_free_major;
#endif
    CAMLassert (caml_fl_merge == prev);
  }else if (Wosize_hd (hd) != 0){
    Hd_val (bp) = Bluehd_hd (hd);
    Next_small (bp) = cur;
    Next_small (prev) = bp;
    caml_fl_merge = bp;
  }else{
    /* This is a fragment.  Leave it in white but remember it for eventual
       merging with the next block. */
    ff_last_fragment = (header_t *) bp;
    caml_fl_cur_wsz -= Whsize_wosize (0);
  }
  return Hp_val (adj);
}

/* This is a heap extension.  We have to insert it in the right place
   in the free-list.
   [ff_add_blocks] can only be called right after a call to
   [ff_allocate] that returned Val_NULL.
   Most of the heap extensions are expected to be at the end of the
   free list.  (This depends on the implementation of [malloc].)

   [bp] must point to a list of blocks chained by their field 0,
   terminated by Val_NULL, and field 1 of the first block must point to
   the last block.
*/
static void ff_add_blocks (value bp)
{
  value cur = bp;
  CAMLassert (ff_last != Val_NULL);
  CAMLassert (Next_small (ff_last) == Val_NULL);
  do {
    caml_fl_cur_wsz += Whsize_bp (cur);
    cur = Field(cur, 0);
  } while (cur != Val_NULL);

  if (bp > ff_last){
    Next_small (ff_last) = bp;
    if (ff_last == caml_fl_merge && (char *) bp < caml_gc_sweep_hp){
      caml_fl_merge = Field (bp, 1);
    }
    if (flp_size < FLP_MAX){
      flp [flp_size++] = ff_last;
    }
  }else{
    value prev;

    prev = Ff_head;
    cur = Next_small (prev);
    while (cur != Val_NULL && cur < bp){
      CAMLassert (prev < bp || prev == Ff_head);
      /* XXX TODO: extend flp on the fly */
      prev = cur;
      cur = Next_small (prev);
    }
    CAMLassert (prev < bp || prev == Ff_head);
    CAMLassert (cur > bp || cur == Val_NULL);
    Next_small (Field (bp, 1)) = cur;
    Next_small (prev) = bp;
    /* When inserting blocks between [caml_fl_merge] and [caml_gc_sweep_hp],
       we must advance [caml_fl_merge] to the new block, so that [caml_fl_merge]
       is always the last free-list block before [caml_gc_sweep_hp]. */
    if (prev == caml_fl_merge && (char *) bp < caml_gc_sweep_hp){
      caml_fl_merge = Field (bp, 1);
    }
    ff_truncate_flp (bp);
  }
}

static void ff_make_free_blocks
  (value *p, mlsize_t size, int do_merge, int color)
{
  mlsize_t sz;

  while (size > 0){
    if (size > Whsize_wosize (Max_wosize)){
      sz = Whsize_wosize (Max_wosize);
    }else{
      sz = size;
    }
    *(header_t *)p = Make_header (Wosize_whsize (sz), 0, color);
    if (do_merge) ff_merge_block (Val_hp (p), NULL);
    size -= sz;
    p += sz;
  }
}

/********************* best-fit allocation policy *********************/

/* quick-fit + FIFO-ordered best fit (Wilson's nomenclature)
   We use Standish's data structure (a tree of doubly-linked lists)
   with a splay tree (Sleator & Tarjan).
*/

/* [BF_NUM_SMALL] must be at least 4 for this code to work
   and at least 5 for good performance on typical OCaml programs.
*/
#define BF_NUM_SMALL 16

static struct {
  value free;
  value *merge;
} bf_small_fl [BF_NUM_SMALL + 1];

/* Small free blocks have only one pointer to the next block.
   Large free blocks have 5 fields:
   tree fields:
     - node flag
     - left son
     - right son
   list fields:
     - next
     - prev
*/
typedef struct large_free_block {
  int isnode;
  struct large_free_block *left;
  struct large_free_block *right;
  struct large_free_block *prev;
  struct large_free_block *next;
} large_free_block;

static inline mlsize_t bf_large_wosize (struct large_free_block *n) {
  return Wosize_val((value)(n));
}

static struct large_free_block *bf_large_tree;


/* debug functions for checking the data structures */

#ifdef DEBUG
static mlsize_t bf_check_cur_size = 0;
static asize_t bf_check_subtree (large_free_block *p)
{
  mlsize_t wosz;
  large_free_block *cur, *next;
  asize_t total_size = 0;

  if (p == NULL) return 0;

  wosz = bf_large_wosize(p);
  CAMLassert (p->isnode);
  total_size += bf_check_subtree (p->left);
  CAMLassert (wosz > BF_NUM_SMALL);
  CAMLassert (wosz > bf_check_cur_size);
  bf_check_cur_size = wosz;
  cur = p;
  while (1){
    CAMLassert (bf_large_wosize (cur) == wosz);
    CAMLassert (Color_val ((value) cur) == Caml_blue);
    CAMLassert (cur == p || ! cur->isnode);
    total_size += Whsize_wosize (wosz);
    next = cur->next;
    CAMLassert (next->prev == cur);
    if (next == p) break;
    cur = next;
  }
  total_size += bf_check_subtree (p->right);
  return total_size;
}

static void bf_check (void)
{
  mlsize_t i;
  asize_t total_size = 0;

  /* check free lists */
  for (i = 1; i <= BF_NUM_SMALL; i++){
    value b;
    int col = 0;
    int merge_found = 0;

    if (bf_small_fl[i].merge == &bf_small_fl[i].free) merge_found = 1;
    for (b = bf_small_fl[i].free; b != Val_NULL; b = Next_small (b)){
      if (bf_small_fl[i].merge == &Next_small (b)) merge_found = 1;
      CAMLassert (Wosize_val (b) == i);
      total_size += Whsize_wosize (i);
      if (Color_val (b) == Caml_blue){
        col = 1;
        CAMLassert (Next_small (b) == Val_NULL || Next_small (b) > b);
      }else{
        CAMLassert (col == 0);
        CAMLassert (Color_val (b) != Caml_gray);
      }
    }
    CAMLassert (merge_found);
  }
  /* check the tree */
  bf_check_cur_size = 0;
  total_size += bf_check_subtree (bf_large_tree);
  /* check the total free set size */
  CAMLassert (total_size == caml_fl_cur_wsz);
}

#define DEBUG_bf_check() bf_check()

#else

#define DEBUG_bf_check() ((void)0)

#endif /* DEBUG */

/**************************************************************************/
/* splay trees */

/* Our tree is composed of nodes. Each node is the head of a doubly-linked
   circular list of blocks, all of the same size.
*/

/* Search for the node of the given size. Return a pointer to the pointer
   to the node, or a pointer to the NULL where the node should have been
   (it can be inserted here).
*/
static large_free_block **bf_search (mlsize_t wosz)
{
  large_free_block **p = &bf_large_tree;
  large_free_block *cur;
  mlsize_t cursz;

  while (1){
    cur = *p;
    INSTR_alloc_jump (1);
    if (cur == NULL) break;
    cursz = bf_large_wosize (cur);
    if (cursz == wosz){
      break;
    }else if (cursz > wosz){
      p = &(cur->left);
    }else{
      CAMLassert (cursz < wosz);
      p = &(cur->right);
    }
  }
  return p;
}

/* Search for the least node that is large enough to accomodate the given
   size. Return in [next_lower] an upper bound on the size of the
   next-lower node in the tree, or BF_NUM_SMALL if there is no such node.
*/
static large_free_block **bf_search_best (mlsize_t wosz, mlsize_t *next_lower)
{
  large_free_block **p = &bf_large_tree;
  large_free_block **best = NULL;
  mlsize_t lowsz = BF_NUM_SMALL;
  large_free_block *cur;
  mlsize_t cursz;

  while (1){
    cur = *p;
    INSTR_alloc_jump (1);
    if (cur == NULL){
      *next_lower = lowsz;
      break;
    }
    cursz = bf_large_wosize (cur);
    if (cursz == wosz){
      best = p;
      *next_lower = wosz;
      break;
    }else if (cursz > wosz){
      best = p;
      p = &(cur->left);
    }else{
      CAMLassert (cursz < wosz);
      lowsz = cursz;
      p = &(cur->right);
    }
  }
  return best;
}

/* Splay the tree at the given size. If a node of this size exists, it will
   become the root. If not, the last visited node will be the root. This is
   either the least node larger or the greatest node smaller than the given
   size.
   We use simple top-down splaying as described in S&T 85.
*/
static void bf_splay (mlsize_t wosz)
{
  large_free_block *x, *y;
  mlsize_t xsz;
  large_free_block *left_top = NULL;
  large_free_block *right_top = NULL;
  large_free_block **left_bottom = &left_top;
  large_free_block **right_bottom = &right_top;

  x = bf_large_tree;
  if (x == NULL) return;
  while (1){
    xsz = bf_large_wosize (x);
    if (xsz == wosz) break;
    if (xsz > wosz){
      /* zig */
      y = x->left;
      INSTR_alloc_jump (1);
      if (y == NULL) break;
      if (bf_large_wosize (y) > wosz){
        /* zig-zig: rotate right */
        x->left = y->right;
        y->right = x;
        x = y;
        y = x->left;
        INSTR_alloc_jump (2);
        if (y == NULL) break;
      }
      /* link right */
      *right_bottom = x;
      right_bottom = &(x->left);
      x = y;
    }else{
      CAMLassert (xsz < wosz);
      /* zag */
      y = x->right;
      INSTR_alloc_jump (1);
      if (y == NULL) break;
      if (bf_large_wosize (y) < wosz){
        /* zag-zag : rotate left */
        x->right = y->left;
        y->left = x;
        x = y;
        y = x->right;
        INSTR_alloc_jump (2);
        if (y == NULL) break;
      }
      /* link left */
      *left_bottom = x;
      left_bottom = &(x->right);
      x = y;
    }
  }
  /* reassemble the tree */
  *left_bottom = x->left;
  *right_bottom = x->right;
  x->left = left_top;
  x->right = right_top;
  INSTR_alloc_jump (2);
  bf_large_tree = x;
}

/* Splay the subtree at [p] on its leftmost (least) node. After this
   operation, the root node of the subtree is the least node and it
   has no left child.
   The subtree must not be empty.
*/
static void bf_splay_least (large_free_block **p)
{
  large_free_block *x, *y;
  large_free_block *right_top = NULL;
  large_free_block **right_bottom = &right_top;

  x = *p;
  INSTR_alloc_jump (1);
  CAMLassert (x != NULL);
  while (1){
    /* We are always in the zig case. */
    y = x->left;
    INSTR_alloc_jump (1);
    if (y == NULL) break;
    /* And in the zig-zig case. rotate right */
    x->left = y->right;
    y->right = x;
    x = y;
    y = x->left;
    INSTR_alloc_jump (2);
    if (y == NULL) break;
    /* link right */
    *right_bottom = x;
    right_bottom = &(x->left);
    x = y;
  }
  /* reassemble the tree */
  *right_bottom = x->right;
  INSTR_alloc_jump (1);
  x->right = right_top;
  *p = x;
}

/* Remove the node at [p], if any. */
static void bf_remove_node (large_free_block **p)
{
  large_free_block *x;
  large_free_block *l, *r;

  x = *p;
  INSTR_alloc_jump (1);
  if (x == NULL) return;
  l = x->left;
  r = x->right;
  INSTR_alloc_jump (2);
  if (l == NULL){
    *p = r;
  }else if (r == NULL){
    *p = l;
  }else{
    bf_splay_least (&(x->right));
    r = x->right;
    INSTR_alloc_jump (1);
    r->left = l;
    *p = r;
  }
}

/* Insert a block into the tree, either as a new node or as a block in an
   existing list.
   Splay if the list is already present.
*/
static void bf_insert_block (large_free_block *n)
{
  large_free_block **p = bf_search (bf_large_wosize (n));
  large_free_block *x = *p;
  INSTR_alloc_jump (1);

  CAMLassert (Color_val ((value) n) == Caml_blue);
  CAMLassert (Wosize_val ((value) n) > BF_NUM_SMALL);
  if (x == NULL){
    /* add new node */
    n->isnode = 1;
    n->left = n->right = NULL;
    n->prev = n->next = n;
    *p = n;
  }else{
    /* insert at tail of doubly-linked list */
    CAMLassert (x->isnode);
    n->isnode = 0;
#ifdef DEBUG
    n->left = n->right = (large_free_block *) Debug_free_unused;
#endif
    n->prev = x->prev;
    n->next = x;
    x->prev->next = n;
    x->prev = n;
    INSTR_alloc_jump (2);
    bf_splay (bf_large_wosize (n));
  }
}

#ifdef DEBUG
static int bf_is_in_tree (large_free_block *b)
{
  int wosz = bf_large_wosize (b);
  large_free_block **p = bf_search (wosz);
  large_free_block *n = *p;
  large_free_block *cur = n;

  if (n == NULL) return 0;
  while (1){
    if (cur == b) return 1;
    cur = cur->next;
    if (cur == n) return 0;
  }
}
#endif /* DEBUG */

/**************************************************************************/

/* Add back a fragment into a small free list. The block must be small
   and black and its tag must be abstract. */
static void bf_insert_fragment_small (value v)
{
  mlsize_t wosz = Wosize_val (v);

  CAMLassert (Color_val (v) == Caml_black);
  CAMLassert (Tag_val (v) == Abstract_tag);
  CAMLassert (1 <= wosz && wosz <= BF_NUM_SMALL);
  Next_small (v) = bf_small_fl[wosz].free;
  bf_small_fl[wosz].free = v;
  if (bf_small_fl[wosz].merge == &bf_small_fl[wosz].free){
    bf_small_fl[wosz].merge = &Next_small (v);
  }
}

/* Add back a fragment into the free set. The block must have the
   appropriate color:
   White if it is a fragment (wosize = 0)
   Black if it is a small block (0 < wosize <= BF_NUM_SMALL)
   Blue if it is a large block (BF_NUM_SMALL < wosize)
*/
static void bf_insert_fragment (value v)
{
  mlsize_t wosz = Wosize_val (v);

  if (wosz == 0){
    CAMLassert (Color_val (v) == Caml_white);
  }else if (wosz <= BF_NUM_SMALL){
    CAMLassert (Color_val (v) == Caml_black);
    bf_insert_fragment_small (v);
  }else{
    CAMLassert (Color_val (v) == Caml_blue);
    bf_insert_block ((large_free_block *) v);
  }
}
/* Insert the block into the free set during sweep. The block must be blue. */
static void bf_insert_sweep (value v)
{
  mlsize_t wosz = Wosize_val (v);
  value next;

  CAMLassert (Color_val (v) == Caml_blue);
  if (wosz <= BF_NUM_SMALL){
    while (1){
      next = *bf_small_fl[wosz].merge;
      if (next == Val_NULL || next >= v) break;
      bf_small_fl[wosz].merge = &Next_small (next);
    }
    Next_small (v) = *bf_small_fl[wosz].merge;
    *bf_small_fl[wosz].merge = v;
    bf_small_fl[wosz].merge = &Next_small (v);
  }else{
    bf_insert_block ((large_free_block *) v);
  }
}

/* Remove a given block from the free set. */
static void bf_remove (value v)
{
  mlsize_t wosz = Wosize_val (v);

  CAMLassert (Color_val (v) == Caml_blue);
  if (wosz <= BF_NUM_SMALL){
    while (*bf_small_fl[wosz].merge != v){
      CAMLassert (*bf_small_fl[wosz].merge < v);
      bf_small_fl[wosz].merge = &Next_small (*bf_small_fl[wosz].merge);
    }
    *bf_small_fl[wosz].merge = Next_small (v);
  }else{
    large_free_block *b = (large_free_block *) v;
    CAMLassert (bf_is_in_tree (b));
    CAMLassert (b->prev->next == b);
    CAMLassert (b->next->prev == b);
    if (b->isnode){
      large_free_block **p = bf_search (bf_large_wosize (b));
      CAMLassert (*p != NULL);
      if (b->next == b){
        bf_remove_node (p);
      }else{
        large_free_block *n = b->next;
        n->prev = b->prev;
        b->prev->next = n;
        *p = n;
        n->isnode = 1;
        n->left = b->left;
        n->right = b->right;
#ifdef DEBUG
        Field ((value) b, 0) = Debug_free_major;
        b->left = b->right = b->next = b->prev =
          (large_free_block *) Debug_free_major;
#endif
      }
    }else{
      b->prev->next = b->next;
      b->next->prev = b->prev;
    }
  }
}

/* Split the given block, return a new block of the given size.
   The remaining block is still at the same address, its size is changed
   and its color becomes black if its size is greater than 0.
*/
static header_t *bf_split_small (mlsize_t wosz, value v)
{
  intnat remwhsz = Whsize_val (v) - Whsize_wosize (wosz);

  CAMLassert (Wosize_val (v) >= wosz);
  if (remwhsz > Whsize_wosize (0)){
    Hd_val (v) =
      Make_header (Wosize_whsize (remwhsz), Abstract_tag, Caml_black);
    caml_fl_cur_wsz -= Whsize_wosize (wosz);
  }else{
    Hd_val (v) = Make_header (0, 0, Caml_white);
    caml_fl_cur_wsz -= Whsize_val (v);
  }
  return (header_t *) &Field (v, Wosize_whsize (remwhsz));
}

/* Split the given block, return a new block of the given size.
   The original block is at the same address but its size is changed.
   Its color and tag are changed as appropriate for calling the
   insert_fragment* functions.
*/
static header_t *bf_split (mlsize_t wosz, value v)
{
  header_t hd = Hd_val (v);
  mlsize_t remwhsz = Whsize_hd (hd) - Whsize_wosize (wosz);

  CAMLassert (Wosize_val (v) >= wosz);
  if (remwhsz <= Whsize_wosize (0)){
    Hd_val (v) = Make_header (0, 0, Caml_white);
    caml_fl_cur_wsz -= Whsize_hd (hd);
  }else if (remwhsz <= Whsize_wosize (BF_NUM_SMALL)){
    Hd_val (v) =
      Make_header (Wosize_whsize (remwhsz), Abstract_tag, Caml_black);
    caml_fl_cur_wsz -= Whsize_wosize (wosz);
  }else{
    Hd_val (v) =
      Make_header (Wosize_whsize (remwhsz), Abstract_tag, Caml_blue);
    caml_fl_cur_wsz -= Whsize_wosize (wosz);
  }
  return (header_t *) &Field (v, Wosize_whsize (remwhsz));
}

/* Allocate from a large block at [p]. If the node is single and the remaining
   size is greater than [bound], it stays at the same place in the tree.
*/
static header_t *bf_alloc_from_large (mlsize_t wosz, large_free_block **p,
                                      mlsize_t bound)
{
  large_free_block *n = *p;
  large_free_block *b;
  header_t *result;

  CAMLassert (bf_large_wosize (n) >= wosz);
  if (n->next == n){
    if (bf_large_wosize (n) > bound + wosz + 1){
      /* TODO splay at [n]? if the remainder is larger than [wosz]? */
      return bf_split (wosz, (value) n);
    }else{
      bf_remove_node (p);
      result = bf_split (wosz, (value) n);
      bf_insert_fragment ((value) n);
      return result;
    }
  }else{
    b = n->next;
    n->next = b->next;
    b->next->prev = n;
    result = bf_split (wosz, (value) b);
    /* TODO: splay at [n] if the remainder is smaller than [wosz] */
    bf_insert_fragment ((value) b);
    return result;
  }
}

static header_t *bf_allocate (mlsize_t wosz)
{
  value block;
  header_t *result;

  CAMLassert (sizeof (char *) == sizeof (value));
  CAMLassert (wosz >= 1);

#ifdef CAML_INSTR
  if (wosz < 10){
    ++instr_size[wosz];
  }else if (wosz < 100){
    ++instr_size[wosz/10 + 9];
  }else{
    ++instr_size[19];
  }
#endif /* CAML_INSTR */

  DEBUG_bf_check ();
  if (wosz <= BF_NUM_SMALL){
    if (bf_small_fl[wosz].free != Val_NULL){
      /* fast path: allocate from the corresponding free list */
      block = bf_small_fl[wosz].free;
      if (bf_small_fl[wosz].merge == &Next_small (bf_small_fl[wosz].free)){
        bf_small_fl[wosz].merge = &bf_small_fl[wosz].free;
      }
      bf_small_fl[wosz].free = Next_small (bf_small_fl[wosz].free);
      caml_fl_cur_wsz -= Whsize_wosize (wosz);
  DEBUG_bf_check ();
      return Hp_val (block);
    }else{
      /* allocate from the next available size */
      mlsize_t s = wosz + 1;
      while (1){
        if (s > BF_NUM_SMALL) break;
        if ((block = bf_small_fl[s].free) != Val_NULL){
          if (bf_small_fl[s].merge == &Next_small (bf_small_fl[s].free)){
            bf_small_fl[s].merge = &bf_small_fl[s].free;
          }
          bf_small_fl[s].free = Next_small (bf_small_fl[s].free);
          result = bf_split_small (wosz, block);
          if (s - wosz > 1) bf_insert_fragment_small (block);
          return result;
        }
        ++s;
      }
    }
    /* failed to find a suitable small block: allocate from the tree. */
    if (bf_large_tree == NULL) return NULL;
    bf_splay_least (&bf_large_tree);
    result = bf_alloc_from_large (wosz, &bf_large_tree, BF_NUM_SMALL);
  DEBUG_bf_check ();
    return result;
  }else{
    /* allocate a large block */
    large_free_block **n;
    mlsize_t bound;
    n = bf_search_best (wosz, &bound);
    if (n == NULL) return NULL;
    result = bf_alloc_from_large (wosz, n, bound);
  DEBUG_bf_check ();
    return result;
  }
}

static void bf_init_merge (void)
{
  mlsize_t i;

#ifdef CAML_INSTR
  for (i = 1; i < 20; i++){
    CAML_INSTR_INT (instr_name[i], instr_size[i]);
    instr_size[i] = 0;
  }
#endif /* CAML_INSTR */

  caml_fl_merge = Val_NULL;

  for (i = 1; i <= BF_NUM_SMALL; i++){
    /* At the beginning of each small free list is a segment of fragments
       that were pushed back to the list after splitting. These are either
       black or white, and they are not in order. We need to remove them
       from the list for coalescing to work. We set them white so they
       will be picked up by the sweeping code and inserted in the right
       place in the list.
    */
    value p = bf_small_fl[i].free;
    while (p != Val_NULL && Color_val (p) != Caml_blue){
      CAMLassert (Color_val (p) == Caml_white || Color_val (p) == Caml_black);
      Hd_val(p) = Whitehd_hd (Hd_val (p));
      caml_fl_cur_wsz -= Whsize_val (p);
      p = Next_small (p);
    }
    bf_small_fl[i].free = p;
    /* Set the merge pointer to its initial value */
    bf_small_fl[i].merge = &bf_small_fl[i].free;
  }
}

static void bf_reset (void)
{
  mlsize_t i;

  for (i = 1; i <= BF_NUM_SMALL; i++){
    bf_small_fl[i].free = Val_NULL;
    bf_small_fl[i].merge = &(bf_small_fl[i].free);
  }
  bf_large_tree = NULL;
  caml_fl_cur_wsz = 0;
  bf_init_merge ();
}

static header_t *bf_merge_block (value bp, char *limit)
{
  value start;
  value next;
  mlsize_t wosz;

  DEBUG_bf_check ();
  CAMLassert (Color_val (bp) == Caml_white);
  caml_fl_cur_wsz += Whsize_val (bp);
  /* Find the starting point of the current run of free blocks. */
  if (caml_fl_merge != Val_NULL && Next_in_mem (caml_fl_merge) == bp){
    start = caml_fl_merge;
    CAMLassert (Color_val (start) == Caml_blue);
    bf_remove (start);
  }else{
    start = bp;
  }
  next = bp;
  while ((char *) next <= limit){
    /* FIXME todo recognize (w+b)w* instead of (w|b)* */
    switch (Color_val (next)){
    case Caml_white:
      if (Tag_val (next) == Custom_tag){
        void (*final_fun)(value) = Custom_ops_val(next)->finalize;
        if (final_fun != NULL) final_fun(next);
      }
      caml_fl_cur_wsz += Whsize_val (next);
      break;
    case Caml_blue:
      bf_remove (next);
      break;
    case Caml_gray: case Caml_black:
      goto end_while;
    }
    next = Next_in_mem (next);
  }
 end_while:
  wosz = Wosize_whsize ((value *) next - (value *) start);
#ifdef DEBUG
  {
    value *p;
    for (p = (value *) start; p < (value *) next; p++){
      *p = Debug_free_major;
    }
  }
#endif
  while (wosz > Max_wosize){
    Hd_val (start) = Make_header (Max_wosize, 0, Caml_blue);
    bf_insert_sweep (start);
    start = Next_small (start);
    wosz -= Whsize_wosize (Max_wosize);
  }
  if (wosz > 0){
    Hd_val (start) = Make_header (wosz, 0, Caml_blue);
    bf_insert_sweep (start);
  }else{
    Hd_val (start) = Make_header (0, 0, Caml_white);
    caml_fl_cur_wsz -= Whsize_wosize (0);
  }
  DEBUG_bf_check ();
  return Hp_val (next);
}

static void bf_add_blocks (value bp)
{
  while (bp != Val_NULL){
    value next = Next_small (bp);
    mlsize_t wosz = Wosize_val (bp);

    caml_fl_cur_wsz += Whsize_wosize (wosz);
    if (wosz > BF_NUM_SMALL){
      bf_insert_block ((large_free_block *) bp);
    }else{
      Hd_val (bp) = Blackhd_hd (Hd_val (bp));
      bf_insert_fragment_small (bp);
    }
    bp = next;
  }
}

static void bf_make_free_blocks (value *p, mlsize_t size, int do_merge,
                                 int color)
{
  mlsize_t sz;

  while (size > 0){
    if (size > Whsize_wosize (Max_wosize)){
      sz = Whsize_wosize (Max_wosize);
    }else{
      sz = size;
    }
    if (do_merge){
      mlsize_t wosz = Wosize_whsize (sz);
      if (wosz == 0){
        color = Caml_white;
      }else if (wosz <= BF_NUM_SMALL){
        caml_fl_cur_wsz += Whsize_wosize (wosz);
        color = Caml_black;
      }else{
        caml_fl_cur_wsz += Whsize_wosize (wosz);
        color = Caml_blue;
      }
      *(header_t *)p = Make_header (wosz, Abstract_tag, color);
      bf_insert_fragment (Val_hp (p));
    }else{
      *(header_t *)p = Make_header (Wosize_whsize (sz), 0, color);
    }
    size -= sz;
    p += sz;
  }
}

/*********************** policy selection *****************************/

typedef enum caml_policy_t {
  policy_next_fit = 0,
  policy_first_fit = 1,
  policy_best_fit = 2,
} caml_policy_t;

caml_policy_t caml_allocation_policy = policy_next_fit;

/* These pointers are changed when switching between allocation
   policies. */
static header_t *(*p_allocate) (mlsize_t wo_sz) = &nf_allocate;
static void (*p_init_merge) (void) = &nf_init_merge;
static void (*p_reset) (void) = &nf_reset;
static header_t *(*p_merge_block) (value bp, char *limit) = &nf_merge_block;
static void (*p_add_blocks) (value bp) = &nf_add_blocks;
static void (*p_make_free_blocks)
  (value *p, mlsize_t size, int do_merge, int color)
  = &nf_make_free_blocks;
#ifdef DEBUG
static void (*p_check) (void) = &nf_check;
#endif

/********************* exported functions *****************************/

/* [caml_fl_allocate] does not set the header of the newly allocated block.
   The calling function must do it before any GC function gets called.
   [caml_fl_allocate] returns a head pointer, or NULL if no suitable block
   is found in the free set.
*/
header_t *caml_fl_allocate (mlsize_t wo_sz)
{
  return (*p_allocate) (wo_sz);
}

void caml_fl_init_merge (void)
{
  (*p_init_merge) ();
}

/* This is called by caml_compact_heap. */
void caml_fl_reset (void)
{
  (*p_reset) ();
}

/* [caml_fl_merge_block] returns the head pointer of the next block after [bp],
   because merging blocks may change the size of [bp]. */
header_t *caml_fl_merge_block (value bp, char *limit)
{
  return (*p_merge_block) (bp, limit);
}

/* [bp] must point to a list of blocks of wosize >= 1 chained by their field 0,
   terminated by Val_NULL, and field 1 of the first block must point to
   the last block.
   The blocks must be blue.
*/
void caml_fl_add_blocks (value bp)
{
  (*p_add_blocks) (bp);
}

/* Cut a block of memory into Max_wosize pieces, give them headers,
   and optionally merge them into the free list.
   arguments:
   p: pointer to the first word of the block
   size: size of the block (in words)
   do_merge: 1 -> do merge; 0 -> do not merge
   color: which color to give to the pieces; if [do_merge] is 1, this
          is overridden by the merge code, but we have historically used
          [Caml_white].
*/
void caml_make_free_blocks (value *p, mlsize_t size, int do_merge, int color)
{
  (*p_make_free_blocks) (p, size, do_merge, color);
}

void caml_set_allocation_policy (intnat p)
{
  switch (p){
  case policy_next_fit: default:
    caml_allocation_policy = policy_next_fit;
    p_allocate = &nf_allocate;
    p_init_merge = &nf_init_merge;
    p_reset = &nf_reset;
    p_merge_block = &nf_merge_block;
    p_add_blocks = &nf_add_blocks;
    p_make_free_blocks = &nf_make_free_blocks;
#ifdef DEBUG
    p_check = &nf_check;
#endif
    break;
  case policy_first_fit:
    caml_allocation_policy = policy_first_fit;
    p_allocate = &ff_allocate;
    p_init_merge = &ff_init_merge;
    p_reset = &ff_reset;
    p_merge_block = &ff_merge_block;
    p_add_blocks = &ff_add_blocks;
    p_make_free_blocks = &ff_make_free_blocks;
#ifdef DEBUG
    p_check = &ff_check;
#endif
    break;
  case policy_best_fit:
    caml_allocation_policy = policy_best_fit;
    p_allocate = &bf_allocate;
    p_init_merge = &bf_init_merge;
    p_reset = &bf_reset;
    p_merge_block = &bf_merge_block;
    p_add_blocks = &bf_add_blocks;
    p_make_free_blocks = &bf_make_free_blocks;
#ifdef DEBUG
    p_check = &bf_check;
#endif
    break;
  }
}

#ifdef DEBUG
void caml_fl_check (void)
{
  (*p_check) ();
}
#endif
