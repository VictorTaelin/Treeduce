// This file is a prototypal runtime for a subset of Haskell (a functional
// language) that consists only of datatypes and linear pattern-matching
// equations. This file in particular was generated from this program:
//
// data Nat        = S Nat | Z deriving show
// data Bits       = O Bits | I Bits | E deriving show
// data Pair a b   = P a b deriving show
// inc E           = E
// inc (O x)       = (I x)
// inc (I x)       = O (inc x)
// add E     E     = E
// add E     (O b) = O b
// add E     (I b) = I b
// add (O a) E     = O a
// add (O a) (O b) = O (add a b)
// add (O a) (I b) = I (add a b)
// add (I a) E     = I a
// add (I a) (O b) = I (add a b)
// add (I a) (I b) = O (inc (add a b))
// dup Z           = P Z Z
// dup (S x)       = mapS (dup x)
// mapS (P a b)    = P (S a) (S b)
// slow Z          = (I (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O E))))))))))))))))))))))))))))))))
// slow (S x)      = slowGo (dup x)
// slowGo (P a b)  = add (slow a) (slow b)
// main            = print (P (slow (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))) (slow (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))
//
// The program above defines natural numbers, bistrings and pairs using Haskell
// algebraic datatypes, implements inc and add functions for the bitstrings, and
// then uses a recursive function ("slow") to add the number one 2^20 times to
// itself. Notice how no lambdas are used (this is a low-order runtime), and how
// variables are used at most once (this is a linear runtime). 
//
// This file is the compilation of the program above to C. It includes a
// prototypal runtime for such a language. It works by storing constructors and
// functions as tagless nodes in a 32-bit memory heap. Each node has N pointers,
// one for each of its fields (children). The tag is stored on the 8 last bits
// of the pointer, not on the memory. So, for example, the term "(add A B)" is
// represented as a pointer to a block of 64 bits on memory. That block contains
// 1 32-bit pointer to A, and 1 32-bit pointer to B. The "ADD" tag is stored on
// the pointer to "(add A B)`. In other words, `(add A B)` can be created like:
//
// u32 add_A_B(Memory *mem, u32 idx, u32 ptr_to_A, u32 ptr_to_B) {
//   mem.nodes[idx + 0] = ptr_to_A;
//   mem.nodes[idx + 1] = ptr_to_B;
//   return (idx << 8) | ADD;
// }
//
// This file includes a simple memory allocator that stores a buffer of 32-bit
// pointers and its length, and 16 freelists, one for each possible node size
// (max node size = 16 ptrs), to reclaim freed memory. It includes a rewrite()
// function that applies the rewrite equations above at a given index of the
// memory, and a reduce() function that rewrites the memory to normal form using
// a DFS to locate redexes. It also includes a simple parser and stringifier.
//
// The main function creates a pair of two elements: `P (slow 20) (slow 20)`.
// The "slow" function, when applied to the natural number "20", results in the
// bit-string "0x0000000000000001" being added to itself a total of 2^20 times.
// This is done two times. This takes 5.4 seconds on my computer (Apple M1
// processor), with a total of almost 84 million pattern-matches ("rewrites").
// Note that we're only encoding bitstrings as datatypes to stress-test the
// performance of the general-purpose pattern-matching engine, of course adding
// native integers would be much faster, that's not the point. For comparison,
// the Haskell program above returns in 4.5 seconds, so we're already 83% as
// fast as GHC for this example! But I want moar.
//
// The goal is to find ways to optimize this file in order to make it return
// faster and thus increase the number of rewrites per second. It would be
// amazing to use pthreads. For example, each call to `(slow 20)` is technically
// independent, so, in theory, using pthreads to reduce each one in a core
// should make this file almost 2x faster. But in order to do so, the allocator
// must use thread-safe stacks, or something equivalent.
//
// Note that this file was compiled from the specific program above, so it isn't
// a general-purpose runtime. The idea is that, by optimizing this file
// specifically, we'll be able to use the same techniques when generating files
// from different programs.
//
// Questions: should I avoid writing my own memory manager and use malloc/free
// instead? Should I use integer types other than uint32_t? Should I use structs
// (with bitfields?) for pointers, instead of doing bitwise operators? Is there
// anything else I'm doing "wrong" that could be improved? If you think you can
// help us with this challenge, let me know!

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define LENGTH(arr) (sizeof (arr) / sizeof (arr)[0])

// Pointers are 32-bit unsigned ints
typedef uint32_t u32;

// Used for 
static u32 rewrites;

// The global memory representation
typedef struct {
  u32* nodes; // the global memory, storing nodes, each node with many u32 ptrs
  u32  count; // the length of the memory (how many 32 ptrs it allocated)
  u32* reuse[16]; // reuse[L] is an array of freed memory block with size = L
  u32  freed[16]; // freed[L] counts freed memory blocks with size = L
} Memory;

// Initializes the memory
void init_memory(Memory* mem, u32 max_nodes, u32 max_reuse) {
  mem->nodes = malloc(sizeof *mem->nodes * max_nodes);
  mem->count = 0;
  for (int i = 0; i < 16; ++i) {
    mem->reuse[i] = malloc(sizeof *mem->reuse[i] * max_reuse);
    mem->freed[i] = 0;
  }
}

// Frees the memory
void free_memory(Memory* mem) {
  free(mem->nodes);
  for (int i = 0; i < 16; ++i) {
    free(mem->reuse[i]);
  }
}

// Allocs a node of a given size
u32 alloc(Memory* mem, u32 size) {
  // If the size 0, we don't need to allocate anything
  if (size == 0) {
    return 0;
  // If there is a block with that size marked for reuse, return it
  } else if (mem->freed[size] > 0) {
    return mem->reuse[size][--mem->freed[size]];
  // Otherwise, allocs at the end of the memory and increments its length
  } else {
    u32 dest = mem->count;
    mem->count += size;
    return dest;
  }
}

// Frees a memory block, marking it for reuse
void freed(Memory* mem, u32 dest, u32 size) {
  mem->reuse[size][mem->freed[size]++] = dest;
}

// A pointer is a 8-bit tag (kind) plus a 24-bit destination
u32 ptr(u32 kind, u32 dest) {
  return kind | (dest << 8);
}

// Gets the tag (kind) of a pointer
u32 get_kind(u32 ptr) {
  return ptr & 0xFF;
}

// Gets the destination of a pointer (i.e., its target index on mem.value)
u32 get_dest(u32 ptr) {
  return ptr >> 8;
}

// Creates a node with 0 fields (no allocation needed)
u32 make0(Memory* mem, u32 kind) {
  return ptr(kind, 0);
}

// Allocates a node with 1 field
u32 make1(Memory* mem, u32 kind, u32 val0) {
  u32 dest = alloc(mem, 1);
  mem->nodes[dest+0] = val0;
  return ptr(kind, dest);
}

// Allocates a node with 2 field
u32 make2(Memory* mem, u32 kind, u32 val0, u32 val1) {
  u32 dest = alloc(mem, 2);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  return ptr(kind, dest);
}

// Allocates a node with 3 fields
u32 make3(Memory* mem, u32 kind, u32 val0, u32 val1, u32 val2) {
  u32 dest = alloc(mem, 3);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  mem->nodes[dest+2] = val2;
  return ptr(kind, dest);
}

// Allocates a node with 4 fields
u32 make4(Memory* mem, u32 kind, u32 val0, u32 val1, u32 val2, u32 val3) {
  u32 dest = alloc(mem, 4);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  mem->nodes[dest+2] = val2;
  mem->nodes[dest+3] = val3;
  return ptr(kind, dest);
}

// Allocates a node with 5 fields
u32 make5(Memory* mem, u32 kind, u32 val0, u32 val1, u32 val2, u32 val3, u32 val4) {
  u32 dest = alloc(mem, 4);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  mem->nodes[dest+2] = val2;
  mem->nodes[dest+3] = val3;
  mem->nodes[dest+4] = val4;
  return ptr(kind, dest);
}

// Allocates a node with 6 fields
u32 make6(Memory* mem, u32 kind, u32 val0, u32 val1, u32 val2, u32 val3, u32 val4, u32 val5) {
  u32 dest = alloc(mem, 4);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  mem->nodes[dest+2] = val2;
  mem->nodes[dest+3] = val3;
  mem->nodes[dest+4] = val4;
  mem->nodes[dest+5] = val5;
  return ptr(kind, dest);
}

// Allocates a node with 7 fields
u32 make7(Memory* mem, u32 kind, u32 val0, u32 val1, u32 val2, u32 val3, u32 val4, u32 val5, u32 val6) {
  u32 dest = alloc(mem, 4);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  mem->nodes[dest+2] = val2;
  mem->nodes[dest+3] = val3;
  mem->nodes[dest+4] = val4;
  mem->nodes[dest+5] = val5;
  mem->nodes[dest+6] = val6;
  return ptr(kind, dest);
}

// Allocates a node with 8 fields
u32 make8(Memory* mem, u32 kind, u32 val0, u32 val1, u32 val2, u32 val3, u32 val4, u32 val5, u32 val6, u32 val7) {
  u32 dest = alloc(mem, 4);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  mem->nodes[dest+2] = val2;
  mem->nodes[dest+3] = val3;
  mem->nodes[dest+4] = val4;
  mem->nodes[dest+5] = val5;
  mem->nodes[dest+6] = val6;
  mem->nodes[dest+7] = val7;
  return ptr(kind, dest);
}

// Tags (kinds) for the constructors and functions of the compiled program
const u32 S      = 1;
const u32 Z      = 2;
const u32 O      = 3;
const u32 I      = 4;
const u32 E      = 5;
const u32 P      = 6;
const u32 Inc    = 7;
const u32 Add    = 8;
const u32 Cpy    = 9;
const u32 Map    = 10;
const u32 Slow   = 11;
const u32 SlowGo = 12;
const u32 MAX    = 13;

// Gets the arity (number of fields) of a kind
const u32 kind_to_arity[] = {
  0, // Air
  1, // S
  0, // Z
  1, // O
  1, // I
  0, // E
  2, // P
  1, // Inc
  2, // Add
  1, // Cpy
  1, // Map
  1, // Slow
  1, // SlowGo
};

// Gets the name of a kind
const char* kind_to_name[] = {
  "Air",
  "S",
  "Z",
  "O",
  "I",
  "E",
  "P",
  "Inc",
  "Add",
  "Cpy",
  "Map",
  "Slow",
  "SlowGo",
};

// Gets the kind of a name (used on the parser)
u32 name_to_kind(char* name) {
  for (size_t i = 0; i < LENGTH(kind_to_name); ++i) {
    if (strcmp(name, kind_to_name[i]) == 0) {
      return i;
    }
  }
  return -1;
}

// Garbage-collects a node and all of its children recursivelly
// Since there is no sharing, we don't need reference counting. As long as a
// node goes out of scope it is safe for collection.
void collect(Memory* mem, u32 term) {
  u32 arity = kind_to_arity[get_kind(term)];
  for (u32 i = 0; i < arity; ++i) {
    collect(mem, mem->nodes[get_dest(term) + i]);
  }
  freed(mem, get_dest(term), arity);
}

// Applies a single rewrite at memory (func is a ptr to the function node)
// So, for example, if we apply `rewrite(mem, func)` with `func` being a pointer
// to where `(inc (O (O (O (E)))))` is stored, it results in `(I (O (O (E))`.
// The `inc` and upper `O` nodes on the input are freed, and this function
// returns a pointer to the root node of the output `(I ...)`. If `func` isn't
// a redex, this returns -1.
// This function is basically a case tree, doing the required rewrite if a match
// is found. It was generated automatically, I won't fully comment it. If there
// is any idea on how to optimize it, run "build.js" to generate it again with
// the idealized changes.
u32 rewrite(Memory* mem, u32 func) {
  switch (get_kind(func)) {
    case Inc: {
      u32 arg0 = mem->nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E: {
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make0(mem,E);
        }
        case O: {
          u32 arg0_0 = mem->nodes[get_dest(arg0)+0];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,I,arg0_0);
        }
        case I: {
          u32 arg0_0 = mem->nodes[get_dest(arg0)+0];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,O,make1(mem,Inc,arg0_0));
        }
      }
    }

    case Add: {
      u32 arg0 = mem->nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E: {
          u32 arg1 = mem->nodes[get_dest(func)+1];
          switch (get_kind(arg1)) {
            case E: {
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make0(mem,E);
            }
            case O: {
              u32 arg1_0 = mem->nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,O,arg1_0);
            }
            case I: {
              u32 arg1_0 = mem->nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,I,arg1_0);
            }
          }
        }
        case O: {
          u32 arg0_0 = mem->nodes[get_dest(arg0)+0];
          u32 arg1 = mem->nodes[get_dest(func)+1];
          switch (get_kind(arg1)) {
            case E: {
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,O,arg0_0);
            }
            case O: {
              u32 arg1_0 = mem->nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,O,make2(mem,Add,arg0_0,arg1_0));
            }
            case I: {
              u32 arg1_0 = mem->nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,I,make2(mem,Add,arg0_0,arg1_0));
            }
          }
        }
        case I: {
          u32 arg0_0 = mem->nodes[get_dest(arg0)+0];
          u32 arg1 = mem->nodes[get_dest(func)+1];
          switch (get_kind(arg1)) {
            case E: {
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,I,arg0_0);
            }
            case O: {
              u32 arg1_0 = mem->nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,I,make2(mem,Add,arg0_0,arg1_0));
            }
            case I: {
              u32 arg1_0 = mem->nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,O,make1(mem,Inc,make2(mem,Add,arg0_0,arg1_0)));
            }
          }
        }
      }
    }

    case Cpy: {
      u32 arg0 = mem->nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case Z: {
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,P,make0(mem,Z),make0(mem,Z));
        }
        case S: {
          u32 arg0_0 = mem->nodes[get_dest(arg0)+0];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,Map,make1(mem,Cpy,arg0_0));
        }
      }
    }

    case Map: {
      u32 arg0 = mem->nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case P: {
          u32 arg0_0 = mem->nodes[get_dest(arg0)+0];
          u32 arg0_1 = mem->nodes[get_dest(arg0)+1];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,P,make1(mem,S,arg0_0),make1(mem,S,arg0_1));
        }
      }
    }

    case Slow: {
      u32 arg0 = mem->nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case Z: {
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,I,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make0(mem,E)))))))))))))))))))))))))))))))));
        }
        case S: {
          u32 arg0_0 = mem->nodes[get_dest(arg0)+0];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,SlowGo,make1(mem,Cpy,arg0_0));
        }
      }
    }

    case SlowGo: {
      u32 arg0 = mem->nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case P: {
          u32 arg0_0 = mem->nodes[get_dest(arg0)+0];
          u32 arg0_1 = mem->nodes[get_dest(arg0)+1];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Add,make1(mem,Slow,arg0_0),make1(mem,Slow,arg0_1));
        }
      }
    }
  }
  return -1;
}

typedef struct {
  u32 term; // pointer to a term
  u32 down; // next child we must visit (or child we're visiting)
} Cursor;

// Reduces a term to normal form by performing a DFS, walking through the tree,
// searching for redexes, reducing and jumping back, until fully reduced. 
u32 reduce(Memory *mem, u32 term) {
  static Cursor stack[65536];
  u32 count = 1;
  stack[0].term = term;
  stack[0].down = 0;
  // While there are nodes to visit
  while (count > 0) {
    // Gets the last node
    u32 term = stack[count - 1].term;
    u32 down = stack[count - 1].down;
    // Attempts to rewrite it
    u32 norm = rewrite(mem, term);
    // If it was rewritten...
    if (norm != -1) {
      rewrites++;
      // If this is the root node, replace it by the result
      if (count <= 1) {
        stack[count - 1].term = norm;
        stack[count - 1].down = 0;
      // Otherwise, update its parent to point to the result and go to it
      } else {
        --count;
        u32 parent_term = stack[count - 1].term;
        u32 parent_down = stack[count - 1].down;
        mem->nodes[get_dest(parent_term) + parent_down] = norm;
      }
    // If it was not rewritten...
    } else {
      u32 arit = kind_to_arity[get_kind(term)];
      // Move to the first non-normalized children 
      // (Optimization: parallelize here?)
      if (down < arit) {
        u32 next = mem->nodes[get_dest(term) + down];
        stack[count].term = next;
        stack[count].down = 0;
        count++;
      // If all children are normalized
      } else {
        // If this is the root node, we're done!
        if (count <= 1) {
          break;
        // Otherwise, go to its parent and continue
        } else {
          --count;
          stack[count - 1].down++;
        }
      }
    }
  }
  // Returns the pointer to the normalized term
  return stack[0].term;
}

typedef struct {
  char* str;
  u32 res;
} Reply;

// Parses a term, adding it to memory
Reply parse(Memory *mem, char *str) {
  Reply reply;
  while (str[0] == ' ' || str[0] == '\n') {
    str = str + 1;
  }
  if (str[0] == '(') {
    reply = parse(mem, str + 1);
    str = reply.str;
    u32 kind = reply.res;
    if (kind >= MAX) exit(-1);
    u32 arit = kind_to_arity[kind];
    u32 dest = alloc(mem, arit);
    for (u32 i = 0; i < arit; ++i) {
      reply = parse(mem, str);
      str = reply.str;
      u32 argm = reply.res;
      mem->nodes[dest + i] = argm;
    }
    if (str[0] != ')') exit(-1);
    reply.str = str + 1;
    reply.res = ptr(kind, dest);
    return reply;
  } else {
    char name[256];
    u32 count = 0;
    while (str[0] != '\0') {
      char chr = str[0];
      if ( '0' <= chr && chr <= '9'
        || 'a' <= chr && chr <= 'z'
        || 'A' <= chr && chr <= 'Z' 
        || chr == '_' || chr == '.') {
        name[count++] = str[0];
        str = str + 1;
      } else {
        break;
      }
    }
    name[count] = '\0';
    if (count == 0) exit(-1);
    u32 kind = name_to_kind(name);
    if (kind == -1) exit(-1);
    reply.str = str;
    reply.res = kind;
    return reply;
  }
}
// Reads a term and allocates the memory for it
u32 read(Memory* mem, char* code) {
  //printf("...a\n");
  init_memory(mem, 0x10000000, 0x1000000);
  //printf("...b\n");
  Reply reply = parse(mem, code);
  //printf("...c\n");
  return reply.res;
}

// Stringifies a term
void show(Memory* mem, u32 ptr, char* text, u32* count) {
  if (ptr == 0) {
    text[(*count)++] = '~';
  } else {
    //var text = "";
    text[(*count)++] = '(';
    const char* name = kind_to_name[get_kind(ptr)];
    for (u32 i = 0; name[i] != '\0'; ++i) {
      text[(*count)++] = name[i];
    }
    u32 arit = kind_to_arity[get_kind(ptr)];
    for (u32 i = 0; i < arit; ++i) {
      text[(*count)++] = ' ';
      show(mem, mem->nodes[get_dest(ptr) + i], text, count);
    }
    text[(*count)++] = ')';
  }
}

// Main function creates a pair of two bitstrings, each generated by calling
// `(slow 20)`, which, in turn, adds the 64-bit bitstring to itself 2^20 times.
int main() {
  Memory mem;
  char shown[65536];
  u32 main, count = 0;

  main = read(&mem, "(P (Slow (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z)))))))))))))))))))))) (Slow (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z)))))))))))))))))))))))");
  main = reduce(&mem, main);

  show(&mem, main, shown, &count);
  
  printf("result: %s\n", shown);
  printf("mem_len: %d\n", mem.count);
  printf("rewrite: %d\n", rewrites);
}

