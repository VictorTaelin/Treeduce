// clang -O3 -L/usr/local/lib/mimalloc-1.7 -I/usr/local/include/mimalloc-1.7 -rpath /usr/local/lib/mimalloc-1.7 -lmimalloc -o busy_threads busy_threads.c

// This file is just like busy.c, but I changed it to use C's malloc.
// But it is 3x slower. Will try adding threads when it is as fast as.

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

#include <pthread.h>
#include <mimalloc.h>



#define LENGTH(arr) (sizeof (arr) / sizeof (arr)[0])

// Pointers are 32-bit unsigned ints
typedef uint64_t u64;

// Used for 
static u64 rewrites;

// The global memory representation
//typedef struct {
  //u32* nodes; // the global memory, storing nodes, each node with many u32 ptrs
  //u32  count; // the length of the memory (how many 32 ptrs it allocated)
  //u32* reuse[16]; // reuse[L] is an array of freed memory block with size = L
  //u32  freed[16]; // freed[L] counts freed memory blocks with size = L
//} Memory;

// Initializes the memory
//void init_memory(Memory* mem, u32 max_nodes, u32 max_reuse) {
  //mem->nodes = malloc(sizeof *mem->nodes * max_nodes);
  //mem->count = 0;
  //for (int i = 0; i < 16; ++i) {
    //mem->reuse[i] = malloc(sizeof *mem->reuse[i] * max_reuse);
    //mem->freed[i] = 0;
  //}
//}

// Frees the memory
//void free_memory(Memory* mem) {
  //free(mem->nodes);
  //for (int i = 0; i < 16; ++i) {
    //free(mem->reuse[i]);
  //}
//}

// Frees a memory block, marking it for reuse
//void freed(Memory* mem, u32 dest, u32 size) {
  //mem->reuse[size][mem->freed[size]++] = dest;
//}



//typedef struct Node Node;
//typedef union Ptr Ptr;

//struct Node {
  //Ptr *args;
//};

//union Ptr {
  //u64 kind;
  //Ptr* args;
//};

typedef u64 Ptr;

void print(Ptr ptr); // TODO REMOVE

// A pointer is a 8-bit tag (kind) plus a 24-bit destination
Ptr ptr(u64 kind, Ptr *args) {
  //Ptr ptr;
  //ptr.kind = kind << 48;
  //ptr.args = args;
  u64 val = *(u64*)&args;
  return kind | (val << 8);
}

// Gets the tag (kind) of a pointer
u64 get_kind(Ptr ptr) {
  return ptr & 0xFF;
}

// Gets the destination of a pointer (i.e., its target index on mem.value)
Ptr* get_args(Ptr ptr) {
  u64 val = ptr >> 8;
  return *(Ptr**)&val;
  //u64 val = *((u64*)&ptr.args) & 0xFFFFFFFFFFFF;
  //return (*(Ptr**)&val);
}

// Allocs a node of a given size
Ptr* alloc(u64 size) {
  return mi_malloc(sizeof (Ptr) * size);
}

// Creates a node with 0 fields (no allocation needed)
Ptr make0(u64 kind) {
  return ptr(kind, 0);
}

// Allocates a node with 1 field
Ptr make1(u64 kind, Ptr val0) {
  Ptr* args = mi_malloc(sizeof (Ptr) * 1);
  args[0] = val0;
  return ptr(kind, args);
  
  //Ptr ptr;
  //ptr.kind = kind << 56;
  //ptr.args = malloc(sizeof (Ptr) * 1);
  //ptr.args[0] = val0;
  //return ptr;
}

// Allocates a node with 2 fields
Ptr make2(u64 kind, Ptr val0, Ptr val1) {
  Ptr* args = mi_malloc(sizeof (Ptr) * 2);
  args[0] = val0;
  args[1] = val1;
  return ptr(kind, args);
  //Ptr ptr;
  //ptr.kind = kind << 56;
  //ptr.args = malloc(sizeof (Ptr) * 2);
  //return ptr;
}

// Tags (kinds) for the constructors and functions of the compiled program
enum {
  Air,
  S,
  Z,
  O,
  I,
  E,
  P,
  Inc,
  Add,
  Cpy,
  Map,
  Slow,
  SlowGo,
  MAX,
};

// Gets the arity (number of fields) of a kind
const u64 kind_to_arity[] = {
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
u64 name_to_kind(char* name) {
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
//void collect(Ptr term) {
  //u64 arity = kind_to_arity[get_kind(term)];
  //for (u64 i = 0; i < arity; ++i) {
    //collect(term.args[i]);
    ////mem, mem->nodes[get_dest(term) + i]);
    ////collect(mem, mem->nodes[get_dest(term) + i]);
  //}
  //free(term.args); 
//}

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
Ptr rewrite(Ptr func) {
  switch (get_kind(func)) {
    case Inc: {
      Ptr arg0 = get_args(func)[0];
      switch (get_kind(arg0)) {
        case E: {
          mi_free(get_args(func));
          mi_free(get_args(arg0));
          return make0(E);
        }
        case O: {
          Ptr arg0_0 = get_args(arg0)[0];
          mi_free(get_args(func));
          mi_free(get_args(arg0));
          return make1(I,arg0_0);
        }
        case I: {
          Ptr arg0_0 = get_args(arg0)[0];
          mi_free(get_args(func));
          mi_free(get_args(arg0));
          return make1(O,make1(Inc,arg0_0));
        }
      }
      break;
    }
    case Add: {
      Ptr arg0 = get_args(func)[0];
      switch (get_kind(arg0)) {
        case E: {
          Ptr arg1 = get_args(func)[1];
          switch (get_kind(arg1)) {
            case E: {
              mi_free(get_args(func));
              mi_free(get_args(arg0));
              mi_free(get_args(arg1));
              return make0(E);
            }
            case O: {
              Ptr arg1_0 = get_args(arg1)[0];
              mi_free(get_args(func));
              mi_free(get_args(arg0));
              mi_free(get_args(arg1));
              return make1(O,arg1_0);
            }
            case I: {
              Ptr arg1_0 = get_args(arg1)[0];
              mi_free(get_args(func));
              mi_free(get_args(arg0));
              mi_free(get_args(arg1));
              return make1(I,arg1_0);
            }
          }
          break;
        }
        case O: {
          Ptr arg0_0 = get_args(arg0)[0];
          Ptr arg1 = get_args(func)[1];
          switch (get_kind(arg1)) {
            case E: {
              mi_free(get_args(func));
              mi_free(get_args(arg0));
              mi_free(get_args(arg1));
              return make1(O,arg0_0);
            }
            case O: {
              Ptr arg1_0 = get_args(arg1)[0];
              mi_free(get_args(func));
              mi_free(get_args(arg0));
              mi_free(get_args(arg1));
              return make1(O,make2(Add,arg0_0,arg1_0));
            }
            case I: {
              Ptr arg1_0 = get_args(arg1)[0];
              mi_free(get_args(func));
              mi_free(get_args(arg0));
              mi_free(get_args(arg1));
              return make1(I,make2(Add,arg0_0,arg1_0));
            }
          }
          break;
        }
        case I: {
          Ptr arg0_0 = get_args(arg0)[0];
          Ptr arg1 = get_args(func)[1];
          switch (get_kind(arg1)) {
            case E: {
              mi_free(get_args(func));
              mi_free(get_args(arg0));
              mi_free(get_args(arg1));
              return make1(I,arg0_0);
            }
            case O: {
              Ptr arg1_0 = get_args(arg1)[0];
              mi_free(get_args(func));
              mi_free(get_args(arg0));
              mi_free(get_args(arg1));
              return make1(I,make2(Add,arg0_0,arg1_0));
            }
            case I: {
              Ptr arg1_0 = get_args(arg1)[0];
              mi_free(get_args(func));
              mi_free(get_args(arg0));
              mi_free(get_args(arg1));
              return make1(O,make1(Inc,make2(Add,arg0_0,arg1_0)));
            }
          }
          break;
        }
      }
      break;
    }
    case Cpy: {
      Ptr arg0 = get_args(func)[0];
      switch (get_kind(arg0)) {
        case Z: {
          mi_free(get_args(func));
          mi_free(get_args(arg0));
          return make2(P,make0(Z),make0(Z));
        }
        case S: {
          Ptr arg0_0 = get_args(arg0)[0];
          mi_free(get_args(func));
          mi_free(get_args(arg0));
          return make1(Map,make1(Cpy,arg0_0));
        }
      }
      break;
    }
    case Map: {
      Ptr arg0 = get_args(func)[0];
      switch (get_kind(arg0)) {
        case P: {
          Ptr arg0_0 = get_args(arg0)[0];
          Ptr arg0_1 = get_args(arg0)[1];
          mi_free(get_args(func));
          mi_free(get_args(arg0));
          return make2(P,make1(S,arg0_0),make1(S,arg0_1));
        }
      }
      break;
    }
    case Slow: {
      Ptr arg0 = get_args(func)[0];
      switch (get_kind(arg0)) {
        case Z: {
          mi_free(get_args(func));
          mi_free(get_args(arg0));
          return make1(I,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make1(O,make0(E)))))))))))))))))))))))))))))))));
        }
        case S: {
          Ptr arg0_0 = get_args(arg0)[0];
          mi_free(get_args(func));
          mi_free(get_args(arg0));
          return make1(SlowGo,make1(Cpy,arg0_0));
        }
      }
      break;
    }
    case SlowGo: {
      Ptr arg0 = get_args(func)[0];
      switch (get_kind(arg0)) {
        case P: {
          Ptr arg0_0 = get_args(arg0)[0];
          Ptr arg0_1 = get_args(arg0)[1];
          mi_free(get_args(func));
          mi_free(get_args(arg0));
          return make2(Add,make1(Slow,arg0_0),make1(Slow,arg0_1));
        }
      }
      break;
    }
  }
  return ptr(MAX,NULL);
}

typedef struct {
  Ptr term; // pointer to a term
  u64 down; // next child we must visit (or child we're visiting)
} Cursor;

// Reduces a term to normal form by performing a DFS, walking through the tree,
// searching for redexes, reducing and jumping back, until fully reduced. 
Ptr reduce(Ptr term) {
  Cursor stack[8192];
  u64 count = 1;
  stack[0].term = term;
  stack[0].down = 0;
  // While there are nodes to visit
  while (count > 0) {
    //// Gets the last node
    Ptr term = stack[count - 1].term;

    //printf("reduce: ");
    //print(term);
    //printf("\n");

    u64 down = stack[count - 1].down;
    // Attempts to rewrite it
    Ptr norm = rewrite(term);
    // If it was rewritten...
    if (get_kind(norm) != MAX) {
      rewrites++;
      // If this is the root node, replace it by the result
      if (count <= 1) {
        stack[count - 1].term = norm;
        stack[count - 1].down = 0;
      // Otherwise, update its parent to point to the result and go to it
      } else {
        --count;
        Ptr parent_term = stack[count - 1].term;
        u64 parent_down = stack[count - 1].down;
        get_args(parent_term)[parent_down] = norm;
      }
    // If it was not rewritten...
    } else {
      u64 arit = kind_to_arity[get_kind(term)];
      // Move to the first non-normalized children 
      // (Optimization: parallelize here?)
      if (down < arit) {
        Ptr next = get_args(term)[down];
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
  union {
    u64 u64_res;
    Ptr ptr_res;
  };
} Reply;

// Parses a term, adding it to memory
Reply parse(char *str) {
  Reply reply;
  while (str[0] == ' ' || str[0] == '\n') {
    str = str + 1;
  }
  if (str[0] == '(') {
    reply = parse(str + 1);
    str = reply.str;
    u64 kind = reply.u64_res;
    if (kind >= MAX) exit(-1);
    u64 arit = kind_to_arity[kind];
    Ptr* args = alloc(arit);
    for (u64 i = 0; i < arit; ++i) {
      reply = parse(str);
      str = reply.str;
      Ptr argm = reply.ptr_res;
      args[i] = argm;
    }
    if (str[0] != ')') exit(-1);
    reply.str = str + 1;
    reply.ptr_res = ptr(kind, args);
    return reply;
  } else {
    char name[256];
    u64 count = 0;
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
    u64 kind = name_to_kind(name);
    if (kind == -1) exit(-1);
    reply.str = str;
    reply.u64_res = kind;
    return reply;
  }
}

// Reads a term and allocates the memory for it
Ptr read(char* code) {
  //printf("...a\n");
  //init_memory(mem, 0x10000000, 0x1000000);
  //printf("...b\n");
  Reply reply = parse(code);
  //printf("...c\n");
  return reply.ptr_res;
}

// Stringifies a term
void show(Ptr ptr, char* text, u64* count) {
  if (get_kind(ptr) == 0) {
    text[(*count)++] = '~';
  } else {
    //var text = "";
    text[(*count)++] = '(';
    const char* name = kind_to_name[get_kind(ptr)];
    for (u64 i = 0; name[i] != '\0'; ++i) {
      text[(*count)++] = name[i];
    }
    u64 arit = kind_to_arity[get_kind(ptr)];
    for (u64 i = 0; i < arit; ++i) {
      //text[(*count)++] = ' ';
      show(get_args(ptr)[i], text, count);
    }
    text[(*count)++] = ')';
  }
}

void print(Ptr ptr) {
  static char shown[65536];
  for (int i = 0; i < 65536; ++i) {
    shown[i] = '\0';
  }
  u64 count = 0;
  show(ptr, shown, &count);
  printf("%s", shown);
}

void *reducer(void *data) {
  Ptr *term = data;
  *term = reduce(*term);
  return 0;
}

// Main function creates a pair of two bitstrings, each generated by calling
// `(slow 20)`, which, in turn, adds the 64-bit bitstring to itself 2^20 times.
int main() {
  printf("hi\n");
  //printf("%llu\n", get_kind(ptr(Z,0)));

  //Ptr main = read("(P (Slow (S (S (Z)))) (Slow (S (S (Z)))))");
  Ptr main = read("(P(P(P(P(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z)))))))))))))))))))))))(P(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))))(P(P(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z)))))))))))))))))))))))(P(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z)))))))))))))))))))))))))(P(P(P(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z)))))))))))))))))))))))(P(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))))(P(P(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z)))))))))))))))))))))))(P(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))(Slow(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(Z))))))))))))))))))))))))))");
  print(get_args(main)[29]);
  // 6 7 8 9 12 13 14 15 20 21 22 23 26 27 28 29


  // MULTI THREADED

  const int threads = 16;
  const int indices[] = {6, 7, 8, 9, 12, 13, 14, 15, 20, 21, 22, 23, 26, 27, 28, 29};
  pthread_t thread[threads];
  Ptr vals[threads];
  for (int i = 0; i < threads; ++i) {
    vals[i] = get_args(main)[indices[i]];
  }
  for (int i = 0; i < threads; ++i) {
    pthread_create(&thread[i], NULL, &reducer, &vals[i]);
  }
  for (int i = 0; i < threads; ++i) {
    pthread_join(thread[i], NULL);
  }
  for (int i = 0; i < threads; ++i) {
    get_args(main)[indices[i]] = vals[i];
  }

  // SINGLE THREADED

  //main = reduce(main);

  // PRINTS RESULT

  printf("\n[RESULT]\n");
  print(main);
  printf("\n");
}

//(P(P(P(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E))))))))))))))))))))))))))))))))))(P(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))))(P(P(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E))))))))))))))))))))))))))))))))))(P(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E))))))))))))))))))))))))))))))))))))
//(P(P(P(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E))))))))))))))))))))))))))))))))))(P(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))))(P(P(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E))))))))))))))))))))))))))))))))))(P(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E)))))))))))))))))))))))))))))))))(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(O(I(O(O(O(O(O(O(O(O(O(O(O(E))))))))))))))))))))))))))))))))))))
