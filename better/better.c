#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define LENGTH(arr) (sizeof (arr) / sizeof (arr)[0])

typedef uint32_t u32;

static u32 rewrites;

typedef struct {
  u32* nodes;
  u32  count;
  u32* reuse[16];
  u32  freed[16];
} Memory;

void init_memory(Memory* mem, u32 max_nodes, u32 max_reuse) {
  mem->nodes = (u32*) malloc(sizeof(u32) * max_nodes);
  mem->count = 0;
  for (int i = 0; i < 16; ++i) {
    mem->reuse[i] = (u32*) malloc(sizeof(u32) * max_reuse);
    mem->freed[i] = 0;
  }
}

void free_memory(Memory* mem) {
  free(mem->nodes);
  for (int i = 0; i < 16; ++i) {
    free(mem->reuse[i]);
  }
}

u32 alloc(Memory* mem, u32 size) {
  if (size == 0) {
    return 0;
  } else if (mem->freed[size] > 0) {
    return mem->reuse[size][--mem->freed[size]];
  } else {
    u32 dest = mem->count;
    mem->count += size;
    return dest;
  }
}

void freed(Memory* mem, u32 dest, u32 size) {
  mem->reuse[size][mem->freed[size]++] = dest;
}

u32 ptr(u32 kind, u32 dest) {
  return kind | (dest << 8);
}

u32 get_kind(u32 ptr) {
  return ptr & 0xFF;
}

u32 get_dest(u32 ptr) {
  return ptr >> 8;
}

u32 make0(Memory* mem, u32 kind) {
  return ptr(kind, 0);
}

u32 make1(Memory* mem, u32 kind, u32 val0) {
  u32 dest = alloc(mem, 1);
  mem->nodes[dest+0] = val0;
  return ptr(kind, dest);
}

u32 make2(Memory* mem, u32 kind, u32 val0, u32 val1) {
  u32 dest = alloc(mem, 2);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  return ptr(kind, dest);
}

u32 make3(Memory* mem, u32 kind, u32 val0, u32 val1, u32 val2) {
  u32 dest = alloc(mem, 3);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  mem->nodes[dest+2] = val2;
  return ptr(kind, dest);
}

u32 make4(Memory* mem, u32 kind, u32 val0, u32 val1, u32 val2, u32 val3) {
  u32 dest = alloc(mem, 4);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  mem->nodes[dest+2] = val2;
  mem->nodes[dest+3] = val3;
  return ptr(kind, dest);
}

u32 make5(Memory* mem, u32 kind, u32 val0, u32 val1, u32 val2, u32 val3, u32 val4) {
  u32 dest = alloc(mem, 4);
  mem->nodes[dest+0] = val0;
  mem->nodes[dest+1] = val1;
  mem->nodes[dest+2] = val2;
  mem->nodes[dest+3] = val3;
  mem->nodes[dest+4] = val4;
  return ptr(kind, dest);
}

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

const u32 S = 1;
const u32 Z = 2;
const u32 O = 3;
const u32 I = 4;
const u32 E = 5;
const u32 P = 6;
const u32 Inc = 7;
const u32 Add = 8;
const u32 Cpy = 9;
const u32 Map = 10;
const u32 Slow = 11;
const u32 SlowGo = 12;
const u32 MAX = 13;

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

u32 name_to_kind(char* name) {
  for (size_t i = 0; i < LENGTH(kind_to_name); ++i) {
    if (strcmp(name, kind_to_name[i]) == 0) {
      return i;
    }
  }
  return -1;
}

// Garbage-collects
void collect(Memory* mem, u32 term) {
  u32 arity = kind_to_arity[get_kind(term)];
  for (u32 i = 0; i < arity; ++i) {
    collect(mem, mem->nodes[get_dest(term) + i]);
  }
  freed(mem, get_dest(term), arity);
}

// Applies a single rewrite
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
  u32 term;
  u32 down;
} Cursor;

// Reduces a term to normal form
u32 reduce(Memory *mem, u32 term) {
  static Cursor stack[65536];
  u32 count = 1;
  stack[0].term = term;
  stack[0].down = 0;
  while (count > 0) {
    u32 term = stack[count - 1].term;
    u32 down = stack[count - 1].down;
    u32 norm = rewrite(mem, term);
    if (norm != -1) {
      rewrites++;
      if (count <= 1) {
        stack[count - 1].term = norm;
        stack[count - 1].down = 0;
      } else {
        --count;
        u32 parent_term = stack[count - 1].term;
        u32 parent_down = stack[count - 1].down;
        mem->nodes[get_dest(parent_term) + parent_down] = norm;
      }
    } else {
      u32 arit = kind_to_arity[get_kind(term)];
      if (down < arit) {
        u32 next = mem->nodes[get_dest(term) + down];
        stack[count].term = next;
        stack[count].down = 0;
        count++;
      } else {
        if (count <= 1) {
          break;
        } else {
          --count;
          stack[count - 1].down++;
        }
      }
    }
  }
  return stack[0].term;
}

typedef struct {
  char* str;
  u32 res;
} Reply;

// Parses a code to a memory
Reply parse(Memory *mem, char *str) {
  //printf("parsing...\n");
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
// Reads a code, returns root pointer
u32 read(Memory* mem, char* code) {
  //printf("...a\n");
  init_memory(mem, 0x10000000, 0x1000000);
  //printf("...b\n");
  Reply reply = parse(mem, code);
  //printf("...c\n");
  return reply.res;
}

// Stringifies the term stored at the pointer
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

//var {mem, ptr: root} = read("(Fn0 (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (E))))))))))))))))))))");
//console.log(show(mem, root));

//var norm = reduce(mem, root);
//console.log(show(mem, norm));

//console.log("mem_len:", mem.freed);
////console.log("rewrite:", global.rewrites);

int main() {
  Memory mem;
  char text[65536];
  u32 main, count = 0;

  main = read(&mem, "(Slow (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z))))))))))))))))))))))");
  main = reduce(&mem, main);

  show(&mem, main, text, &count);
  
  printf("result: %s\n", text);
  printf("mem_len: %d\n", mem.count);
  printf("rewrite: %d\n", rewrites);
}

