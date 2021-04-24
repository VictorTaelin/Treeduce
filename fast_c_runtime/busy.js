// This is the same as busy.c, but in JavaScript

var rewrites = 0;

function Memory() {
  return {
    nodes: [],
    count: 0,
    reuse: [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]],
    freed: [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  };
}

function alloc(mem, size) {
  if (size === 0) {
    return 0;
  } else if (mem.freed[size] > 0) {
    return mem.reuse[size][--mem.freed[size]];
  } else {
    var dest = mem.count;
    mem.count += size;
    return dest;
  }
}

function freed(mem, dest, size) {
  mem.reuse[size][mem.freed[size]++] = dest;
}

// Garbage-collects
function collect(mem, term) {
  var arity = kind_to_arity[get_kind(term)];
  for (var i = 0; i < arity; ++i) {
    collect(mem, mem.nodes[get_dest(term) + i]);
  }
  freed(mem, get_dest(term), arity);
}

function ptr(kind, dest) {
  return kind | (dest << 8)
}

function get_kind(ptr) {
  return ptr & 0xFF;
}

function get_dest(ptr) {
  return ptr >>> 8;
}


function make0(mem, kind) {
  return ptr(kind, 0);
}

function make1(mem, kind, val0) {
  var dest = alloc(mem, 1);
  mem.nodes[dest+0] = val0;
  return ptr(kind, dest);
}

function make2(mem, kind, val0, val1) {
  var dest = alloc(mem, 2);
  mem.nodes[dest+0] = val0;
  mem.nodes[dest+1] = val1;
  return ptr(kind, dest);
}

function make3(mem, kind, val0, val1, val2) {
  var dest = alloc(mem, 3);
  mem.nodes[dest+0] = val0;
  mem.nodes[dest+1] = val1;
  mem.nodes[dest+2] = val2;
  return ptr(kind, dest);
}

function make4(mem, kind, val0, val1, val2, val3) {
  var dest = alloc(mem, 4);
  mem.nodes[dest+0] = val0;
  mem.nodes[dest+1] = val1;
  mem.nodes[dest+2] = val2;
  mem.nodes[dest+3] = val3;
  return ptr(kind, dest);
}

function make5(mem, kind, val0, val1, val2, val3, val4) {
  var dest = alloc(mem, 4);
  mem.nodes[dest+0] = val0;
  mem.nodes[dest+1] = val1;
  mem.nodes[dest+2] = val2;
  mem.nodes[dest+3] = val3;
  mem.nodes[dest+4] = val4;
  return ptr(kind, dest);
}

function make6(mem, kind, val0, val1, val2, val3, val4, val5) {
  var dest = alloc(mem, 4);
  mem.nodes[dest+0] = val0;
  mem.nodes[dest+1] = val1;
  mem.nodes[dest+2] = val2;
  mem.nodes[dest+3] = val3;
  mem.nodes[dest+4] = val4;
  mem.nodes[dest+5] = val5;
  return ptr(kind, dest);
}

function make7(mem, kind, val0, val1, val2, val3, val4, val5, val6) {
  var dest = alloc(mem, 4);
  mem.nodes[dest+0] = val0;
  mem.nodes[dest+1] = val1;
  mem.nodes[dest+2] = val2;
  mem.nodes[dest+3] = val3;
  mem.nodes[dest+4] = val4;
  mem.nodes[dest+5] = val5;
  mem.nodes[dest+6] = val6;
  return ptr(kind, dest);
}

function make8(mem, kind, val0, val1, val2, val3, val4, val5, val6, val7) {
  var dest = alloc(mem, 4);
  mem.nodes[dest+0] = val0;
  mem.nodes[dest+1] = val1;
  mem.nodes[dest+2] = val2;
  mem.nodes[dest+3] = val3;
  mem.nodes[dest+4] = val4;
  mem.nodes[dest+5] = val5;
  mem.nodes[dest+6] = val6;
  mem.nodes[dest+7] = val7;
  return ptr(kind, dest);
}

//data Nat = S Nat | Z deriving Show
//data Bits = O Bits | I Bits | E deriving Show
//data Pair a b = Make a b

var S = 1;
var Z = 2;
var O = 3;
var I = 4;
var E = 5;
var P = 6;
var Inc = 7;
var Add = 8;
var Cpy = 9;
var Map = 10;
var Slow = 11;
var SlowGo = 12;

var kind_to_arity = {
  [0]: 0,
  [S]: 1,
  [Z]: 0,
  [O]: 1,
  [I]: 1,
  [E]: 0,
  [P]: 2,
  [Inc]: 1,
  [Add]: 2,
  [Cpy]: 1,
  [Map]: 1,
  [Slow]: 1,
  [SlowGo]: 1,
};

var kind_to_name = {
  [0]: "Air",
  [S]: "S",
  [Z]: "Z",
  [O]: "O",
  [I]: "I",
  [E]: "E",
  [P]: "P",
  [Inc]: "Inc",
  [Add]: "Add",
  [Cpy]: "Cpy",
  [Map]: "Map",
  [Slow]: "Slow",
  [SlowGo]: "SlowGo",
}

var name_to_kind = {
  Air: 0,
  S: S,
  Z: Z,
  O: O,
  I: I,
  E: E,
  P: P,
  Inc: Inc,
  Add: Add,
  Cpy: Cpy,
  Map: Map,
  Slow: Slow,
  SlowGo: SlowGo,
};

//main :: IO ()
//main = print (slow (S (S (S (S Z)))))

// Applies a single rewrite
function rewrite(mem, func) {
  switch (get_kind(func)) {
    case Inc: {
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E: {
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make0(mem,E);
        }
        case O: {
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,I,arg0_0);
        }
        case I: {
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,O,make1(mem,Inc,arg0_0));
        }
      }
    }

    case Add: {
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E: {
          var arg1 = mem.nodes[get_dest(func)+1];
          switch (get_kind(arg1)) {
            case E: {
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make0(mem,E);
            }
            case O: {
              var arg1_0 = mem.nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,O,arg1_0);
            }
            case I: {
              var arg1_0 = mem.nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,I,arg1_0);
            }
          }
        }
        case O: {
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg1 = mem.nodes[get_dest(func)+1];
          switch (get_kind(arg1)) {
            case E: {
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,O,arg0_0);
            }
            case O: {
              var arg1_0 = mem.nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,O,make2(mem,Add,arg0_0,arg1_0));
            }
            case I: {
              var arg1_0 = mem.nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,I,make2(mem,Add,arg0_0,arg1_0));
            }
          }
        }
        case I: {
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg1 = mem.nodes[get_dest(func)+1];
          switch (get_kind(arg1)) {
            case E: {
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,I,arg0_0);
            }
            case O: {
              var arg1_0 = mem.nodes[get_dest(arg1)+0];
              freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
              freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
              freed(mem, get_dest(arg1), kind_to_arity[get_kind(arg1)]);
              return make1(mem,I,make2(mem,Add,arg0_0,arg1_0));
            }
            case I: {
              var arg1_0 = mem.nodes[get_dest(arg1)+0];
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
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case Z: {
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,P,make0(mem,Z),make0(mem,Z));
        }
        case S: {
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,Map,make1(mem,Cpy,arg0_0));
        }
      }
    }

    case Map: {
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case P: {
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg0_1 = mem.nodes[get_dest(arg0)+1];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,P,make1(mem,S,arg0_0),make1(mem,S,arg0_1));
        }
      }
    }

    case Slow: {
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case Z: {
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,I,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make1(mem,O,make0(mem,E)))))))))))))))))))))))))))))))));
        }
        case S: {
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,SlowGo,make1(mem,Cpy,arg0_0));
        }
      }
    }

    case SlowGo: {
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case P: {
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg0_1 = mem.nodes[get_dest(arg0)+1];
          freed(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          freed(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Add,make1(mem,Slow,arg0_0),make1(mem,Slow,arg0_1));
        }
      }
    }
  }
  return -1;
}

// Reduces a term to normal form
function reduce(mem, term) {
  var stack = [{term, down:0}];
  var count = 1;
  while (count > 0) {
    var term = stack[count - 1].term;
    var down = stack[count - 1].down;
    var norm = rewrite(mem, term);
    if (norm !== -1) {
      ++rewrites;
      if (count <= 1) {
        stack[count - 1].term = norm;
        stack[count - 1].down = 0;
      } else {
        --count;
        var parent_term = stack[count - 1].term;
        var parent_down = stack[count - 1].down;
        mem.nodes[get_dest(parent_term) + parent_down] = norm;
      }
    } else {
      var arit = kind_to_arity[get_kind(term)];
      if (down < arit) {
        var next = mem.nodes[get_dest(term) + down];
        stack[count++] = {term: next, down: 0};
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

// Parses a code to a memory
function parse(mem, str) {
  while (str[0] === " " || str[0] === "\n") {
    str = str.slice(1);
  }
  if (str[0] === "(") {
    var [str, kind] = parse(mem, str.slice(1));
    var arit = kind_to_arity[kind];
    if (arit === undefined) {
      throw "No parse.";
    }
    var dest = alloc(mem, arit);
    for (var i = 0; i < arit; ++i) {
      var [str, argm] = parse(mem, str);
      mem.nodes[dest + i] = argm;
    }
    if (str[0] !== ")") {
      throw "No parse.";
    }
    return [str.slice(1), ptr(kind, dest)];
  } else {
    var name = "";
    while (str !== "") {
      var chr = str.charCodeAt(0);
      if ( 48 <= chr && chr < 58
        || 65 <= chr && chr < 91
        || 97 <= chr && chr < 123 
        || chr === 46
        || chr === 95) {
        name += str[0];
        str = str.slice(1);
      } else {
        break;
      }
    }
    if (name.length === 0) {
      throw "No parse.";
    }
    var kind = name_to_kind[name];
    if (kind === undefined) {
      throw "No parse.";
    }
    return [str, kind];
  }
}

// Reads a code, returns the memory and root pointer
function read(code) {
  var mem = Memory();
  var [str,ptr] = parse(mem, code);
  return {mem, ptr};
}

// Stringifies the term stored at the pointer
function show(mem, ptr) {
  var text = "";
  function go(ptr) {
    if (ptr === 0) {
      text += "~";
    } else {
      //var text = "";
      text += "(";
      text += kind_to_name[get_kind(ptr)];
      for (var i = 0; i < kind_to_arity[get_kind(ptr)]; ++i) {
        text += " ";
        go(mem.nodes[get_dest(ptr) + i]);
      }
      text += ")";
    }
  }
  go(ptr);
  return text;
}

var {mem, ptr: root} = read("(Slow (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))))))");
console.log(show(mem, root));

var norm = reduce(mem, root);
console.log(show(mem, norm));

console.log("mem_len:", mem.count);
console.log("rewrite:", rewrites);
