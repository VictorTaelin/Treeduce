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

function free(mem, dest, size) {
  mem.reuse[size][mem.freed[size]++] = dest;
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

//inc E     = E
//inc (O x) = (I x)
//inc (I x) = O (inc x)

//add E     E     = E
//add E     (O b) = O b
//add E     (I b) = I b
//add (O a) E     = O a
//add (O a) (O b) = O (add a b)
//add (O a) (I b) = I (add a b)
//add (I a) E     = I a
//add (I a) (O b) = I (add a b)
//add (I a) (I b) = O (inc (add a b))

//dup Z     = Make Z Z
//dup (S x) = mapS (dup x)

//mapS (Make a b) = Make (S a) (S b)

//slow Z     = I (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O E)))))))))))))))
//slow (S x) = slowGo (dup x)

//slowGo (Make a b) = add (slow a) (slow b)

//main :: IO ()
//main = print (slow (S (S (S (S Z)))))

var O   = 1;
var I   = 2;
var E   = 3;
var Inc = 4;
var Rev = 5;
var Fn0 = 6;
var Fn1 = 7;
var Fn2 = 8;

var kind_to_arity = {
  [O]: 1,
  [I]: 1,
  [E]: 0,
  [Inc]: 1,
  [Rev]: 2,
  [Fn0]: 1,
  [Fn1]: 3,
  [Fn2]: 2,
};

var kind_to_name = {
  [O]: "O",
  [I]: "I",
  [E]: "E",
  [Inc]: "Inc",
  [Rev]: "Rev",
  [Fn0]: "Fn0",
  [Fn1]: "Fn1",
  [Fn2]: "Fn2",
}

var name_to_kind = {
  O: O,
  I: I,
  E: E,
  Inc: Inc,
  Rev: Rev,
  Fn0: Fn0,
  Fn1: Fn1,
  Fn2: Fn2,
};

// Garbage-collects
function collect(mem, term) {
  var arity = kind_to_arity[get_kind(term)];
  for (var i = 0; i < arity; ++i) {
    collect(mem, mem.nodes[get_dest(term) + i]);
  }
  free(mem, get_dest(term), arity);
}

// Applies a single rewrite
function rewrite(mem, func) {
  switch (get_kind(func)) {
    case Inc:
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make0(mem,E);
        case O:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,I,arg0_0);
        case I:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,O,make1(mem,Inc,arg0_0));
      }

    case Rev:
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          var arg1 = mem.nodes[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return arg1;
        case O:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg1 = mem.nodes[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Rev,arg0_0,make1(mem,O,arg1));
        case I:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg1 = mem.nodes[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Rev,arg0_0,make1(mem,I,arg1));
      }

    case Fn0:
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,make1(mem,Inc,make0(mem,E)),make0(mem,E),make0(mem,E));
        case O:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,make1(mem,Inc,make1(mem,O,arg0_0)),make0(mem,E),make0(mem,E));
        case I:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,make1(mem,Inc,make1(mem,I,arg0_0)),make0(mem,E),make0(mem,E));
      }

    case Fn1:
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          var arg1 = mem.nodes[get_dest(func)+1];
          var arg2 = mem.nodes[get_dest(func)+2];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Fn2,arg1,arg2);
        case O:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg1 = mem.nodes[get_dest(func)+1];
          var arg2 = mem.nodes[get_dest(func)+2];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,arg0_0,make1(mem,O,arg1),make1(mem,O,arg2));
        case I:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg1 = mem.nodes[get_dest(func)+1];
          var arg2 = mem.nodes[get_dest(func)+2];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,arg0_0,make1(mem,I,arg1),make1(mem,I,arg2));
      }

    case Fn2:
      var arg0 = mem.nodes[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          var arg1 = mem.nodes[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return arg1;
        case O:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg1 = mem.nodes[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          collect(mem, arg0_0);
          return make1(mem,Fn0,make2(mem,Rev,arg1,make0(mem,E)));
        case I:
          var arg0_0 = mem.nodes[get_dest(arg0)+0];
          var arg1 = mem.nodes[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Fn2,arg0_0,arg1);
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

var {mem, ptr: root} = read("(Fn0 (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (E))))))))))))))))))))))))");
console.log(show(mem, root));

var norm = reduce(mem, root);
console.log(show(mem, norm));

console.log("mem_len:", mem.freed);
//console.log("rewrite:", global.rewrites);
