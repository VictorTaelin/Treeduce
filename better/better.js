global.rewrites = {total: 0};
function register(name) {
  if (!global.rewrites[name]) {
    global.rewrites[name] = 0;
  }
  global.rewrites[name]++;
  global.rewrites.total++;
}

function Memory() {
  return {
    val: [],
    use: [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]],
  };
}

function alloc(mem, size) {
  if (size === 0) {
    return 0;
  } else {
    var reused = mem.use[size].pop();
    if (reused !== undefined) {
      return reused;
    } else {
      var dest = mem.val.length;
      mem.val.length += size;
      return dest;
    }
  }
}

function make(mem, kind, vals) {
  var dest = alloc(mem, vals.length);
  for (var i = 0; i < vals.length; ++i) {
    mem.val[dest+i] = vals[i];
  }
  return ptr(kind, dest);
}

function make0(mem, kind) {
  return ptr(kind, 0);
}

function make1(mem, kind, val0) {
  var dest = alloc(mem, 1);
  mem.val[dest+0] = val0;
  return ptr(kind, dest);
}

function make2(mem, kind, val0, val1) {
  var dest = alloc(mem, 2);
  mem.val[dest+0] = val0;
  mem.val[dest+1] = val1;
  return ptr(kind, dest);
}

function make3(mem, kind, val0, val1, val2) {
  var dest = alloc(mem, 3);
  mem.val[dest+0] = val0;
  mem.val[dest+1] = val1;
  mem.val[dest+2] = val2;
  return ptr(kind, dest);
}

function make4(mem, kind, val0, val1, val2, val3) {
  var dest = alloc(mem, 4);
  mem.val[dest+0] = val0;
  mem.val[dest+1] = val1;
  mem.val[dest+2] = val2;
  mem.val[dest+3] = val4;
  return ptr(kind, dest);
}

function make5(mem, kind, val0, val1, val2, val3, val4) {
  var dest = alloc(mem, 4);
  mem.val[dest+0] = val0;
  mem.val[dest+1] = val1;
  mem.val[dest+2] = val2;
  mem.val[dest+3] = val4;
  mem.val[dest+4] = val4;
  return ptr(kind, dest);
}

function make6(mem, kind, val0, val1, val2, val3, val4, val5) {
  var dest = alloc(mem, 4);
  mem.val[dest+0] = val0;
  mem.val[dest+1] = val1;
  mem.val[dest+2] = val2;
  mem.val[dest+3] = val4;
  mem.val[dest+4] = val4;
  mem.val[dest+5] = val5;
  return ptr(kind, dest);
}

function make7(mem, kind, val0, val1, val2, val3, val4, val5, val6) {
  var dest = alloc(mem, 4);
  mem.val[dest+0] = val0;
  mem.val[dest+1] = val1;
  mem.val[dest+2] = val2;
  mem.val[dest+3] = val4;
  mem.val[dest+4] = val4;
  mem.val[dest+5] = val5;
  mem.val[dest+6] = val6;
  return ptr(kind, dest);
}

function make8(mem, kind, val0, val1, val2, val3, val4, val5, val6, val7) {
  var dest = alloc(mem, 4);
  mem.val[dest+0] = val0;
  mem.val[dest+1] = val1;
  mem.val[dest+2] = val2;
  mem.val[dest+3] = val4;
  mem.val[dest+4] = val4;
  mem.val[dest+5] = val5;
  mem.val[dest+6] = val6;
  mem.val[dest+7] = val7;
  return ptr(kind, dest);
}

function free(mem, dest, size) {
  mem.use[size].push(dest);
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

function collect(mem, term) {
  var arity = kind_to_arity[get_kind(term)];
  for (var i = 0; i < arity; ++i) {
    collect(mem, mem.val[get_dest(term) + i]);
  }
  free(mem, get_dest(term), arity);
}

function rewrite(mem, func) {
  switch (get_kind(func)) {
    case Inc:
      var arg0 = mem.val[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make0(mem,E);
        case O:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,I,arg0_0);
        case I:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make1(mem,O,make1(mem,Inc,arg0_0));
      }

    case Rev:
      var arg0 = mem.val[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          var arg1 = mem.val[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return arg1;
        case O:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          var arg1 = mem.val[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Rev,arg0_0,make1(mem,O,arg1));
        case I:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          var arg1 = mem.val[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Rev,arg0_0,make1(mem,I,arg1));
      }

    case Fn0:
      var arg0 = mem.val[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,make1(mem,Inc,make0(mem,E)),make0(mem,E),make0(mem,E));
        case O:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,make1(mem,Inc,make1(mem,O,arg0_0)),make0(mem,E),make0(mem,E));
        case I:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,make1(mem,Inc,make1(mem,I,arg0_0)),make0(mem,E),make0(mem,E));
      }

    case Fn1:
      var arg0 = mem.val[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          var arg1 = mem.val[get_dest(func)+1];
          var arg2 = mem.val[get_dest(func)+2];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Fn2,arg1,arg2);
        case O:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          var arg1 = mem.val[get_dest(func)+1];
          var arg2 = mem.val[get_dest(func)+2];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,arg0_0,make1(mem,O,arg1),make1(mem,O,arg2));
        case I:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          var arg1 = mem.val[get_dest(func)+1];
          var arg2 = mem.val[get_dest(func)+2];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make3(mem,Fn1,arg0_0,make1(mem,I,arg1),make1(mem,I,arg2));
      }

    case Fn2:
      var arg0 = mem.val[get_dest(func)+0];
      switch (get_kind(arg0)) {
        case E:
          var arg1 = mem.val[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return arg1;
        case O:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          var arg1 = mem.val[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          collect(mem, arg0_0);
          return make1(mem,Fn0,make2(mem,Rev,arg1,make0(mem,E)));
        case I:
          var arg0_0 = mem.val[get_dest(arg0)+0];
          var arg1 = mem.val[get_dest(func)+1];
          free(mem, get_dest(func), kind_to_arity[get_kind(func)]);
          free(mem, get_dest(arg0), kind_to_arity[get_kind(arg0)]);
          return make2(mem,Fn2,arg0_0,arg1);
      }

  }
  return 0;
}

function reduce(mem, term) {
  var stack = [{term, down:0}];
  while (stack.length > 0) {
    var {term, down} = stack[stack.length - 1];
    var norm = rewrite(mem, term);
    if (norm !== 0) {
      if (stack.length <= 1) {
        stack[stack.length - 1].term = norm;
        stack[stack.length - 1].down = 0;
      } else {
        stack.pop();
        var {term: parent_term, down: parent_down} = stack[stack.length - 1];
        mem.val[get_dest(parent_term) + parent_down] = norm;
      }
    } else {
      var arit = kind_to_arity[get_kind(term)];
      if (down < arit) {
        var next = mem.val[get_dest(term) + down];
        stack.push({term: next, down: 0});
      } else {
        if (stack.length <= 1) {
          break;
        } else {
          stack.pop();
          stack[stack.length - 1].down++;
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
      mem.val[dest + i] = argm;
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
    if (!kind) {
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
  if (ptr === 0) {
    return "~";
  } else {
    var text = "";
    text += "(";
    text += kind_to_name[get_kind(ptr)];
    for (var i = 0; i < kind_to_arity[get_kind(ptr)]; ++i) {
      text += " ";
      text += show(mem, mem.val[get_dest(ptr) + i]);
    }
    text += ")";
    return text;
  }
}

function count_used(mem) {
  var count = 0;
  for (var i = 0; i < mem.val.length; ++i) {
    if (mem.val[i]) count++;
  }
  return count;
}

var {mem, ptr: root} = read("(Fn0 (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (O (E))))))))))))))))))))");
console.log(show(mem, root));

var norm = reduce(mem, root);
console.log(show(mem, norm));

console.log("mem_len:", mem.val.length);
console.log("mem_use:", count_used(mem));
console.log("rewrite:", global.rewrites);
