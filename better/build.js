function build_rule(func, tree) {
  var tabs = n => n === 0 ? "" : " " + tabs(n - 1);
  var code = "";
  code += tabs(4) + "case " + func + ":\n";
  function elim(tree, n, tab, free, used) {
    if (Array.isArray(tree) || typeof tree === "string") {
      //code += tabs(4+n*2) + JSON.stringify(tree) + "\n";
      for (var i = 0; i < free.length; ++i) {
        code += tabs(tab) + "free(mem, get_dest("+free[i]+"), kind_to_arity[get_kind("+free[i]+")]);\n";
      }
      var done = make(tree, used);
      for (var arg in used) {
        if (used[arg] === false) {
          code += tabs(tab) + "collect(mem, "+arg+");\n";
        }
      }
      code += tabs(tab) + "return " + done + ";\n";
    } else {
      code += tabs(tab) + "var arg"+n+" = mem.nodes[get_dest(func)+"+n+"];\n";
      if (tree._) {
        elim(tree._("arg"+n), n + 1, tab, free, used);
      } else {
        code += tabs(tab) + "switch (get_kind(arg"+n+")) {\n";
        for (var ctor in tree) {
          code += tabs(tab+2) + "case "+ctor+":\n";
          //console.log(ctor, kind_to_arity[name_to_kind[ctor]]);
          var i = 0;
          var next = tree[ctor];
          while (typeof next === "function") {
          //for (var i = 0; i < kind_to_arity[name_to_kind[ctor]]; ++i) {
            code += tabs(tab+4)+"var arg"+n+"_"+i+" = mem.nodes[get_dest(arg"+n+")+"+i+"];\n";
            used["arg"+n+"_"+i] = false;
            next = next("arg"+n+"_"+i);
            ++i;
          }
          elim(next, n + 1, tab + 4, free.concat(["arg"+n]), used);
        }
        code += tabs(tab) + "}\n";
      }
    }
  }
  function make(node, used) {
    if (typeof node === "string") {
      if (used[node] === false) {
        used[node] = true;
      }
      return node;
    } else {
      var args = node.slice(1).map(x => make(x,used));
      var vals = args.length > 0 ? ","+args.join(",") : "";
      return "make"+(node.length-1)+"(mem,"+node[0]+vals+")";
    }
  }
  elim(tree, 0, 6, ["func"], {});
  return code;
}

var rules = {
  //"And": {
    //E: {
      //E: ["E"],
      //O: y => ["E"],
      //I: y => ["E"],
    //},
    //O: x => ({
      //E: ["E"],
      //O: y => ["O", ["And", x, y]],
      //I: y => ["O", ["And", x, y]],
    //}),
    //I: x => ({
      //E: ["E"],
      //O: y => ["O", ["And", x, y]],
      //I: y => ["I", ["And", x, y]],
    //}),
  //},
  "Inc": {
    E: ["E"],
    O: x => ["I", x],
    I: x => ["O", ["Inc", x]],
  },
  "Rev": {
    E: {
      _: r => r,
    },
    O: x => ({
      _: r => ["Rev", x, ["O", r]]
    }),
    I: x => ({
      _: r => ["Rev", x, ["I", r]],
    }),
  },
  "Fn0": {
    E: ["Fn1", ["Inc", ["E"]], ["E"], ["E"]],
    O: x => ["Fn1", ["Inc", ["O", x]], ["E"], ["E"]],
    I: x => ["Fn1", ["Inc", ["I", x]], ["E"], ["E"]],
  },
  "Fn1": {
    E: {
      _: y => ({
        _: z => ["Fn2", y, z]
      })
    },
    O: x => ({
      _: y => ({
        _: z => ["Fn1", x, ["O", y], ["O", z]],
      })
    }),
    I: x => ({
      _: y => ({
        _: z => ["Fn1", x, ["I", y], ["I", z]],
      })
    })
  },
  "Fn2": {
    E: ({
      _: y => y
    }),
    O: x => ({
      _: y => ["Fn0", ["Rev", y, ["E"]]],
    }),
    I: x => ({
      _: y => ["Fn2", x, y],
    })
  },
};


for (var fn in rules) {
  console.log(build_rule(fn, rules[fn]));
}

