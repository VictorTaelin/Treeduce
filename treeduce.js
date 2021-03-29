function is_var(str) {
  return str[0] === "#";
};

function match(pattern, term, vars = {}) {
  if (typeof pattern === "string" && is_var(pattern[0])) {
    vars[pattern] = term;
    return vars;
  } else if (typeof pattern === "string" && typeof term === "string") {
    return pattern === term ? vars : null;
  } else if (typeof pattern === "object" && typeof term === "object" && pattern.length === term.length) {
    for (var i = 0; i < pattern.length; ++i) {
      var vars = match(pattern[i], term[i], vars);
      if (vars === null) {
        return null;
      }
    }
    return vars;
  } else {
    return null;
  }
};

function build(pattern, vars) {
  if (typeof pattern === "string" && is_var(pattern[0])) {
    if (vars[pattern]) {
      return vars[pattern];
    } else {
      throw "Unbound variable: " + pattern;
    }
  } else if (typeof pattern === "string") {
    return pattern;
  } else {
    var done = [];
    for (var i = 0; i < pattern.length; ++i) {
      done.push(build(pattern[i], vars));
    }
    return done;
  }
};

function rewrite(rules, term) {
  for (var [pattern, result] of rules) {
    var vars = match(pattern, term);
    if (vars) {
      var old = term;
      term = build(result, vars);
      if (DEBUG) {
        console.log(":: " + show_term(old) + " ~> " + show_term(term));
      }
      ++REWRITES;
      return {diff: true, term};
    }
  }
  return {diff: false, term};
};

var CALLS = 0;
var REWRITES = 0;
var DEBUG = false;
function reduce(rules, term) {
  ++CALLS;
  if (DEBUG) {
    console.log("reduce", show_term(term));
  }
  // Attempts to rewrite term
  var rewt = rewrite(rules, term);
  if (rewt.diff) {
    return rewt;
  } else {
    if (typeof term === "string") {
      return {diff: false, term};
    } else {
      for (let i = 0; i < term.length; ++i) {
        var done = reduce(rules, term[i]);
        if (done.diff) {
          term[i] = done.term;
          return reduce(rules, term);
        }
      }
      return {diff: false, term};
    }
  }
}
  
function normal(rules, term) {
  var done = reduce(rules, term);
  while (done.diff) {
    done = reduce(rules, done.term);
  }
  return done.term;
};

function show_term(term) {
  if (typeof term === "string") {
    return term;
  } else {
    var str = "";
    for (var i = 0; i < term.length; ++i) {
      str += i === 0 ? "(" : " ";
      str += show_term(term[i]); 
    }
    str += ")";
    return str;
  }
};

function parse_term(str) {
  while (str[0] === " " || str[0] === "=") {
    str = str.slice(1);
  }
  if (str[0] === "(") {
    var term = [];
    while (str[0] !== ")") {
      var [str, argm] = parse_term(str.slice(1));
      term.push(argm);
    }
    return [str.slice(1), term];
  } else {
    var term = "";
    while (str !== "" && str[0] !== " " && str[0] !== ")") {
      term += str[0];
      str = str.slice(1);
    }
    return [str, term];
  }
};

function read_term(str) {
  return parse_term(str)[1];
};

function read_rules(str) {
  var rules = [];
  var lines = str.split("\n");
  for (var line of lines) {
    if (line.length > 0) {
      var [line, pattern] = parse_term(line);
      var [line, result]  = parse_term(line);
      rules.push([pattern, result]);
    }
  };
  return rules;
};

function eval(code, {debug} = {}) {
  CALLS = 0;
  REWRITES = 0;
  DEBUG = debug;
  var term = read_term("main");
  var rules = read_rules(code);
  return {
    norm: show_term(normal(rules, term)),
    calls: CALLS,
    rewrites: REWRITES,
  }
};

module.exports = {
  is_var,
  match,
  build,
  rewrite,
  reduce,
  normal,
  show_term,
  parse_term,
  read_term,
  read_rules,
  eval,
};
