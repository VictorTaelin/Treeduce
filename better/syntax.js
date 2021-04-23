function parse_sexp(str) {
  while (/\s/.test(str[0])) {
    str = str.slice(1);
  }
  if (str[0] === "(") {
    var term = [];
    while (str[0] !== ")") {
      var [str, argm] = parse_sexp(str.slice(1));
      if (argm !== "") {
        term.push(argm);
      }
    }
    return [str.slice(1), term];
  } else {
    var term = "";
    while (str !== "" && /[\w\-<>_]/.test(str[0])) {
      term += str[0];
      str = str.slice(1);
    }
    return [str, term];
  }
};

function show_sexp(sexp) {
  if (typeof sexp === "string") {
    return sexp;
  } else {
    return "("+sexp.map(show_sexp).join(" ")+")";
  }
};

module.exports = {
  parse_sexp,
  show_sexp,
}
