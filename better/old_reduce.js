
//function reduce(mem, term) {
  //var arity = kind_to_arity[get_kind(term)];
  //if (arity === 0) {
    //return {cont: false, term};
  //} else {
    //var norm = rewrite(mem, term);
    //if (norm !== 0) {
      //return {cont: true, term: norm};
    //} else {
      //for (let i = 0; i < arity; ++i) {
        //var dest = get_dest(term);
        //var done = reduce(mem, mem.val[dest + i]);
        //if (done.cont) {
          //mem.val[dest + i] = done.term;
          //return reduce(mem, term);
        //}
      //}
      //return {cont: false, term};
    //}
  //}
//}

//function normal(mem, term) {
  //var done = reduce(mem, term);
  //while (done.cont) {
    //done = reduce(mem, done.term);
  //}
  //return done.term;
//}

