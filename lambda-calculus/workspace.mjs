import * as LC from "./lib.mjs";
import { v, ap, ab } from "./lib.mjs";

const section = (s) => {
  console.log("");
  console.log(`===== ${s} =====`);
  console.log("");
};

const log = (label, e) => console.log(label + ": " + LC.display(e));
const trace = (n, e) => {
  let f = e;
  for (let i = 0; i < n; i++) {
    log(i, f);
    f = LC.reduce(f);
  }
};

/* Boolean logic */

section("boolean logic");

log("true:", LC.bool_t);
log("false:", LC.bool_f);

log("not", LC.bool_not);
log("and", LC.bool_and);
log("or", LC.bool_or);

log("if-then-else", LC.if_then_else);

console.log("");

console.log("t and (f or t)");
trace(
  13,
  ap(ap(LC.bool_and, LC.bool_t), ap(ap(LC.bool_or, LC.bool_f), LC.bool_t))
);

/* Linked lists */

section("linked lists");

log("nil", LC.nil);
log("cons", LC.cons);

console.log("");

log("is_null", LC.is_null);
log("head", LC.head);
log("tail", LC.tail);

console.log("");

log("singleton", LC.singleton);

console.log("");

const some_list = ap(
  ap(LC.cons, LC.bool_t),
  ap(ap(LC.cons, LC.bool_f), LC.nil)
);
log("some_list", some_list);

log("is_null some_list", LC.reduce_n(30, ap(LC.is_null, some_list)));
log("head some_list", LC.reduce_n(30, ap(LC.head, some_list)));

/* Church arithmetic */

section("church arithmetic");

log("+", LC.church_sum);
log("*", LC.church_prod);

console.log("");

log("0", LC.church_0);
log("1", LC.church_1);

const church_2 = ap(ap(LC.church_sum, LC.church_1), LC.church_1);
const church_3 = ap(ap(LC.church_sum, LC.church_1), church_2);

log("2", church_2);
log("3", church_3);

log("2 (reduced)", LC.reduce_n(6, church_2));
log("3 (reduced)", LC.reduce_n(12, church_3));

/* Recursion */

section("recursion");

log("y combinator", LC.y_combinator);

/* Combinations */

section("combinations");

const result = ap(
  ap(LC.append, ap(LC.singleton, LC.bool_t)),
  ap(LC.singleton, LC.bool_f)
);

log("head(append([t], [f]))", LC.reduce_n(33, ap(LC.head, result)));
log(
  "head(tail(apppend([t],[f])))",
  LC.reduce_n(55, ap(LC.head, ap(LC.tail, result)))
);
log(
  "is_null(tail(tail(append([t],[f]))))",
  LC.reduce_n(60, ap(LC.is_null, ap(LC.tail, ap(LC.tail, result))))
);

console.log("");

log("replicate", LC.replicate);
