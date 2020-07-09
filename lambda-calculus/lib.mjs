/* Common strings */
const types = {
  variable: "variable",
  abstraction: "abstraction",
  application: "application",
};

/* Helpers to construct different kinds of expression objects */
export const v = (name) => ({ type: types.variable, name });
export const ab = (x, body) => ({ type: types.abstraction, x, body });
export const ap = (func, arg) => ({ type: types.application, func, arg });

/* Expression type tests */
export const is_abstraction = (e) => e.type == types.abstraction;

/* A helper function for doing case analysis on expressions */
export const expr = (on_var, on_abstract, on_ap) => (e) => {
  switch (e.type) {
    case types.variable:
      return on_var(e.name);
    case types.abstraction:
      return on_abstract(e.x, e.body);
    case types.application:
      return on_ap(e.func, e.arg);
    default:
      console.log(e);
      throw "invalid type";
  }
};

/* Expression pretty printing */
export const display = expr(
  (name) => name,
  (x, body) => accumulate_ab([x], body),
  (f, x) => accumulate_ap(f, [x])
);

const accumulate_ab = (args, body) =>
  expr(
    (name) => display_ab(args, name),
    (x, next) => {
      args.push(x);
      return accumulate_ab(args, next);
    },
    (f, x) => display_ab(args, accumulate_ap(f, [x]))
  )(body);

const display_ab = (args, body) => {
  if (args.length == 1) {
    return `(${args[0]} => ${body})`;
  } else {
    return `((${args.join(", ")}) => ${body}))`;
  }
};

const accumulate_ap = (head, spine) =>
  expr(
    (name) => display_ap(name, spine),
    (x, body) => display_ap(accumulate_ab([x], body), spine),
    (f, x) => {
      spine.unshift(x);
      return accumulate_ap(f, spine);
    }
  )(head);

const display_ap = (head, spine) => `(${head} ${spine.map(display).join(" ")})`;

/* The height function measuring the complexity of an expression */
export const height = expr(
  (_n) => 1,
  (_x, body) => height(body) + 1,
  (f, x) => Math.max(height(f), height(x)) + 1
);

/* Calculate all variables that show up in an expression */
export const all_vars = expr(
  (name) => new Set([name]),
  (x, body) => {
    let vs = all_vars(body);
    vs.add(x);
    return vs;
  },
  (f, x) => {
    let vs = all_vars(f);
    all_vars(x).forEach((v) => vs.add(v));
    return vs;
  }
);

/* Calculate the free variables in an expression */
export const free_vars = expr(
  (name) => new Set([name]),
  (x, body) => {
    let vs = free_vars(body);
    vs.delete(x);
    return vs;
  },
  (f, x) => {
    let vs = free_vars(f);
    free_vars(x).forEach((v) => vs.add(v));
    return vs;
  }
);

/* Test if the expresssion is in normal form */
export const is_normal_form = expr(
  (_n) => true,
  (_x, _body) => true,
  (f, x) => !is_abstraction(f) && is_normal_form(f) && is_normal_form(x)
);

/* Perform an alpha-transform, renaming variables */
export const alpha = (name_0, name_1, e) =>
  expr(
    (name) => (name == name_0 ? v(name_1) : v(name)),
    (x, body) => ab(x == name_0 ? name_1 : x, alpha(name_0, name_1, body)),
    (f, x) => ap(alpha(name_0, name_1, f), alpha(name_0, name_1, x))
  )(e);

// Easy and dirty search for an unused name
const new_var_name = (vs) => {
  let i = 0;
  let x = `x${i}`;
  while (vs.has(x)) {
    i++;
    x = `x${i}`;
  }
  return x;
};

/* Perform beta substitution (allowing for shadowing) */
export const beta = (name, val, e) => {
  let vs = free_vars(e);
  return expr(
    (var_name) => (var_name == name ? val : e),
    (x, body) => {
      if (x == name) {
        return e;
      } else if (vs.has(x)) {
        // The binding is one of the free variables in our replacement
        let avoidance_set = all_vars(body);
        vs.forEach((v) => avoidance_set.add(v));
        let y = new_var_name(avoidance_set);
        return ab(y, beta(name, val, alpha(x, y, body)));
      } else {
        return ab(x, beta(name, val, body));
      }
    },
    (f, x) => ap(beta(name, val, f), beta(name, val, x))
  )(e);
};

/* Reduce an expression by reducing the leftmost, outermost redex */
export const reduce = expr(
  (name) => v(name),
  (x, body) => ab(x, reduce(body)),
  (f, x) => {
    if (is_abstraction(f)) {
      return beta(f.x, x, f.body);
    } else if (is_normal_form(f)) {
      return ap(f, reduce(x));
    } else {
      return ap(reduce(f), x);
    }
  }
);

/* Perform several reduction steps */
export const reduce_n = (n, e) => {
  let f = e;
  for (let i = 0; i < n; i++) {
    f = reduce(f);
  }
  return f;
};

/* Common variables */

const var_x = v("x");
const var_y = v("y");
const var_n = v("n");
const var_m = v("m");
const var_f = v("f");

/* Boolean logic */

export const bool_t = ab("x", ab("y", var_x));
export const bool_f = ab("x", ab("y", var_y));

export const bool_not = ab(
  "b1",
  ab("x", ab("y", ap(ap(v("b1"), var_y), var_x)))
);
export const bool_and = ab(
  "b1",
  ab(
    "b2",
    ab("x", ab("y", ap(ap(v("b1"), ap(ap(v("b2"), var_x), var_y)), var_y)))
  )
);
export const bool_or = ab(
  "b1",
  ab(
    "b2",
    ab("x", ab("y", ap(ap(v("b1"), var_x), ap(ap(v("b2"), var_x), var_y))))
  )
);

export const if_then_else = ab(
  "b",
  ab("if_t", ab("if_f", ap(ap(v("b"), v("if_t")), v("if_f"))))
);
export const f_if = (b, if_t, if_f) => ap(ap(ap(if_then_else, b), if_t), if_f);

/* Linked lists */

export const nil = ab("f", ap(ap(ap(var_f, bool_t), var_x), var_x));
export const cons = ab(
  "h",
  ab("t", ab("f", ap(ap(ap(var_f, bool_f), v("h")), v("t"))))
);

export const is_null = ab("xs", ap(v("xs"), ab("z", ab("h", ab("t", v("z"))))));
export const head = ab("xs", ap(v("xs"), ab("z", ab("h", ab("t", v("h"))))));
export const tail = ab("xs", ap(v("xs"), ab("z", ab("h", ab("t", v("t"))))));

export const singleton = ab("z", ap(ap(cons, v("z")), nil));

/* Church arithmetic */

export const church_sum = ab(
  "n",
  ab("m", ab("f", ab("x", ap(ap(var_n, var_f), ap(ap(var_m, var_f), var_x)))))
);
export const church_prod = ab(
  "n",
  ab("m", ab("f", ab("x", ap(ap(var_n, ap(var_m, var_f)), var_x))))
);

export const church_0 = ab("f", ab("x", var_x));
export const church_1 = ab("f", ab("x", ap(var_f, var_x)));

/* Recursion */

export const y_combinator = ab(
  "f",
  ap(ab("x", ap(var_f, ap(var_x, var_x))), ab("x", ap(var_f, ap(var_x, var_x))))
);

/* Combinations */

export const outer_append = ab(
  "f",
  ab(
    "xs",
    ab(
      "ys",
      f_if(
        ap(is_null, v("xs")),
        v("ys"),
        ap(
          ap(cons, ap(head, v("xs"))),
          ap(ap(var_f, ap(tail, v("xs"))), v("ys"))
        )
      )
    )
  )
);
export const append = ap(y_combinator, outer_append);

export const replicate = ab(
  "n",
  ab("x", ap(ap(var_n, ab("y", ap(ap(cons, var_x), var_y))), nil))
);
