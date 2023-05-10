"use strict";

import biwa from "biwascheme";

export const _evalToForeign = left => right => src => {
  var first_error = "no_error"
  const interp = new biwa.Interpreter(err => {
    if (first_error === "no_error") {
      first_error = err;
    }
    return err; // This becomes the `result` returned from `interp.evaluate`
  });
  const result = interp.evaluate(src);
  if (first_error === "no_error") {
    return right(result);
  } else {
    return left(first_error.message);
  }
}

export const _applyClosure = left => right => lambda => args => {
  const interp = new biwa.Interpreter();
  try {
    return right(interp.invoke_closure(lambda, args));
  } catch (error) {
    return left(error.toString());
  }
}

export const _toString = biwa_object => {
  return biwa.to_write(biwa_object);
}

export const _toArrayInt = biwa_object => {
  return biwa_object;
}
