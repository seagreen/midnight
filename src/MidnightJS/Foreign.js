"use strict";

import * as MidnightJS from '../MidnightJS/index.js';

export const _evalToJson = left => right => src => {
  try {
    return (right(eval(src)));
  } catch (error) {
    return (left(error.toString() + error.stack));
  }
}

// Same as _evalToJson, but given a different type on the PS side.
export const _evalToForeign = left => right => src => {
  try {
    return (right(eval(src)));
  } catch (error) {
    return (left(error.toString()));
  }
}

export const _applyClosure = left => right => f => args => {
  try {
    return (right(f(...args)));
  } catch (error) {
    return (left(error.toString()));
  }
}

export const _toString = js_value => {
  return JSON.stringify(js_value);
}

// Lispy

const evalMidnight = a => {
  return MidnightJS.evalJsonToJson(a).value0;
}

// Conditional

const ifMidnight = (predicate, consequent, alternative) => {
  return (predicate === "t") ? consequent : alternative;
}

// Pair

const car = xs => {
  return xs[0];
}

const cdr = xs => {
  return xs.slice(1);
}

const cons = (x, xs) => {
  return [x, ...xs];
}

const isPair = xs => {
  return boolToSymbol(Array.isArray(xs) && xs.length > 0);
}

const isListEmpty = xs => {
  return boolToSymbol(Array.isArray(xs) && xs.length === 0);
}

// Symbol

const isSymbol = a => {
  return boolToSymbol(typeof(a) === 'string');
}

const isSymbolEq = (a, b) => {
  return boolToSymbol(a === b);
}

const codepointsToSymbol = xs => {
  return String.fromCodePoint(...xs);
}

// Integer

const isInt = a => {
  return boolToSymbol(typeof(a) === 'number');
}

const add = (x, y) => {
  return x + y;
}

const subtract = (x, y) => {
  return x - y;
}

const multiply = (x, y) => {
  return x * y;
}

const divide = (x, y) => {
  return Math.floor(x / y);
}

const modulo = (x, y) => {
    return x % y;
  }

const lessThan = (x, y) => {
  return boolToSymbol(x < y);
}

const equal = (x, y) => {
  return boolToSymbol(x === y);
}

const greaterThan = (x, y) => {
  return boolToSymbol(x > y);
}

// Debug

const crash_now_midnight = a => {
  throw new Error(a);
}

const traceMidnightHelper = a => {
  console.log(a);
  return a;
}

const tracebenchMidnightHelper = lazyA => {
  // TODO: test
  console.time("bench");
  const a = lazyA();
  console.timeEnd("bench");
  return a;
}

// Helpers

const boolToSymbol = bool => {
  return bool ? "t" : "f";
}