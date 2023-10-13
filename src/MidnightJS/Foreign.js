"use strict";

import * as MidnightJS from '../MidnightJS/index.js';

export const _evalToJson = left => right => src => {
  try {
    return right(evalWithBuiltinSnippet(src));
  } catch (error) {
    return left(error.toString());
  }
}

export const _evalToForeignNoCatch = src => {
  return evalWithBuiltinSnippet(src);
}

// Same as _evalToJson, but given a different type on the PS side.
export const _evalToForeign = left => right => src => {
  try {
    return right(evalWithBuiltinSnippet(src));
  } catch (error) {
    return left(error.toString());
  }
}

export const _applyClosure = left => right => f => args => {
  try {
    return right(f(...args));
  } catch (error) {
    return left(error.toString());
  }
}

export const _toString = js_value => {
  return JSON.stringify(js_value);
}

// Eval

const globalObject = typeof window !== 'undefined' ? window : global;

/*
globalObject.evalJsonToJsonAttachedToGlobalObject = (a) => {
  // TODO: is the value0 for the `Right` constructor?
  return MidnightJS.evalJsonToJson(a).value0;
}
*/

globalObject.evalJsonToForeignNoCatchAttachedToGlobalObject = (a) => {
  return MidnightJS.evalJsonToForeignNoCatch(a);
}

const evalWithBuiltinSnippet = jsExpr => {
  // https://esbuild.github.io/content-types/#direct-eval
  return (0, eval)(addBuiltinSnippet(jsExpr));
}

const addBuiltinSnippet = jsExpr => {
  return builtinSnippet + "\n\n" + jsExpr;
}

const builtinSnippet = `
// Lispy
const globalObject = typeof window !== 'undefined' ? window : global;

const evalMidnight = midnightAsJson => {
  const getRandomDigits = max => (Math.floor(Math.random() * max)).toString().padStart(4, '0');
  const label = 'eval_internal_' + getRandomDigits(9999)
  console.time(label);
  const a = globalObject.evalJsonToForeignNoCatchAttachedToGlobalObject(midnightAsJson);
  console.timeEnd(label);
  return a;
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
  return xs[1];
}

const cons = (x, xs) => {
  return [x, xs];
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
  return String.fromCodePoint(...(linkedListToArray(xs)));
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
  return (x / y) | 0;
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

const traceMidnightHelper = (label, a) => {
  console.log(label + " " + a);
  return a;
}

const tracebenchMidnightHelper = (label, lazyA) => {
  console.time(label);
  const a = lazyA();
  console.timeEnd(label);
  return a;
}

// Helpers

const boolToSymbol = bool => {
  return bool ? "t" : "f";
}

const linkedListToArray = xs => {
  let ys = [];
  let current = xs;
  while (current.length > 0) {
    ys.push(current[0]);
    current = current[1];
  }
  return ys;
}

// TODO: questionable and duplicate
const arrayToLinkedList = arr => {
  if (arr.length === 0) {
    return [];
  }

  let list = [arr[0], []];
  let current = list;

  for (let i = 1; i < arr.length; i++) {
    current[1] = [arr[i], []];
    current = current[1];
  }

  return list;
}
`;
