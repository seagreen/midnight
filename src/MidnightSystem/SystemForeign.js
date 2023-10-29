"use strict";

export const _editorStringToInput = left => right => jsStr => {
  try {
    return right(arrayToLinkedList([
      // NOTE(QUOTED_SYMBOLS_NOT_SANITIZED)
      "system-input-start-with-editor-contents",
      arrayToLinkedList ([
        "string-tag",
        stringToLinkedListOfCodepoints(jsStr)
      ])
    ]));
  } catch (error) {
    return (left(error.toString()));
  }
}

const stringToLinkedListOfCodepoints = str => {
  return arrayToLinkedList(Array.from(str).map(char => char.codePointAt(0)));
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