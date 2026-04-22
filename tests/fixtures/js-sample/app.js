// Sample JavaScript file for anvil-js locator tests.
// Covers CommonJS-looking import-style via `require`, ES module
// imports, async functions, classes + methods, arrow function
// assigned to a const, and a nested inner function.

import { readFile } from "node:fs/promises";
import * as path from "node:path";
import express from "express";
import "./side-effects";

export const MAX_ROWS = 1000;

export function plainFunc(a, b) {
  return a + b;
}

export async function fetchValue(url) {
  const buf = await readFile(url);
  return buf.toString();
}

export const arrowHelper = (x) => x * 2;

const privateArrow = async (n) => n + 1;

export class ReportWriter {
  constructor(filename) {
    this.filename = filename;
  }

  write(rows) {
    rows.forEach((r) => console.log(r));
  }

  static defaultName() {
    return "report.xlsx";
  }

  async writeAsync(rows) {
    this.write(rows);
  }
}

export default class Config {
  constructor(name, limit = 100) {
    this.name = name;
    this.limit = limit;
  }

  describe() {
    return `${this.name}: ${this.limit}`;
  }
}

export function outer() {
  function inner(a) {
    return a + 1;
  }
  return inner;
}

export { plainFunc as renamedPlain };
