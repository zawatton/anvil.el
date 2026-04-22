// Sample TypeScript file for anvil-ts locator tests.
//
// Mirrors the py-sample shape: imports, exports, classes with
// methods (including async), decorated declarations via
// experimentalDecorators, interfaces, type aliases, top-level
// functions (both `function` and arrow), a nested inner function,
// and a module-level constant.

import { readFile } from "node:fs/promises";
import * as path from "node:path";
import type { Buffer } from "node:buffer";
import express, { Request, Response } from "express";
import "./side-effects";

export const MAX_ROWS = 1000;

export interface ReportInput {
  rows: number;
  filename: string;
}

export type ReportMode = "append" | "overwrite";

type Callback = (err: Error | null, value: number) => void;

interface Internal {
  ready: boolean;
}

export function plainFunc(a: number, b: number): number {
  return a + b;
}

export async function fetchValue(url: string): Promise<string> {
  const buf = await readFile(url);
  return buf.toString();
}

export const arrowHelper = (x: number): number => x * 2;

const privateArrow = async (n: number) => n + 1;

export class ReportWriter {
  readonly filename: string;

  constructor(filename: string) {
    this.filename = filename;
  }

  write(rows: number[]): void {
    rows.forEach((r) => console.log(r));
  }

  static defaultName(): string {
    return "report.xlsx";
  }

  async writeAsync(rows: number[]): Promise<void> {
    this.write(rows);
  }
}

export default class Config {
  constructor(public name: string, public limit: number = 100) {}

  describe(): string {
    return `${this.name}: ${this.limit}`;
  }
}

export function outer(): (a: number) => number {
  function inner(a: number): number {
    return a + 1;
  }
  return inner;
}

export { plainFunc as renamedPlain };
