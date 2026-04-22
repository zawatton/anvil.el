// Sample TSX file for anvil-ts locator tests.
//
// Exercises JSX syntax alongside TypeScript features so the tsx
// grammar dispatch is actually taken.

import React, { useEffect, useState } from "react";
import type { ReactNode } from "react";

export interface ButtonProps {
  label: string;
  onClick?: () => void;
}

export type Theme = "light" | "dark";

export function Button({ label, onClick }: ButtonProps): JSX.Element {
  return <button onClick={onClick}>{label}</button>;
}

export const Greeting: React.FC<{ name: string }> = ({ name }) => {
  const [count, setCount] = useState<number>(0);
  useEffect(() => {
    setCount((c) => c + 1);
  }, []);
  return <div>Hello {name}, count={count}</div>;
};

export class ThemedPanel extends React.Component<{ theme: Theme }> {
  render(): ReactNode {
    return <section className={this.props.theme}>panel</section>;
  }
}
