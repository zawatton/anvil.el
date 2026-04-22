// Sample JSX file for anvil-js locator tests.
// Exercises React JSX syntax under the javascript grammar.

import React, { useState } from "react";

export function Button({ label, onClick }) {
  return <button onClick={onClick}>{label}</button>;
}

export const Greeting = ({ name }) => {
  const [count, setCount] = useState(0);
  return <div>Hello {name}, count={count}</div>;
};

export class ThemedPanel extends React.Component {
  render() {
    return <section className={this.props.theme}>panel</section>;
  }
}
