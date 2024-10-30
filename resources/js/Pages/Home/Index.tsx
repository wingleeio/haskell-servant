import React, { useState } from "react";

import { Head } from "@inertiajs/react";
import { usePage } from "@inertiajs/react";

export default function Home() {
  const props: any = usePage().props;
  const [count, setCount] = useState(0);

  return (
    <div className="p-4">
      <Head title="What is this!!!" />
      <pre className="p-2">{JSON.stringify(props, null, 2)}</pre>
      <div>{count}</div>
      <button onClick={() => setCount((count) => count + 1)}>+</button>
    </div>
  );
}
