import { Head } from "@inertiajs/react";
import React from "react";
import { usePage } from "@inertiajs/react";

export default function Home() {
  const props: any = usePage().props;

  return (
    <div>
      <Head title="What is this!!!" />
      {JSON.stringify(props)}!{props.test}
      {props.test2}
      test
    </div>
  );
}
