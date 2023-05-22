import { useState } from "react";
import { range, drop, take } from "lodash";
import { dateColumn } from "react-datasheet-grid";
import { DataSheetGrid, textColumn, keyColumn } from "react-datasheet-grid";
import timelineStyle from "@penrose/examples/dist/timeline/timeline.style";
import timelineDomain from "@penrose/examples/dist/timeline/timeline.domain";

// Import the style only once in your app!
import "react-datasheet-grid/dist/style.css";
import { Simple } from "../Simple";
import { zip2 } from "@penrose/core";

interface Data {
  start: Date;
  end: Date;
  categories: string[];
  events: Event[];
}

interface Event {
  start: Date;
  end: Date;
  category: string;
  task: string;
}

// https://stackoverflow.com/questions/11981453/get-current-quarter-in-year-with-javascript
function getQuarter(d: Date) {
  const q = [1, 2, 3, 4];
  return q[Math.floor(d.getUTCMonth() / 3)];
}

const substance = (
  start: Date,
  end: Date,
  categories: string[],
  events: Event[]
) => {
  const startYear = start.getUTCFullYear();
  const startQuarter = getQuarter(start);
  const endYear = end.getUTCFullYear();
  const endQuarter = getQuarter(end);
  const yrs = Array.from({ length: endYear - startYear + 1 }).map((_, i) => ({
    value: startYear + i,
    label: `y${startYear + i}`,
    startQuarter: i == 0 ? startQuarter : 1,
    endQuarter: i == endYear - startYear ? endQuarter : 4,
  }));
  const qts = yrs.flatMap((y, i) =>
    range(y.startQuarter, y.endQuarter + 1).map((i) => ({
      label: `q${i}_${y.value}`,
      value: i,
      year: y.label,
    }))
  );

  const preamble = `
Year ${yrs.map((y) => y.label).join(", ")}
${yrs.map((y) => `Label ${y.label} "${y.value}"`).join("\n")}
${qts.map((q) => `Quarter ${q.label} := MkQuarter(${q.year})`).join("\n")}
${qts.map((q) => `Label ${q.label} "Q${q.value}"`).join("\n")}
${yrs.map((y) => `First(q${y.startQuarter}_${y.value}, ${y.label})`).join("\n")}
${yrs.map((y) => `Last(q${y.endQuarter}_${y.value}, ${y.label})`).join("\n")}
${zip2(take(qts, qts.length - 1), drop(qts))
  .map(([before, after]) => `Before(${before.label}, ${after.label})`)
  .join("\n")}

Category ${categories.join(", ")}
  `;

  console.log(preamble);
  let prog = preamble;
  for (let i = 0; i < events.length; i++) {
    const e = events[i];
    prog += `
  Task e${i} := MkTask(${toYearQuarter(e.start)}, ${toYearQuarter(e.end)})
  In(e${i}, ${e.category})
  Label e${i} "${e.task}"
    `;
  }
  return prog;
};

const toYearQuarter = (d: Date): string => {
  const year = d.getUTCFullYear();
  const quarter = Math.floor((d.getUTCMonth() + 3) / 3);
  return `q${quarter}_${year}`;
};

export default function (props: Data): React.ReactElement {
  const [events, setEvents] = useState<Event[]>(props.events);
  const [bounds, setBounds] = useState({
    start: props.start,
    end: props.end,
  });

  const validate = (
    d: any,
    start: Date,
    end: Date,
    categories: string[]
  ): Event => {
    const validDate = (date: Date | undefined): Date | undefined => {
      if (date && !isNaN(date.getTime())) {
        return date;
      } else return undefined;
    };
    return {
      start: validDate(d.start) ?? start,
      end: validDate(d.end) ?? end,
      category: categories.includes(d.category) ? d.category : categories[0],
      task: d.task ?? "New Task",
    };
  };

  const columns = [
    { ...keyColumn("task", textColumn), title: "Task" },
    { ...keyColumn("category", textColumn), title: "Category" },
    { ...keyColumn("start", dateColumn), title: "Start date" },
    { ...keyColumn("end", dateColumn), title: "End date" },
  ];

  return (
    <div style={{ display: "flex" }}>
      <div style={{ width: "50%" }}>
        <DataSheetGrid
          value={[bounds]}
          onChange={(d) => {
            setBounds({
              start: d[0].start,
              end: d[0].end,
            });
          }}
          columns={[
            {
              ...keyColumn("start", dateColumn),
              title: "Timeline Start Date",
            },
            {
              ...keyColumn("end", dateColumn),
              title: "Timeline End Date",
            },
          ]}
          lockRows
        />
        <DataSheetGrid
          value={events}
          onChange={(d) =>
            setEvents(
              d.map((e) =>
                validate(e, bounds.start, bounds.end, props.categories)
              ) as Event[]
            )
          }
          columns={columns}
        />
      </div>
      <div style={{ width: "50%" }}>
        <Simple
          substance={substance(
            bounds.start,
            bounds.end,
            props.categories,
            events.filter((e) => e.start >= bounds.start && e.end <= bounds.end)
          )}
          style={timelineStyle}
          domain={timelineDomain}
          variation=""
        ></Simple>
      </div>
    </div>
  );
}
