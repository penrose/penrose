import "global-jsdom/register";

import { PenroseError, compile, optimize, toSVG } from "@penrose/core";
import { config } from "dotenv";
import * as fs from "fs";
import {
  LLMPrompt,
  LLMPromptCollection,
  LLMResult,
  generateSubstanceLLM,
  llmPrompts,
} from "../gpt.js";

config({ path: ".env.development.local" });

type Metadata = {
  date: Date;
  prompt: LLMPrompt;
  output: string;
  inferenceTime: number;
  APIError?: string;
  penroseError?: PenroseError;
};

const formatDate = (date: Date): string => {
  // Get the individual date components
  const year = date.getFullYear(); // 4-digit year
  const month = String(date.getMonth() + 1).padStart(2, "0"); // Month (0-11), adding 1 to get 1-12 range
  const day = String(date.getDate()).padStart(2, "0"); // Day of the month (1-31)
  const hour = String(date.getHours()).padStart(2, "0"); // Hour (0-23)
  const minute = String(date.getMinutes()).padStart(2, "0"); // Minute (0-59)
  const second = String(date.getSeconds()).padStart(2, "0"); // Second (0-59)

  // Combine the components into the desired format
  const formattedDate = `${year}${month}${day}${hour}${minute}${second}`;

  // Output the formatted date
  return formattedDate;
};

const writeToJSON = (
  metadata: Metadata,
  enclosingFolder: string,
  date: Date,
) => {
  fs.writeFileSync(
    `${process.env.LLM_OUTPUT_PATH}/json/${enclosingFolder}/${formatDate(
      date,
    )}.json`,
    JSON.stringify(metadata),
  );
};

const handlePenroseErr = (
  err: PenroseError,
  result: LLMResult,
  enclosingFolder: string,
) => {
  if (result.tag === "Ok") {
    const date = new Date();
    writeToJSON(
      {
        date,
        prompt: result.prompt,
        output: result.substance,
        inferenceTime: result.inferenceTime,
        penroseError: err,
      },
      enclosingFolder,
      date,
    );
  }
};

const testPrompts = async (prompts: LLMPromptCollection) => {
  const dateString = formatDate(new Date());
  fs.mkdirSync(`${process.env.LLM_OUTPUT_PATH}/json/${dateString}`, {
    recursive: true,
  });
  fs.mkdirSync(`${process.env.LLM_OUTPUT_PATH}/svg/${dateString}`, {
    recursive: true,
  });
  for (const key in prompts) {
    //if (!/^geometry_[0]_1_1/.test(key)) {
    if (!key.endsWith("_1_1")) {
      continue;
    }
    const result = await generateSubstanceLLM({
      prompt: prompts[key],
      openaiApiKey: process.env.OPENAI_API_KEY!,
    });

    if (result.tag === "Ok") {
      const compilerOutput = await compile({
        substance: result.substance,
        style: result.prompt.style,
        domain: result.prompt.domain,
        variation: "something",
      });
      if (compilerOutput.isErr()) {
        const err = compilerOutput.error;
        handlePenroseErr(err, result, dateString);
        continue;
      }
      let optimizedState;
      const optimizedOutput = optimize(compilerOutput.value);
      if (optimizedOutput.isOk()) {
        optimizedState = optimizedOutput.value;
      } else {
        handlePenroseErr(optimizedOutput.error, result, dateString);
        continue;
      }

      const canvas = (
        await toSVG(optimizedState, async () => undefined, "roger")
      ).outerHTML;

      const date = new Date();
      const svgOutputPath = `${
        process.env.LLM_OUTPUT_PATH
      }/svg/${dateString}/${formatDate(date)}.svg`;
      fs.writeFileSync(svgOutputPath, canvas);

      const metadata: Metadata = {
        date,
        prompt: result.prompt,
        output: result.substance,
        inferenceTime: result.inferenceTime,
      };
      writeToJSON(metadata, dateString, date);
    } else {
      const date = new Date();
      const metadata: Metadata = {
        date,
        prompt: result.prompt,
        output: "",
        inferenceTime: result.inferenceTime,
        APIError: result.APIError,
      };
      writeToJSON(metadata, dateString, date);
    }
  }
  return dateString;
};

const analyzeJSONs = (enclosingFolder: string) => {
  const jsons = fs.readdirSync(
    `${process.env.LLM_OUTPUT_PATH}/json/${enclosingFolder}`,
  );
  const metadata: Metadata[] = [];
  for (const json of jsons) {
    const data = fs.readFileSync(
      `${process.env.LLM_OUTPUT_PATH}/json/${enclosingFolder}/${json}`,
    );
    metadata.push(JSON.parse(data.toString()));
  }
  const errors = metadata.filter((m) => m.penroseError);
  const yesBNF = errors.filter((m) => !m.prompt.bnf);
  const noBNF = errors.filter((m) => m.prompt.bnf);
  const successes = metadata.filter((m) => !m.penroseError && !m.APIError);
  const APIErrors = metadata.filter((m) => m.APIError);
  console.log("API Errors", APIErrors.length);
  console.log("Successes", successes.length);
  console.log("Errors", errors.length);
  console.log("Errors: Domain", noBNF.length);
  console.log("Errors: BNF", yesBNF.length);
};

// Run tests and analyze results
const which = await testPrompts(llmPrompts);
analyzeJSONs(which);
