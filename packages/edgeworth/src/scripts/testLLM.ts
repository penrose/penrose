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

config({ path: "../.env.development.local" });

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

const handlePenroseErr = (err: PenroseError, result: LLMResult) => {
  const date = new Date();
  if (result.tag === "Ok") {
    fs.writeFileSync(
      `/Users/rijuljain/Library/CloudStorage/Box-Box/llm-output/json/${formatDate(
        date
      )}.json`,
      JSON.stringify({
        date,
        prompt: result.prompt,
        output: result.substance,
        inferenceTime: result.inferenceTime,
        penroseError: err,
      })
    );
  }
};

const testPrompts = async (prompts: LLMPromptCollection) => {
  const dateString = formatDate(new Date());
  for (const key in prompts) {
    if (!key.startsWith("geometry")) {
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
        handlePenroseErr(err, result);
        continue;
      }
      /*let initialState;
      try {
        initialState = await prepareState(compilerOutput.value);
      } catch (e) {
        handlePenroseErr(
          {
            tag: "RuntimeError",
            message: "prepareState failed",
            errorType: "RuntimeError",
          },
          result
        );
        continue;
      }*/
      let optimizedState;
      const optimizedOutput = optimize(compilerOutput.value);
      if (optimizedOutput.isOk()) {
        optimizedState = optimizedOutput.value;
      } else {
        handlePenroseErr(optimizedOutput.error, result);
        continue;
      }

      const canvas = (
        await toSVG(optimizedState, async () => undefined, "roger")
      ).outerHTML;

      const date = new Date();
      const svgOutputPath = `/Users/rijuljain/Library/CloudStorage/Box-Box/llm-output/svg/${formatDate(
        date
      )}.svg`;
      const jsonOutputPath = `/Users/rijuljain/Library/CloudStorage/Box-Box/llm-output/json/${formatDate(
        date
      )}.json`;
      fs.writeFileSync(svgOutputPath, canvas);

      const metadata: Metadata = {
        date,
        prompt: result.prompt,
        output: result.substance,
        inferenceTime: result.inferenceTime,
      };
      fs.writeFileSync(jsonOutputPath, JSON.stringify(metadata));
    } else {
      const date = new Date();
      const metadata: Metadata = {
        date,
        prompt: result.prompt,
        output: "",
        inferenceTime: result.inferenceTime,
        APIError: result.APIError,
      };
      fs.writeFileSync(
        `/Users/rijuljain/Library/CloudStorage/Box-Box/llm-output/json/${formatDate(
          date
        )}.json`,
        JSON.stringify(metadata)
      );
    }
  }
  return dateString;
};

const analyzeJSONs = (date: string) => {
  const jsons = fs.readdirSync(
    "/Users/rijuljain/Library/CloudStorage/Box-Box/llm-output/json"
  );
  const metadata: Metadata[] = [];
  for (const json of jsons) {
    if (json.replace(".json", "") < date) {
      continue;
    }
    const data = fs.readFileSync(
      `/Users/rijuljain/Library/CloudStorage/Box-Box/llm-output/json/${json}`
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
  console.log("Errors: Domain without BNF", noBNF.length);
  console.log("Errors: Domain with BNF", yesBNF.length);
};

// Run tests and analyze results
const which = await testPrompts(llmPrompts);
analyzeJSONs(which);
