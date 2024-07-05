export const LLMMutatorPrompt = (
  domain: string,
  prompt: string,
  substance: string,
  numProgs: number,
): string => {
  return `I have a practice problem: ${prompt}. A correct answer is written in a domain-specific language called Substance. Here is the correct answer:  

		${substance}

		The syntax and constructs of this domain-specific language is parametrized by the following specification:

		${domain}

		Now, come up with ${numProgs} incorrect answers that will help the student understand the concepts related to this problem, in the format of a list of Substance programs. To write comments, begin with \`--\`. Return only the Substance program; explain your reasoning in Substance comments only. Respond with the raw JSON list without markdown formatting.
		`;
};
