import animalNameList from "animals";
import colorNameList from "color-name-list/dist/colornames.json";

// all one-word colors
const colors: string[] = colorNameList
  .map(({ name }) => name)
  .filter((color) => /^[A-Z][a-z]+$/.test(color));

// all one-word animals, with first letter capitalized
const animals: string[] = animalNameList.words
  .filter((animal: string) => /^[a-z]+$/.test(animal))
  .map((animal: string) => animal.charAt(0).toUpperCase() + animal.slice(1));

// min and max are both inclusive
const randInt = (min: number, max: number) =>
  Math.floor(Math.random() * (max + 1 - min)) + min;

const choose = (list: string[]) =>
  list[Math.floor(Math.random() * list.length)];

export const generateVariation = () => {
  const numDigits = randInt(3, 5);
  const digits: number[] = [];
  for (let i = 0; i < numDigits; i++) {
    digits.push(randInt(0, 9));
  }
  return `${choose(colors)}${choose(animals)}${digits.join("")}`;
};
