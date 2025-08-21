// @ts-check

import eslint from '@eslint/js';
import tseslint from 'typescript-eslint';

export default tseslint.config(
  eslint.configs.recommended, // Core ESLint recommended rules
  ...tseslint.configs.recommendedTypeChecked, // TypeScript-specific recommended rules with type-checking
  {
    languageOptions: {
      parserOptions: {
        project: ['./tsconfig.json'], // Specify your tsconfig.json file
        tsconfigRootDir: import.meta.dirname, // Set the root directory for tsconfig.json resolution
      },
    },
    rules: {
      // Add or override specific rules here
      // Example: 'no-unused-vars': 'warn',
    },
  },
  {
    ignores: ['dist/', 'node_modules/'], // Files/directories to ignore
  }
);