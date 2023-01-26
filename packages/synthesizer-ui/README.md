# Synthesizer UI Package

The general purpose of this web-app is to provide a user-interface which problem authors or Penrose users can interact with to generate multiple diagrams, view mutated programs, and export diagrams that they like. This is a first pass, so it covers some core functionalities with opportunities to extend as necessary. The core things that it does:

- Runs the synthesizer to generate multiple mutated programs based on a file triple
- Provides a default substance, domain, and style program but they can be altered if the author chooses
- Provides settings that can be adjusted to configure how many programs, mutations/program, and types of statements can be mutated on
- Each mutated program displays its CIEE as well as the operations and mutated substance program
- Clicking on a checkbox stages it for exporting, and clicking on the EXPORT button downloads all staged diagrams as SVGs

## Available Scripts

In the project directory, you can run:

### `yarn start`

Runs the app in the development mode.\
Open [http://localhost:4000](http://localhost:4000) to view it in the browser.

You will see any lint errors in the console.

### `yarn build`

Builds the app for production to the `dist` folder.\
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.\
Your app is ready to be deployed!

See the section about [deployment](https://facebook.github.io/create-react-app/docs/deployment) for more information.
