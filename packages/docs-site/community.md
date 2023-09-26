---
layout: docs
---

<div class="action-grid">

<div class="action-group diagrams">

<h1 class="action-header">🌹 Make Diagrams</h1>

We are always excited to see diagrams made with Penrose! Share them in [#gallery][] on [Discord][] and we are happy to include them in our [example gallery][].

- **If you want to make diagrams using in existing domains**, check out our [tutorial][] and [gallery][example gallery] to learn how to use them!
- **If you are making many diagrams in a new domain**, refer to our [docs][using penrose] to learn our toolings and [open a PR][contributing PR] in our repo so others can use your domain too!

</div>

<div class="action-group research">

<h1 class="action-header">🔭 Do Research</h1>

Our team consist of interdisciplinary researchers in programming languages, computer graphics, human-computer interaction, software engineering, and more!

- If you are interested in **collaborating with us on research**, reach out to us on [Discord][] and/or [email][] us with a brief description of your background and interests in Penrose!
- If you are **an undergrad looking for summer opportunities**, consider applying for the [REUSE][] program.

</div>

<div class="action-group code-docs">

<h1 class="action-header">💻 Contribute Code and Docs</h1>

We welcome contributions to our [repository][]. Take a look at the [contributor guide](https://github.com/penrose/penrose/blob/main/CONTRIBUTING.md) to get started!

</div>

<div class="action-group integration">

<h1 class="action-header">💞 Integrate with Tools</h1>

We'd love to hear how you'd like to integrate Penrose with your favorite tools! Here are some ongoing tool integration projects:

- [Lean](https://leanprover.github.io/): [ProofWidgets](https://github.com/EdAyers/ProofWidgets4) based on Penrose.
- [Obsidian](https://obsidian.md/): an experimental [`obsidian-penrose-plugin`](https://github.com/wodeni/obsidian-penrose-plugin).
- [Alloy](http://alloytools.org/) visualizer using Penrose.

Join [our Discord server] and chat with us on [#integration](https://discord.com/channels/1115349463603617954/1130497270664679444) about integrating with external tools!

</div>

<div class="action-group posts">

<h1 class="action-header">💬 Share Knowledge</h1>

We maintain a [blog][] and a [mailing list][] on Penrose and diagramming in general. We are happy to host your posts!

</div>
</div>

## Technical Roadmap

### 🚀 Active

- Layout optimization on WebWorkers
- Automatic differentiation based on [Rose][rose] 🌹

### 🎯 Planned

- [SolidJS API for Penrose](/docs/ref/solid.md)
-

<style>
  .action-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(320px, 1fr)); 
    grid-gap: 20px;
    /* grid-auto-row: min-content; */
    /* display: flex;
    width: 100%;
    flex-wrap: wrap; */
  }

  .action-group {
    /* max-width: 50%; */
    border-radius: 10px;
    background-color: #f5f5f5;
    padding: 20px;
  }
  .dark .action-group {
    background-color: var(--vp-c-bg-soft);
  }
  .action-header {
    font-size: 28px !important;
  }
  /* .research {
    grid-row: span 1
  }
  .code-and-docs {
    grid-row: span 1
  } */
</style>

[discord]: https://discord.gg/a7VXJU4dfR
[email]: mailto:team@penrose.ink
[#gallery]: https://discord.com/channels/1115349463603617954/1115717389787611155
[blog]: /blog
[REUSE]: https://www.cmu.edu/scs/s3d/reuse/
[repository]: https://github.com/penrose/penrose
[example gallery]: /examples
[mailing list]: http://eepurl.com/cIapnn
[tutorial]: /docs/tutorial/welcome
[using penrose]: /docs/ref/using
[contributing PR]: https://github.com/penrose/penrose/blob/main/CONTRIBUTING.md#contributing
[rose]: https://rosejs.dev/
