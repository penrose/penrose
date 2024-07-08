<script setup>
import { VPTeamMembers } from "vitepress/theme";
import Contributors from "../src/components/Contributors.vue"

// https://commons.wikimedia.org/wiki/File:Globe_icon_2.svg
// https://creativecommons.org/licenses/by-sa/3.0/deed.en
// changes:
// - replaced width and height with viewbox
// - changed stroke from #000000 to currentColor
const website = `
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg
   xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:cc="http://creativecommons.org/ns#"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   viewbox="0 0 420 420"
   id="svg2"
   version="1.1"
   inkscape:version="0.48.4 r9939"
   sodipodi:docname="Globe_icon.svg">
  <metadata
     id="metadata10">
    <rdf:RDF>
      <cc:Work
         rdf:about="">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource="http://purl.org/dc/dcmitype/StillImage" />
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <defs
     id="defs8" />
  <sodipodi:namedview
     pagecolor="#ffffff"
     bordercolor="#666666"
     borderopacity="1"
     objecttolerance="10"
     gridtolerance="10"
     guidetolerance="10"
     inkscape:pageopacity="0"
     inkscape:pageshadow="2"
     inkscape:window-width="1366"
     inkscape:window-height="705"
     id="namedview6"
     showgrid="false"
     inkscape:zoom="0.79465332"
     inkscape:cx="264.12085"
     inkscape:cy="85.913893"
     inkscape:window-x="-8"
     inkscape:window-y="-8"
     inkscape:window-maximized="1"
     inkscape:current-layer="svg2" />
  <path
     id="path3822"
     style="fill:none;stroke:currentColor;stroke-width:20;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none"
     d="m 226.19946,16.656571 a 473.96004,333.37897 0 0 1 0,387.232509 M 59,333.21514 a 260,260 0 0 1 302,0 M 197.17143,14.79354 a 477.24462,335.68933 0 0 0 0,389.91607 M 209,15 a 195,195 0 1 0 2,0 z m 1,0 V 405 M 405,210 H 15 M 59,92.669492 a 260,260 0 0 0 302,0 M 361,330" />
</svg>
`;

const members = [
  {
    name: 'Wode "Nimo" Ni',
    title: "Ph.D. student @ CMU",
    avatar: "https://www.github.com/wodeni.png",
    links: [
      { icon: { svg: website }, link: "https://www.cs.cmu.edu/~woden/" },
      { icon: "github", link: "https://github.com/wodeni" },
    ],
  },
  {
    name: "Sam Estep",
    title: "Ph.D. student @ CMU",
    avatar: "https://www.github.com/samestep.png",
    links: [
      { icon: { svg: website }, link: "https://samestep.com/" },
      { icon: "github", link: "https://github.com/samestep" },
    ],
  },
  {
    name: "Yiliang (Leo) Liang",
    title: "Ph.D. student @ CMU",
    avatar: "https://www.github.com/liangyiliang.png",
    links: [
      { icon: "github", link: "https://github.com/liangyiliang" },
    ],
  },
  {
    name: "Jiri Minarcik",
    title: "Ph.D. student @ CTU",
    avatar: "https://www.github.com/jiriminarcik.png",
    links: [
      { icon: { svg: website }, link: "https://github.com/jiriminarcik" },
      { icon: "github", link: "https://github.com/jiriminarcik" },
    ],
  },
  {
    name: "Hwei-Shin Harriman",
    title: "Ph.D. student @ CMU",
    avatar: "https://www.github.com/hsharriman.png",
    links: [
      { icon: { svg: website }, link: "https://hsharriman.github.io/" },
      { icon: "github", link: "https://github.com/hsharriman" },
    ],
  },
	{
		name: "Kyle Lee",
		title: "Undergraduate student @ CMU",
		avatar: "https://github.com/KyleleeSea.png",
		links:[
      { icon: "github", link: "https://github.com/KyleleeSea" },
		]
	},
	{
		name: "Griffin Teller",
		title: "Undergraduate student @ CMU",
		avatar: "https://github.com/griffinteller.png",
    links: [
      { icon: { svg: website }, link: "https://griffinteller.com/" },
      { icon: "github", link: "https://github.com/griffinteller" },
    ],
	},
  {
    name: "Rijul Jain",
    title: "REUSE student",
    avatar: "https://www.github.com/rjainrjain.png",
    links: [
      { icon: { svg: website }, link: "https://rijuljain.com" },
      { icon: "github", link: "https://github.com/rjainrjain" },
    ],
  },
  {
    name: "Keenan Crane",
    title: "Professor @ CMU",
    avatar: "https://www.github.com/keenancrane.png",
    links: [
      { icon: { svg: website }, link: "https://www.cs.cmu.edu/~kmcrane/" },
      { icon: "github", link: "https://github.com/keenancrane" },
    ],
  },
  {
    name: "Josh Sunshine",
    title: "Professor @ CMU",
    avatar: "https://www.github.com/joshsunshine.png",
    links: [
      { icon: { svg: website }, link: "https://www.cs.cmu.edu/~jssunshi/" },
      { icon: "github", link: "https://github.com/joshsunshine" },
    ],
  },
];
</script>

# The Penrose Team

The Penrose Team is based at [Carnegie Mellon University](https://cmu.edu])'s [School of Computer Science](https://cs.cmu.edu)
in Pittsburgh, PA USA—and other locations around the world.

## Current Members

<VPTeamMembers size="small" :members="members" />

## Past Members

- [Raven Rothkopf](https://ravenrothkopf.github.io/)
- [Jonathan Aldrich](https://www.cs.cmu.edu/~aldrich/)
- [Matt Davis](https://cmumatt.github.io/)
- [Rain Du](https://miyehn.me/)
- [Max Krieger](https://a9.io/)
- [Josh Pollock](https://joshmpollock.com/)
- [Dor Ma'ayan](http://www.cs.technion.ac.il/people/dorma10/)
- [Will Rinkoff](https://dsm0.github.io/)
- [Lily Shellhammer](https://www.linkedin.com/in/lily-shellhammer-899b43105)
- [Mia Tang](https://mia-tang.com/#/)
- [Stella Trout](https://github.com/strout18)
- [Jenna Wise](https://www.cs.cmu.edu/~jlwise/)
- [Helena Yang](https://heleaf.me/)
- [Kai Ye](https://www.linkedin.com/in/kai-ye-83946725a/)

## Contributors

<Contributors />
