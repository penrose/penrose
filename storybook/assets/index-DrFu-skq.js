function v(u){for(var i=[],c=1;c<arguments.length;c++)i[c-1]=arguments[c];var n=Array.from(typeof u=="string"?[u]:u);n[n.length-1]=n[n.length-1].replace(/\r?\n([\t ]*)$/,"");var p=n.reduce(function(r,o){var e=o.match(/\n([\t ]+|(?!\s).)/g);return e?r.concat(e.map(function(g){var t,a;return(a=(t=g.match(/[\t ]/g))===null||t===void 0?void 0:t.length)!==null&&a!==void 0?a:0})):r},[]);if(p.length){var f=new RegExp(`
[	 ]{`+Math.min.apply(Math,p)+"}","g");n=n.map(function(r){return r.replace(f,`
`)})}n[0]=n[0].replace(/^\r?\n/,"");var l=n[0];return i.forEach(function(r,o){var e=l.match(/(?:^|\n)( *)$/),g=e?e[1]:"",t=r;typeof r=="string"&&r.includes(`
`)&&(t=String(r).split(`
`).map(function(a,h){return h===0?a:""+g+a}).join(`
`)),l+=t+n[o+1]}),l}export{v as d};
