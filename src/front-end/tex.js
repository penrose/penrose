function tex2svg(texstring, callback) {
    var input = texstring;
    var wrapper = document.createElement("div");
    wrapper.innerHTML = input;
    var svg = ""
    MathJax.Hub.Queue(["Typeset", MathJax.Hub, wrapper]);
    MathJax.Hub.Queue(function() {
        var mjOut = wrapper.getElementsByTagName("svg")[0];
        mjOut.setAttribute("xmlns", "http://www.w3.org/2000/svg");
        svg = mjOut.outerHTML;
        callback(svg);
    });
}

var s = tex2svg("\\[f: X \\to Y\\]", function(svg){
    var parser = new DOMParser();
    var doc = parser.parseFromString(svg, "image/svg+xml");
    document.getElementsByTagName('body')[0].appendChild(doc.documentElement);
    console.log(svg)
});
