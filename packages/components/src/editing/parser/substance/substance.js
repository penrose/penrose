// This file was generated by lezer-generator. You probably shouldn't edit it.
import {LRParser, LocalTokenGroup} from "@lezer/lr"
import {insertSemicolon, trackNewline} from "./substanceTokens.js"
const spec_Identifier = {__proto__:null,for:18, in:20, where:26, Let:64, AutoLabel:68, All:70, Label:72, NoLabel:76}
export const parser = LRParser.deserialize({
  version: 14,
  states: "0UQ`QPOOPqOPOOOvQPO'#CaO!RQPO'#C`OOQS'#Dc'#DcO!ZQTO'#DUQ`QPOOO!cQPO'#CzO!hQPO'#C}O!pQPO'#C}O!uQPO'#C}P!zOWO'#C^POOO)C?Q)C?QOOQO'#C{'#C{O#VQPO,59fO#[QTO,59fO#mQTO,58zO#xQPO'#CxO$ZQTO,59cOOQO'#D{'#D{OOQO,59p,59pOOQO-E7S-E7SO$fQPO,59fO$kQTO'#DdOOQS,59i,59iO$yQQO,59iO%RQTO,59iPOOO'#DT'#DTP%^OWO,58xPOOO,58x,58xOOQO'#Ca'#CaO!UQPO1G/QOOQO'#Cc'#CcO%iQPO'#DVO%nQTO,5:OO#VQPO1G/QOOQS1G.f1G.fO%|QPO'#CdOOQO'#Cp'#CpO&RQPO'#DyO&ZQPO'#DyO&fQPO,59dOOQS1G.}1G.}O&kQTO1G/TOOQS1G/T1G/TPOOO-E7R-E7RPOOO1G.d1G.dO&vQTO7+$lOOQS,59q,59qOOQS-E7T-E7TO!UQPO7+$lO'RQPO'#DfO'WQTO,59OO'cQPO'#DXO'qQPO,5:eO'qQPO,5:eOOQS1G/O1G/OOOQS7+$o7+$oOOQS<<HW<<HWO'yQTO<<HWO(UQPO,5:QO(ZQPO1G.jOOQO,59s,59sO(oQPO,59sOOQO-E7V-E7VO(zQPO1G0POOQSAN=rAN=rO)SQPO'#CgO)XQTO1G/lOOQO'#Cl'#ClOOQS'#Co'#CoO)gQPO'#CoO)uQQO'#CjO(ZQPO'#CoOOQS'#Cj'#CjO(ZQPO'#CjO*gQTO7+$UOOQO1G/_1G/_O*uQPO,59RO*zQTO7+%WO+YQPO'#DWO+vQQO,59ZO)gQPO'#CoOOQO'#Cq'#CqOOQO'#Cr'#CrOOQO'#Cs'#CsOOQO'#Ct'#CtOOQO'#Cu'#CuO)gQPO,59ZO)gQPO,59ZO,WQPO'#CvO)gQPO,59UO,iQQO,59ZO,pQPO,59UOOQS,59U,59UOOQO'#Cm'#CmOOQO'#Cn'#CnO(ZQPO,59UO,{QPO1G.mOOQS-E7U-E7UO-QQPO,59rO-kQPO,59ZOOQS1G.u1G.uO.ZQQO1G.uOOQO,59b,59bO.kQTO1G.pOOQS1G.p1G.pO/OQPO7+$XO(UQPO1G/^O/TQTO,59ZOOQS<<Gs<<GsOOQS7+$x7+$xO/eQTO1G.uO)gQPO'#CoO)gQPO,59ZO)gQPO,59Z",
  stateData: "/|~O!POS!QOSPOS!RPQ~OUQOpVOrWOtXOvYO~O!RZO~O!n]OUTX!fTX~OU_O!faO~O}cO!pcO~OUfO~OUgOshO~OUiO~OUgO~O!SkO!TkO!UmO~OUnO~O!XpO!n]OX!WX}!WX!p!WX~OXuO}Sa!pSa~OUwO[wOmwO!`vO!g!mP~OXuO}ka!pka~O!n]O~O!XpO}!WX!p!WXX!WX~Om{Ou{O~OXuO}qa!pqa~O!SkO!TkO!U!OO~OU!QO~O!XpOX!Wa}!Wa!p!Wa~OU!TO~O!XpO!g!mX~O[!XO!XpO!g!mX~O!g!YO~OXuO}qi!pqi~OXuO}nq!pnq~OY!^O~O]!_O}Wa!pWa~OU!`O[!`Om!`O!`vO~O!XpO!g!ma~OXuO}ny!pny~O![!eO~OU!hO[!hO_!lO!]!gO!`vO!f!kO~O[!oO!X{a!g{a~O!XpO!g!mi~O[!pO~O!X!rO]!Yi}!Yi!p!Yi~OU!hO[!hO!`vO!f!tO~O!`!yO!a!uO!b!vO!c!wO!d!xO!e!yO!h!|O!i!|O!j!|O!k!|O~O!^#RO!_#SO}Wq!pWq~O!X#UO~O!X!rO]!Yq}!Yq!p!Yq~OU#WO~O!`!yO!a!uO!b!vO!c!wO!d!xO!e!yO!gca~O!hca!ica!jca!kca~P+_O!l#[OUjX[jX!`jX!fjX~O!g#YO~P)uO!^#RO!_#SO!g#^O~O[#_O~OY#`O~O!`!yO!a!uO!b!vO!c!wO!d!xO!e!yO~O!g#YO~P-VO!a!uO!b!vO!c!wO!d!xO!`ci!eci!gci~O!hci!ici!jci!kci~P-rO}^i!^^i!_^i!p^i!g^i~P-VO!Z#bO~O}ca!^ca!_ca!pca~P+_O}ci!^ci!_ci!pci~P-rO[_UP!`P~",
  goto: "'n!pPP!qP!t!xP#S#`PP#rPP#xP$U$[$[$`%O%f%f%f%f%r%|!t&QP!t&ZP!tPPPPP&b&h&n&u&{PPPPPPPPP'W'[P'ePPPPPPPPPPPPPPPPPP'hP'kR[PTSOUSROUQo^R!SsUq_grZ!Vwx!W!X!cQt`QzbQ|jQ!Z{Q![!PR!d!]Q!f!^R#c#`Q!n!_Q#P!kQ#Q!mR#^#TX!m!_!k!m#TT#T!n#PU!j!_!m#TQ!s!iQ#O!kQ#X!tS#Y!z#fQ#Z!{Q#]!}Q#a#eR#d#gQxaQ!a!V^!i!_!i!k!m!z!{#TZ#e!t!}#e#f#gW!z!j!s#O#ZX#f#X#]#a#dU!{!j!s#OV#g#X#]#aT!}!j#OQbRQ!PoR!]!SQ^QTs_fQlZR}lQUOReUSr_gR!RrQ!q!fR#V!qS!WwxS!b!W!cR!c!XTTOUQ`RQhWRjYR!UuRyaRdT",
  nodeNames: "⚠ LineComment BlockComment Program TypeApp NamedId Identifier Sep IndexedStatement for in Range Number where BooleanExpression Boolean BoolOp BoolOp BoolOp NumericExpression ArithOp ArithOp ArithOp ArithOp ArithOp ArithOp CompareOp PredicateApp ArgList String Fn_ConsApp Assignment Let Labeling AutoLabel All Label TeX NoLabel",
  maxTerm: 78,
  context: trackNewline,
  skippedNodes: [0,1,2,39],
  repeatNodeCount: 5,
  tokenData: "4P~RwXY#lYZ#}Z[#l]^#}pq#lqr$Srs$atu%}uv*Yvw*_xy*jyz*oz{*t{|*y|}+O}!O+T!O!P+t!P!Q,t!Q![-R![!]-d!]!^-o!^!_-t!_!`-y!`!a.W!c!}'m!}#O.]#P#Q.b#Q#R.g#R#S'm#T#Y'm#Y#Z.l#Z#h'm#h#i2[#i#o'm#p#q3t$f$g#l$g;'S'm;'S;=`([<%lO'm~#qS!P~XY#lZ[#lpq#l$f$g#l~$SO!Q~U$XP!]Q!_!`$[S$aO!kS~$dVOr$ars$ys#O$a#O#P%O#P;'S$a;'S;=`%w<%lO$a~%OOm~~%RRO;'S$a;'S;=`%[;=`O$a~%_WOr$ars$ys#O$a#O#P%O#P;'S$a;'S;=`%w;=`<%l$a<%lO$a~%zP;=`<%l$aU&S^UQOt'Otu'mu!Q'O!Q![(b![!c'O!c!}(b!}#R'O#R#S(b#S#T'O#T#o(b#o$g'O$g;'S(b;'S;=`*S<%lO(bS'RTOt'Otu'bu;'S'O;'S;=`'g<%lO'OS'gOuSS'jP;=`<%l'OQ'rWUQtu'm!Q!['m!c!}'m#R#S'm#T#o'm$g;'S'm;'S;=`([<%lO'mQ(_P;=`<%l'mU(g^UQOt'Otu)cu!Q'O!Q![(b![!c'O!c!}(b!}#R'O#R#S(b#S#T'O#T#o(b#o$g'O$g;'S(b;'S;=`*S<%lO(bU)jWuSUQtu'm!Q!['m!c!}'m#R#S'm#T#o'm$g;'S'm;'S;=`([<%lO'mU*VP;=`<%l(b~*_O!b~~*bPvw*e~*jO!^~~*oO!f~~*tO!g~~*yO!c~~+OO!e~~+TO!X~~+YP!`~}!O+]~+bSP~OY+]Z;'S+];'S;=`+n<%lO+]~+qP;=`<%l+]~+wP!Q![+z~,PR[~!Q![+z!g!h,Y#X#Y,Y~,]R{|,f}!O,f!Q![,l~,iP!Q![,l~,qP[~!Q![,l~,yP!a~z{,|~-RO!R~~-WS[~!O!P+z!Q![-R!g!h,Y#X#Y,Y~-gP!_!`-j~-oO!n~~-tO!p~~-yO!h~U.OP!lQ!_!`.RS.WO!jS~.]O!i~~.bO![~~.gO!Z~~.lO!d~~.qXUQtu'm!Q!['m!c!}'m#R#S'm#T#U/^#U#o'm$g;'S'm;'S;=`([<%lO'm~/cYUQtu'm!Q!['m!c!}'m#R#S'm#T#`'m#`#a0R#a#o'm$g;'S'm;'S;=`([<%lO'm~0WYUQtu'm!Q!['m!c!}'m#R#S'm#T#g'm#g#h0v#h#o'm$g;'S'm;'S;=`([<%lO'm~0{YUQtu'm!Q!['m!c!}'m#R#S'm#T#X'm#X#Y1k#Y#o'm$g;'S'm;'S;=`([<%lO'm~1rW_~UQtu'm!Q!['m!c!}'m#R#S'm#T#o'm$g;'S'm;'S;=`([<%lO'm~2aYUQtu'm!Q!['m!c!}'m#R#S'm#T#f'm#f#g3P#g#o'm$g;'S'm;'S;=`([<%lO'm~3UYUQtu'm!Q!['m!c!}'m#R#S'm#T#i'm#i#j0v#j#o'm$g;'S'm;'S;=`([<%lO'm~3wP#p#q3z~4PO!_~",
  tokenizers: [1, 2, insertSemicolon, new LocalTokenGroup("j~RQYZXz{^~^O!T~~aP!P!Qd~iO!U~~", 25, 50)],
  topRules: {"Program":[0,3]},
  specialized: [{term: 6, get: (value) => spec_Identifier[value] || -1}],
  tokenPrec: 681
})