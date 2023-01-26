# Substance Language Overview

To define a diagram, you must be specify the objects in the diagram and relationships among these objects.

Substance uses concise assertions that resemble standard mathematical prose. Formally, it can model any domain expressible in a compositional language of types, functions, and predicates, which are the basic constructs found in conventional mathematical notation [Ganesalingam 2013]. Just as definitions are typically immutable in mathematics, Substance draws inspiration
from strongly typed functional languages (such as ML [Milner et al. 1997]) where objects are stateless.

A conscious design decision is to exclude all graphical data (coordinates, sizes, colors, etc.) from Substance — since its sole purpose is to specify abstract relationships rather than quantitative data. All such data is instead specified in Style or determined via optimization. This division relieves users from the burden of tedious and repetitive graphics programming, which can instead be factored out into reusable Style code.

Existing languages would be difficult to use in place of Substance since they lack the semantics needed to encode complex logical relationships and do not provide language-level extensibility. For instance, TEX [Beeton and Palais 2016] and MathML [Miner 2005] markup provide only enough information to translate plain text into mathematical glyphs; computer algebra systems like Mathematica and Maple have limited type systems or provide only a small set of fixed predicates (e.g., asserting that a number is real). Conversely, the much richer languages used by automated theorem provers and proof assistants (such as Coq [Bertot and Castéran 2013] and Lean [de Moura et al. 2015]) are overkill for simply specifying diagrams. A custom language provides simple, familiar syntax and clear error messages. We do however adopt some ideas from Coq, such as the ability to customize syntactic sugar (Sec. 3.1).
