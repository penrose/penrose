# ✨ Welcome, we are so glad you are here.

This tutorial consists of 3 individual exercises, teaching you all the skills you would need to make your own domain from scratch! Each exercise builds on the concepts taught in the previous exercise(s), so we highly recommend you to follow through and understand each exercise as you go. Feel free to shoot us a message if you get stuck anywhere at team@penrose.ink.

# Installation
We have a detailed wiki page on how to get Penrose up and running on your computer [here](https://github.com/penrose/penrose/wiki/Getting-started). Come back when you are done. 

# What makes a Penrose program? 
Every Penrose program consists of 3 files: A `.dsl`  file that defines the language specific to the domain, a `.sub` file that creates substances of mathematical content, and lastly, a `.sty` file that specifies the style of the visual representation. We call a group of these programs (**DOMAIN, SUBSTANCE, STYLE**) that can be used together a "triple." 

![triple](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/triple.png)

> Rest assured, you do not need to understand how it is compiled and rendered to start creating diagrams using Penrose. 

After following the getting started page, you should now have forked Penrose and have created a new folder in the `penrose/examples` path. So you should have something like this.

![initialize files](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/initializa_file.png)


# 📖 Exercise (1)
This is the first diagram we will make together. This is the equivalent of ```print("Hello World")``` program for Penrose.

![exercise 1 result](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/2sets.png)
This is what you will end up with at the end of our exercise 1. 

### What is this? 
Some of you who have experiences with set theory may recognize that ellipses are common for representing sets, and that's exactly what we have here. We have 2 sets, set A and set B. 

### DOMAIN
It follows naturally that our mathematical **domain** is the set theory. Therefore, we can rename our `.dsl` file to `setTheory.dsl`. Now recall what does a `.dsl` file do? It defines the language we use to describe the specific mathematical domain. Now I have a question for you,

#### 📒 What's the most fundamental element in set theory? (hint: the name gives it away.)

The answer is a **Set**! A set is a **type** of element in set theory. Therefore in our `setTheory.dsl`, we write the following line,
```typescript
type Set
```
And that is all we need for this exercise! 

> If you look closely, we have a `penrose/examples/set-theory-domain/setTheory.dsl` file that contains more extensive operations common in set theory such as `Intersection, Union, Subset`, and more. 

### STYLE
For style, we have a little more work to do. A `.sty` file is essentially a `.css` file for your `html`(which wouold be our `.sub` file). The syntax for declaring styles goes like this,

![style syntax](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/style_syntax.png)

```typescript
forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }

    x.text = Text {
        string : x.label
    }

    ensure contains(x.icon, x.text)
    ensure minSize(x.icon)
    ensure maxSize(x.icon)
    encourage sameCenter(x.text, x.icon)
    x.textLayering = x.text above x.icon
}
```

### SUBSTANCE
They are our mathematical **substances** that we want to visualize. Therefore... (drumroll 🥁)
    
```typescript
Set A
Set B 
AutoLabel All
```
