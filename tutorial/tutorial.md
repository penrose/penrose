# ✨ Welcome, we are so glad you are here.
Ready to make some beautiful diagrams? You are at the right place. Penrose is accessible for people with a variety of backgrounds including mathematical domain experts as well as people with no programming experiences. 

# What's in this tutorial? 
This tutorial consists of 3 individual sections, each containing the following content:
- **Example**
    - We will show and guide you to understand each line of the code that produces a specific diagram. Furthermore, we will be teaching you how to navigate in the Penrose repository for all the resources you will need. Each example teaches a specific set of skills. 
- **Exercises (with solutions)**
    - These exercises act as sanity checks to confirm that you have comprehended the example. It consolidates your knowledge and tests your understanding. We also have the solutions available for you to check your work. 
  
The sections build on top of each other, using the skillsets we've acquired in the previous exercises, therefore we highly recommend you to follow through the exercises in order.  

# Installation
We have a detailed wiki page on how to get Penrose up and running on your computer [here](https://github.com/penrose/penrose/wiki/Building-and-running). :partying_face: Come back when you are done.

After following the Building & Running page, you should now have your own directory to run your code. It might look something like this,

![initialize files](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/initializa_file.png)

Before getting into the granularity of code, a basic understanding of a Penrose program will make you feel less overwhelmed. Learning a new environment can be intimidating, and we want to make it as smooth as possible.

# How do we diagram by hand? 
Now, for a second, I want you to recall how you would normally diagram a concept using a pen or pencil. 

It will most likely involve the following steps,
* **Decide on what you are diagramming**
    * There must be something that you want to diagram, so you are here pondering how to diagram it. 
    * Let's say we are making a diagram of things in your house, then the **domain** of objects that we are working with includes furnitures, plants, utensils, and everything that is in your house. Furthermore, furniture / plants / utensils are specific types of objects in your houses (the domain). 
* **Have a (mental) list of the objects you want to include in the diagram**
    * We either write down, or mentally construct the list of objects that will be included in our diagram.
    * They are the **substances** of our diagram that will be visualized. 
    * For example, your chair :chair: is a particular instance of an object in the domain :house:, and :chair: has the type of _furniture_. If the chair is in the diagram, then it is a substance of the diagram. 
* **Figure out the relationships between the objects**
    *  If we were only to put the list of things on paper one by one, that would not be an interesting nor useful diagram. Normally, there are relationships that we want to visualize. 
    * For example, we could group the plants based on the number of times they need to be watered on a weekly basis. Then we would have visual clusters of elements.
* **Explore different visual styles**
    * Drawings commonly require explorations and various attempts with colors, sizes, and compositions. The same concept can be visualized in a number of different **styles**. 

### The process of creating a Penrose diagram is similar to our intuitive process of analogue diagramming. :tada: 

# What makes a Penrose program? 
It follows naturally from the process of diagramming by hand, we need to keep track of certain information as described above. The way we do that is by writing code in three specific files. First, we need to define our **domain** of objects because Penrose does not know what is in your house, or what a chair is. You need to define the types of objects and operations in your domain. For example, you can _push_ a chair, or _sit_ on a chair, which are operations related to a chair. Second, we need to store the specific **substances** we want to include in our diagrams, so Penrose knows exactly what to draw for you. Lastly, we need to define the **styles** that we want to visualize our substances in. Each of these corresponds to a specific file with an intuitive file extension designed for accessibility. 

Every Penrose program consists of 3 files: 
* A `.dsl`  file that defines the language specific to the domain. 
* A `.sub` file that creates substances of mathematical content.
* A `.sty` file that specifies the style of the visual representation.

> dsl stands for Domain Specific Language. 

# ( diagram about files )

In general, for each diagram, you will have an individual `.sub` file that contains the specific instances for the diagram, while the `.dsl` and `.sty` files can be applied to a number of different diagrams. For example, we can have various diagrams in the domain of Linear Algebra that visualizes different concepts with different `.sub` files, but we would have a main `linearAlgebra.dsl` file and maybe several `linearAlgebra.sty` file for different styles. 

# Example (1)
This is the first diagram we will make together. This is the equivalent of the ```print("Hello World")``` program for Penrose.

![exercise 1 result](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/2sets_nolabel.png)

This is what you will achieve at the end of Example (1). 

### :speaking_head: WHAT IS THIS?
Some of you who have experiences with set theory may recognize that ellipses are common for representing sets, and that's exactly what we have here. We have 2 sets without names (we will get to labeling later :grimacing:).

### :page_facing_up: DOMAIN
It follows naturally that our mathematical **domain** is Set Theory. Therefore, we can rename our `.dsl` file to `setTheory.dsl`.  

Recall that a `.dsl` file defines the possible types of objects in our domain. Essentially, we are _teaching_ Penrose the neccessary vocabulary that we use to communicate our concept. For example, Penrose has no idea that there are objects of type plant or furniture in a house, but no worries, we can let Penrose know with several lines of code :speaker:. We declare a new type following the syntax of `type TYPE_NAME`. For example, if we want Penrose to know that there are objects of type plant, we would do `type Plant` or `type plant`. We normally capitalize type names. 

#### :question: POP QUIZ: What's the most fundamental type of element in Set Theory? (hint: the name gives it away.)

The answer is a **Set**! A set is a **type** of element in set theory. Therefore in our `setTheory.dsl`, we write the following line,

`setTheory.dsl`
```typescript
type Set
```
And that is all we need for this exercise in `.dsl`! :tada: Now Penrose knows that there are objects of type Set. 

If you look closely at the repository, we have a [domain file](https://github.com/penrose/penrose/blob/main/examples/set-theory-domain/setTheory.dsl) that contains more extensive operations common in Set Theory such as `Intersection`, `Union`, `Subset`, and more. 

### :page_facing_up: SUBSTANCE
Since we are visualizing 2 sets, they are our **substances** for this diagram. 

We declare a substance by first declaring its *type* followed by its *name*. The name will not appear in the diagram unless you choose to label your substances, therefore in this exercise, it doesn't matter how you name your sets. 

`twosets.sub`
```typescript
Set A 
Set B 
```

Now, Penrose will know that you want two substances of type `Set` in your diagram. :tada: 

Here we have capitalized `Set` because recall in our `setTheory.dsl` file, we wrote `type Set`, and if we did `type set` instead, we would declare our set with `set A` here. There is no magic here, you define your Penrose world completely. :earth_americas: :sparkles:

### :page_facing_up: STYLE
For style, we have a little more work to do. A `.sty` file is essentially a `.css` file for your `html` (which wouold be our `.sub` file). We will rename our `.sty` file to `twosets.sty`. 

Now, Penrose does _not_ know a set is commonly represented as a circle. **We need to style our elements from scratch.** This might seem strange, but this way you are given absolute freedom in how you want to represent your substances in the diagram. Your set doesn't have to be a circle, it can be square, a rectangle, etc. But for this example, we will be representing sets as circles. 

The syntax for declaring styles goes like this,

![style syntax](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/style_syntax.png)
## WILL REPLACE 

Note that the syntax is _very_ similar to the mathematical way of talking about objects, so it should be pretty natural for people with some backgrounds in math. If you don't, that's completely fine too! You can interpret the syntax as:  we go through the substances in the diagram, and _for all_ the substances of type `t` that we see, we apply the same styling as defined in the `{ }`. 

Here we have our `type t` as `Set`, and we want to all of our sets to be a circle. We can make that happen by setting the `.icon` field to a shape object. 

`twosets.sty`
```typescript
forall Set x {
    x.icon = (* some shape object *)
}
```

So, what are the shapes we can use? Currently, the system supports 12 different shapes, and you can find the specs for every shape [here](https://github.com/penrose/penrose/wiki/Shape-library). It is a page that you will visit frequently as you work in Penrose. 

![Circle Spec](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/circle_spec.png)

This is the specification for the shape **Circle**, and all the other shapes we have available are documented in the same way. You can see a sample diagram of how the shape will look like, and a table that lists out the different properties you can manipulate. 

When we construct the `Circle` object for our Set, we need to look back at our goal diagram. Our desired circles are strokeless, therefore we will set `strokeWidth: 0.0`.  For all the other properties that we did not specify, Penrose will choose the best value for them based on optimization, or in other words, Penrose will choose the best for you, so you do not have to worry.  

`twosets.sty`
```typescript
forall Set x {
    x.icon =  Circle {
        strokeWidth : 0.0
    }
}
```
And that's a wrap! :tada: 

### :building_construction: COMPILE
Now it's time to see all of our hardwork (drumroll please 🥁)!  To compile your Penrose programs (more detailed description [here](https://github.com/penrose/penrose/wiki/Getting-started)), you need: 
* two terminals opened both at the penrose root directory
* run `yarn start` in one to get the browser window to pop out
* run `roger watch twosets.sub twosets.sty setTheory.dsl` to send the files over to the server
* Refresh! 

# Exercise (1) 
Now, you understand the differences between and usage of the `.dsl`, `.sub` and `sty` files. We have 3 challanges for you that will not require you to create new files, but only work within the existing files. **Hint:** Make use of the shape specs [here](https://github.com/penrose/penrose/wiki/Shape-library).
* **Challenge 1:** Add another `Set` to the diagram. 
![challenge 1 result](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/e1c1.png)
* **Challenge 2:** Keep 3 sets. Represent `Set` as squares with `side` equal to `50.0`. 
![challenge 2 result](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/e1c2.png)
* **Challenge 3:** Keep 3 sets. Represent `Set` as rectangles with `strokeWidth` equal to 15. 

After you are done, you can double check your work with [solutions](https://github.com/penrose/penrose/blob/docs-edit/tutorial/exercise1-sol.md).
