#Hello

To get started, simply copy src/Config.hs.template to src/Config.hs and specify at least srcPath. Continue by running "stack exec java-components-printer" in your terminal while in the project's folder.

# What is this?
This piece of code is developed for a specific project to identify unused java-files. However, a lot of the parts are generic and specific for Spring MVC rather than our specific project. So I extracted the configuration required for making to work with our project into a separate file in order to make it easy for anyone else to play around with the code and use it. I managed to get rid for 1593 LOC spread in 47 files. Not much considering how large codebase I was working with, but I guess a larger number would not have been so nice too see either.

# Why
Many people asked me why I was building this. Surely there are tools out there that can find unused files in a Spring MVC application. However, when looking around I didn't find many nor any that worked well for our project. Maybe it's possible to configure IDEA to work for specific projects, but if I'm going to invest time anyway, why not take the opportunity to spread a little Haskell in the office :) Furthermore, developing software is my passion. Being a pragmatic person I do use existing code and tools when possible to save time. But in this case I combined wanting to code a little Haskell with creating something useful and spreading some functional programming in the office. Win-win-win!

# How
In essenece, I'm reading all the .java-files in a folder, building a AST using language-java (https://hackage.haskell.org/package/language-java) and then traversing the AST to collect information that can later be used to decide if a file is used or not. The core information I'm looking for is whether any other file imports the file. However, since Java does not require an import statement if the dependency is within the same package I also look for other things such as references and autowired classes. The next step is to use the information to actually find unsused files.

To find unused files I'm building a Graph. An edge means that a file is used by another file. So the challenge here is to actually add an edge everytime the file is in fact used. An obvious thing to do is to add an edge for every import statement. To improve the result further I'm adding edges for references within a package, eg. used classes or methods within the package. However, Spring MVC has a powerful dependency injection system. It supports injecting dependencies and still only relying on interfaces. You can get all classes of a type (interface) injected or one specific instance but still only depending on it's interface. Recall that I was taking autowired classes when harvesting the AST. In addition to this I'm also finding super classes. Using this I filter out any file that was autowired, either directly or via an interface.

# Final Thoughts
I'm quite happy with this project and feel that it will help me in spreading some functional programming in the office. I do feel that I didn't manage to show off the full power of Haskell, for example by using monads or a monad transformer stack. But I'm sure I will have plenty of time to show off these concepts in the future. I realized half-way that many programmers probably haven't been playing with ASTs after studying, which might make it difficult for them to compare Haskell with for example Java or Python. Ideally I should have chosen a simpler project for showing the power of Haskell. But just because something else might be better doesn't mean this can't be awesome!

Not having convinced everyone to use Haskell at work, I'm in general restricted to evenings and weekends for learning when to apply what concepts and so. Do you happen to be an experienced Haskell developer? Please fork, send a PR or just some feedback. A simple "you could have used ... to greatly simplify your code" or "... is a bad practice due to ... and you should instead use ..." would be very helpful.

Cheers
Peter @ Small Improvements
