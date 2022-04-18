# weicc: A Small C Compiler

**For the weicc implementation docs, please refer to [https://a4org.github.io/cs/](https://a4org.github.io/cs/)**

## Introduction (quoted from cs)

As I mentioned above, if you already have an implementation of language X, there is no logical contradicton in creating a new language X compiler using that language itself. If you want to self-host, you can simply proceed with the development with an **existing compiler and switch when your own is complete**. 

And that is exactly what I am trying to implement in this series - **A C language compiler that is implement using C**.
Initially, I plan to create a new language, but I can't come up with any new features or peculiarities for this new language, even if I implement it successfully, it may just a mediocre, buggy and flawed language. 
Designing and implementing the original language is good in terms of improving the design sense of the language, but as I mentioned above, it also has its pitfalls: You may design some specification of your own language intentionally where implementation is cumbersome.

After watching the **[chibicc](https://github.com/rui314/chibicc)** by **[Rui](https://github.com/rui314/chibicc)**, I finially choose to implement a C compiler from scratch for these reasons:

1. **C language has clear and reasonable specification**
2. **C compiler translate C program into machine language (assembly), so by creating a compiler, I may learn how program works in real CPU at the same time C itself.**
3. **C is so widely used that once the compiler works, you can compile and play with third-party source code:**
    * Xv6 - a mini unix operating system that I introduced in [os](https://a4org.github.io/os/index.html).
    * Linux (if the compiler is perfect enough)
4. **C++ has such a huge language specification that it is impossible to easily create a self-made compiler.**
5. **There are already lots of self-implemented C compilers for reference, such as:**
    * **[chibicc](https://github.com/rui314/chibicc)** by **[Rui Ueyama](https://github.com/rui314)**.
    * **[lcc](https://github.com/drh/lcc)** by **[Dave Hanson](https://github.com/drh)**.
    * **[SmallerC](https://github.com/alexfru/SmallerC)** by **[Alexey Frunze](https://github.com/alexfru)**
    * **[TinyCC](https://repo.or.cz/w/tinycc.git/tree)** by **[Fabrice Bellard](https://bellard.org/tcc/)**
    * ....

I call this new compiler **weicc**. You can find the newest update of this project on **[https://github.com/Angold-4/weicc](https://github.com/Angold-4/weicc)**.
I pronounce weicc as way cee cee. "wei" means "mini" or "small" in Chinese. "cc" stands for C compiler.

I am going to blog the journey while I implement this compiler, also as weicc's documentation, this is the first, soon the second, and so on...
When I am writing this first document, I had been working on weicc for a while, and I had Implemented a lot of features: **variables**, **statements**, **control structures**... And I believe that as the documentation continues to grow, soon I will have a self-made C compiler.

**The compiler can be conceptly divided into multiple stages such as `lexical analysis`, `parsing` and `code generation`. A common textbook approach would be to have chapters on each topic. In this way, while it does reduce such complexity by splitting the compilation process into several stages, I still don't think it is very easy to implement in stages.**
The reason is that with the development method created for each stage, it is not possible to run the compiler until all stages are completed, which is sometimes couterintuitive **(In my opinion, nothing is hard, they are just complex, and if we want to learn something, we have to start simple, because complex usually means hard and understanding based on this "hard" shell usually means decisively mistake.)**

In this implementation, I decided to take a different approach. We start from a **"propreitary language"** with a very simple language specification such as a calculator in order to go through and understand each compiler stages. Then we will add functionality to the "unique language" throughout our journey, and eventually developing it to match C.

weicc is influenced (at least trying to) by **[Rob Pike](https://en.wikipedia.org/wiki/Rob_Pike)**'s way of **[thinking about programming](https://users.ece.utexas.edu/~adnan/pike.html)**. Rob Pike is a former colleague of C author **[Dennis Ritchie](https://en.wikipedia.org/wiki/Dennis_Ritchie)**, he created go language and UTF-8 with **[Ken Thompson](https://en.wikipedia.org/wiki/Ken_Thompson)**.

I quote these five rules in the index page of **[cs](https://a4org.github.io/cs/index.html)**, I really like the Rule 5, which can be shortened to **"write stubid code that uses smart objects."**
