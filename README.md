# **DFA Manual**

**By:** Juan Muniain Otero and Andrew Dunkerley Vera

This Deterministic Finite Automaton's function is to accept mathematical expressions in the form of strings, and detect each "token" or element in the string in order to print it and its name. 

**Automaton diagram**

![](/automaton.jpg "Automaton diagram")

## **Program result**

When changing the string inside the expression '(arithmetic-lexer "(your string here)")' found at the end of the source code, an output of every found character and its action should be cisible in your terminal.

You can also use automated tests by using the rackunit library.

**Note:** comment detection is still a work in progress. Tests and inputs will fail.

## **How to run this program**

Make sure to download the [latest version](https://download.racket-lang.org/) of Racket installed. You can run the program in the editor of your choice, however, opening the racket file will probably default to Racket's IDE. After opening the DrRacket IDE, make sure to have the racket language selected by going to the <em>Languages </em> tab, and selecting the racket language. After that, pressing <em>play </em> will output the results.
