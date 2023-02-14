# Examples of basic loop and function structures in `R`

This is a simple repository ('repo') meant to demonstrate the use of basic programming concepts for social science applications. 

You can copy the repo to your computer using the command line, or downloading it as a zip file then unzipping it. 

Go to green `<> Code` button and either use the Github CLI command (if you have the `gh` command line interface installed), or click "Download ZIP." Then unzip it, and open the `.Rproj` file.

## 1. The corpus

We need a fairly vanilla dataset just to practice on -- ideally it will be a lot of litle files. How about the Australian Hansard? 

We could try to gather it ourselves... or just google "australian parliament hansard full text github" (protip: adding 'github' to a search can sometimes turns up interesting & usefu public resources.)

In this case: <https://github.com/wragge/hansard-xml>

So we download a bunch of those directories. Then pop them in the `./data` directory for this project.

Now we've got a little corpus of text to search through.

## 2. Strings to search for

Just browsing the files, I saw this amusing invective: 

```
"You are trying to lie your way out of trouble again. You are a compulsive liar.
You are the only liar who has been Prime Minister in the 17 years I have been
here."
```

So let's try to come up with a fuzzy search algorithm to find sentences that include an accusation of lying.

This is very rough-and-ready, but here are some potential sentences:

```
sentences <- c("You're a liar", "You're lying", "I've never met such a big liar
in my life", "You are trying to lie your way out of
trouble again", "You're a pathological liar", "You're a compulsive liar", "liar, liar, pants on fire!")
```

(Note: the pedagogical value of this exercise is the use of for loops and functions to do things to, or find things in, files... If we *actually* wanted to find examples of politicians accusing other politicians of dishonesty and lying, there may be other and probably better ways of doing it.)

## 3. Basic algorithms for fuzzy searching for those strings

The remainder is in the file `fuzzymatch.R`, which is best worked through interactively. 