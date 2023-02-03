# Steps to recreate the analysis

Go here:
<https://www.aph.gov.au/Parliamentary_Business/Hansard/Hansreps_2011>

Then view source:
view-source:<https://www.aph.gov.au/Parliamentary_Business/Hansard/Hansreps_2011>

Observe the URLSs you want:
bid=chamber/hansardr/25466/&sid=0000
...

Or wait -- just do some googling for
"australian parliament hansard full text github"

and find someone has already done it:

<https://github.com/wragge/hansard-xml>

download all that.
pop it in the /data directory.

Now you've got a giant corpus of text to search through.

We'll search for something related to an accusation of lying, because I saw
this:

"You are trying to lie your way out of trouble again. You are a compulsive liar.
You are the only liar who has been Prime Minister in the 1 7 years I have been
here."

Just come up with a bunch of similar sentences:

sentences <- c("You're a liar", "You're lying", "I've never met such a big liar
in my life", "You are trying to lie your way out of
trouble again", "You're a pathological liar", "liar, liar, pants on fire!")
