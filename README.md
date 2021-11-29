# typestrong
Ostensibly, a Haskell-oriented typing tutor

## Premise
The typing inaccuracy that slows me down the most is the number row, and
specifically punctuation (not that other punctuation is great). This is true
mostly while programming and sometimes while typing a password (who does that
any more? gotta get into your password manager).

Most typing-tutor corpora focus on words. This doesn't help.

There are tutors with source-code corpora. Even when this fits your language,
typing through a page of code gets me too hung up on the structure and
semantics of the thing I'm typing out (and sometimes coding style choices that
annoy me), not specifically "get better at code-punctuation".

So: I'm trying to build a typing tutor that focuses on chunks that are useful
for developers looking to improve accuracy. Oh wait, I have another digression.

### Accuracy
Anecdotally, when I watch other people code as we pair, or when I'm writing code
myself, accuracy is a bigger deal in terms of maintaining trains of thought
(i.e. not losing flow state, keeping working memory working, or whatever other
terms you like for "staying happy and not losing track from frustration") than
pure speed. Many typing tutors penalize accuracy only second-hand by way of the
amount of time it takes you to backspace over your mistakes and correct them. I
plan to score accuracy first and then speed, since this is a training tool
rather than a bragging-rights generator. "Slow is smooth and smooth is fast."

So my plan is to read a source code corpus and chunk it into tokens, then
present those tokens to the user roughly weighted by difficulty (which can start
as "how much of this token is punctuation?" and go from there). Since the idea
is to present tokens as "closed conceptual chunks" rather than just random text,
I want to do _something_ to make them a bit more intelligible than just split on
whitespace.

### Fusing tokens
Consider tuples in Haskell. A line of code with a tuple in it might look like:

```haskell
  t:ts | isClosed tHead -> (tHead, tTail)
```

(This is part of a `case` expression, that doesn't really matter.)

Throwing simply `(tHead,` into a training set isn't _great_, there's unresolved
tension for the user because tuples usually close. So here we'd like to combine
the two "words" into one token, `(tHead, tTail)`.

We'd need a stack machine to keep track of opens vs closes on parens, braces,
and other "symmetric punctuation". But if we assume that the corpus is code that
compiles, we can offload that to the compiler and just count matching opens and
closes until we zero out.

That doesn't quite work all the time, though, since e.g. record definitions are
typically multi-line, and things like import lists can get very long. So if we
fuse tokens to try to make them "closed under symmetric punctuation", we should
give a best effort but not try _too_ hard. Crossing newlines is too hard, so is
creating a token that's idk 20 characters or longer.