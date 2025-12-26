aside: I think that dbm's comment about LEGO parts is very significant.
I think that we can use first-class functions ("closures") and FIFO queues to implement LEGO parts very simply and avoid the overhead of using operating systems (an outdated, 1960s, bloated, Greenspunian concept)

---

I've supplied some seed information below and I've attached a common lisp program `simple.lisp`.
Including consideration of the seed information:
1. Summarize `simple.lisp`
2. Document `simple.lisp`
Seed information:
- This is a very simple asynchronous node-and-wire implementation of Hello World. It uses 3 parts
1. Hello
2. World
3. 1→2 (a de-racer, guarantees that input on port 1 is always output before input on port 2, regardless of their order of arrival, both outputs are sent separately to output port "")
- I believe that I can't use Unicode characters in the name of functions, so I use the name "Deracer" instead of "1→2" 
- this simple version is "flat" and does not provide for recursive Container parts, opting instead to use a single top level output queue
- illegal states and error conditions are not handled, for the sake of brevity (and, because this sort of stuff should be handled diagrammatically, and not textually)
