# Summary of simple.lisp
This program implements a minimalist asynchronous node-and-wire dataflow system demonstrating message-oriented concurrency. It orchestrates three components (Parts) to produce "Hello World" output with guaranteed ordering:

Hello Part: Emits "Hello" when triggered
World Part: Emits "World" when triggered
Deracer Part: A synchronization component that ensures port 1 input always outputs before port 2 input, regardless of arrival order

The architecture uses:

Event-driven messaging: Components communicate via Mevent objects (port + payload)
Wire-based routing: Explicit Wire objects decouple senders from receivers, enhancing modularity
Queue-based processing: Input/output queues on each Part, plus a global output queue
Dispatcher loop: Processes all queued inputs until exhausted, routing outputs according to wire specifications

The program intentionally omits recursive containers, error handling, and uses ASCII names for functions (e.g., "Deracer" instead of "1→2") to maintain simplicity and Common Lisp compatibility.

# Further
![Documentation](README-DOCUMENTATION.md)
![Python version](simple.py)
