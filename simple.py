#!/usr/bin/env python3
"""
Simple asynchronous node-and-wire "Hello World" implementation
Converted from Common Lisp to Python

This is a very simple asynchronous node-and-wire implementation of Hello World.
It uses 3 parts:
1. Hello
2. World  
3. Deracer (a de-racer, guarantees that input on port 1 is always output
   before input on port 2, regardless of their order of arrival)
"""

import json
from typing import Any, List, Optional, Callable


# Global top-level output queue
top_level_outputs = []


class Mevent:
    """Message Event - encapsulates a message with a port and payload"""
    def __init__(self, port: Any, payload: Any):
        self.port = port
        self.payload = payload
    
    def to_dict(self):
        """Convert to dictionary for JSON serialization"""
        return {"PORT": self.port, "PAYLOAD": self.payload}


class Part:
    """Computational component in the dataflow system"""
    def __init__(self, name: str, handler: Callable):
        self.name = name
        self.inqueue: List[Mevent] = []
        self.outqueue: List[Mevent] = []
        self.handler = handler
    
    def clear_outputs(self):
        """Reset the output queue"""
        self.outqueue = []
    
    def has_input(self) -> bool:
        """Check if input queue has events"""
        return len(self.inqueue) > 0


class PartPort:
    """References a specific port on a specific part"""
    def __init__(self, part: Any, port: Any):
        self.part = part
        self.port = port


class Wire:
    """Defines a connection between two parts"""
    def __init__(self, sender: PartPort, receiver: PartPort):
        self.sender = sender
        self.receiver = receiver


def wref(part_ref: Any, port_id: Any) -> PartPort:
    """Create a PartPort reference"""
    return PartPort(part_ref, port_id)


def new_wire(sender_ref: PartPort, receiver_ref: PartPort) -> Wire:
    """Create a new wire connection"""
    return Wire(sender_ref, receiver_ref)


def enqueue_input(part: Part, iport: Any, ipayload: Any):
    """Append a new event to a part's input queue"""
    part.inqueue.append(Mevent(iport, ipayload))


def dequeue_input(part: Part) -> Optional[Mevent]:
    """Remove and return the first event from input queue (FIFO)"""
    if part.inqueue:
        return part.inqueue.pop(0)
    return None


def enqueue_output(part: Part, oport: Any, opayload: Any):
    """Append a new event to a part's output queue"""
    part.outqueue.append(Mevent(oport, opayload))


def dequeue_output(part: Part) -> Optional[Mevent]:
    """Remove and return the first event from output queue"""
    if part.outqueue:
        return part.outqueue.pop(0)
    return None


def send(eh: Part, outport: Any, outpayload: Any):
    """Convenience wrapper for enqueue_output"""
    enqueue_output(eh, outport, outpayload)


def enqueue_top_level_output(oport: Any, opayload: Any):
    """Append events to the global top-level output list"""
    global top_level_outputs
    top_level_outputs.append(Mevent(oport, opayload))


# ============================================================================
# Component Handlers
# ============================================================================

def Hello(eh: Part, mev: Mevent):
    """Handler that emits 'Hello'"""
    send(eh, "", "Hello")


def World(eh: Part, mev: Mevent):
    """Handler that emits 'World'"""
    send(eh, "", "World")


# Deracer state - global module-level variable
# [port_1_message, port_2_message]
deracer_state = [None, None]


def deracer_send_maybe(eh: Part):
    """
    Send both messages if both ports have received input.
    Always sends port 1's message before port 2's message.
    """
    global deracer_state
    if deracer_state[0] is not None and deracer_state[1] is not None:
        send(eh, "", deracer_state[0].payload)
        send(eh, "", deracer_state[1].payload)
        deracer_state = [None, None]


def Deracer(eh: Part, mev: Mevent):
    """
    De-racer handler - guarantees port 1 output before port 2.
    Stores messages from both ports and outputs them in order
    when both have arrived.
    """
    global deracer_state
    if mev.port == 1:
        deracer_state[0] = mev
        deracer_send_maybe(eh)
    else:  # port 2
        deracer_state[1] = mev
        deracer_send_maybe(eh)


# ============================================================================
# Dispatcher & Routing
# ============================================================================

def repurpose_mev_and_enqueue(part: Any, port: Any, sender_oriented_mev: Mevent):
    """
    Forward an event to its destination.
    If part is "" (empty string), enqueue to top-level outputs.
    Otherwise, enqueue to the specified part's input queue.
    """
    if part == "":
        enqueue_top_level_output(port, sender_oriented_mev.payload)
    else:
        enqueue_input(part, port, sender_oriented_mev.payload)


def route_outputs(p: Part, wires: List[Wire]):
    """
    Route a part's output events to receivers based on wire definitions.
    Clears the part's output queue after routing.
    """
    for output_mev in p.outqueue:
        for w in wires:
            if (w.sender.part is p and 
                output_mev.port == w.sender.port):
                repurpose_mev_and_enqueue(
                    w.receiver.part,
                    w.receiver.port,
                    output_mev
                )
    p.clear_outputs()


def dispatcher(parts: List[Part], wires: List[Wire]):
    """
    Core execution loop that processes all queued messages.
    Continues until all parts have empty input queues.
    """
    while any(part.has_input() for part in parts):
        for part in parts:
            if part.has_input():
                mev = dequeue_input(part)
                if mev:
                    part.handler(part, mev)
                    route_outputs(part, wires)


def output_top_level_outputs():
    """Serialize top-level outputs to JSON and print"""
    output_list = [mev.to_dict() for mev in top_level_outputs]
    print(json.dumps(output_list))


# ============================================================================
# Main Execution
# ============================================================================

def main():
    """
    Entry point that sets up and runs the dataflow network.
    
    Setup:
    1. Creates three parts: Hello, World, Deracer
    2. Defines wiring:
       Hello → Deracer (port 1)
       World → Deracer (port 2)
       Deracer → Top-level output
    3. Seeds initial inputs
    4. Executes dispatcher
    5. Outputs results as JSON
    """
    global top_level_outputs, deracer_state
    
    # Reset global state
    top_level_outputs = []
    deracer_state = [None, None]
    
    # Create parts
    part_deracer = Part(name="1then2", handler=Deracer)
    part_world = Part(name="World", handler=World)
    part_hello = Part(name="Hello", handler=Hello)
    
    parts_list = [part_deracer, part_world, part_hello]
    
    # Define wiring
    # LEGO-ness is enhanced by not hard-wiring connections between parts
    # Parts output only to their own output queues, then the dispatcher
    # routes the events to intended receivers based on this wiring list
    wires = [
        new_wire(wref(part_hello, ""), wref(part_deracer, 1)),
        new_wire(wref(part_world, ""), wref(part_deracer, 2)),
        new_wire(wref(part_deracer, ""), wref("", ""))
    ]
    
    # Seed initial inputs
    enqueue_input(part_hello, "", "")
    enqueue_input(part_world, "", "")
    
    # Run the dispatcher
    dispatcher(parts_list, wires)
    
    # Output results
    output_top_level_outputs()


if __name__ == "__main__":
    main()
