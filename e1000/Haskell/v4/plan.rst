==========
TODO
==========

 - Reduce the precondition dependency list

==========
Questions
==========

 - Why do I have module and node?

------------------------------------------------------------------------
Add pre-conditions and post-conditions.
    --> Few examples of these conditions as starting point
    --> Data-structure for pre and post conditions
    --> Code to generate all possible combinations of these graphs


------------------------------------------------------------------------
Q: What is a **condition**?
A:
  * It can be a unit condition, or combination of condition
  * They can be combined with
    - AND
    - OR
  * Basic conditions are called combinators
    - They can be expressed in high level labels
    - Will get translated into bit-level value conditions
    - Conditions will be
      + ==
      + !=
      + <=
      + >=
      + between range
  * Condition will be a function which, when applied on packet,
        returns true or false

------------------------------------------------------------------------
Q: Why am I thinking about packet here?  Is there any other way to approach
this problem?
A: What are the possible way to think about this problem?
Or, in what different ways I can approach the formulation of this problem?
 * What will happen to each packet?
    - This is what I have been doing most of the time.  For every incoming
        packet, you decide what will happen to it.  And based on these
        decisions, you figure out whole stack.
 * What is the data-flow? What are different decisions made?
    - Forget about packet, concentrate on decisions here.
        eg: decision X is connected to decision Y, etc
        These decisions can be about packets, or about something else.

Conclusion: decision making sounds better.  It is more generic, and can be
applied to more than one situations.

------------------------------------------------------------------------
How should I abstract out the decision process?

------------------------------------------------------------------------
What I want to capture?
How the decisions are made

------------------------------------------------------------------------
Why I want to capture?
I want to capture the functionality of NIC hardware and the state of
network stack, so that I can do better matching between NIC hardware
and network stack.

What is the functionality of NIC?
    --> Packet processing: That is, deciding what to do with given packet.
    --> They react to events
        - Incoming packet
        - events from user
            + new packet
            + configuration change

What is the functionality of logical protocol stack?
    --> Tells the state of system
    --> How system is currently configured to react to particular event?
    --> How system is currently making decisions?

Point is that, these decisions can be implemented in multiple ways.

This is a system where **event** is an input and **action** is an output.
What happens in between is computations and decisions.

Event can lead to more than one action.

------------------------------------------------------------------------
There has to be a notion of ``Computation`` or ``Action``:
 * Examples of computations:
   + These are intermediate steps
   + Takes some input (ie: packet, configuration), and produces some output
   + These outputs are used by decision making blocks
   + Why can't I just imply computation inside decision making?
        This is the way I am doing things right now.
    - Checksum
    - Hash
    - Filter
 * Examples of Action
   + Actions are terminal.
   + They are final outcome of decision making
    - Copy
    - DMA (isn't it just a copy?)
    - Drop
    - Send
    - Notification generation
   + What about actions related to device configuration?

------------------------------------------------------------------------
Question: Implications of **Assuming computations inside decisions**
Ans:

  * Pros:
    + Easy as there are no separate blocks.

  * Cons:
    + Loss of flexibility as decision and computations are merged
    + This can't exploit situations where decisions and computations
        can be separated and done at different places.
    + May be, it can be simulated with decisions which just do computations
        and call only next guy instead of choosing between list of next
        elements.

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
 * What is a decision?
    - For given set of input, it selects one of the possible output from
        finite set of outputs.

 * Can condition be a decision?
   - Condition can be a binary decision which leads in True/False answer.
   -

------------------------------------------------------------------------
Few examples of these conditions as starting point

* IPv4
  - Preconditions
    + is it valid layer 2 packet?
      - l2 checksum
      - l2 length
      - protocol id IPv4
    + Post condition
      - valid ip
      - valid TTL
      - protocol id
        - TCP
        - UDP
        - ICMP

eg:

 * NIC gives out packet
 * Ethernet module
    - accepts packets
    - gives out
        + invalid packet
        + IPv4 packet
        + IPv6 packet
 * IPv4 module
    - accepts IPv4 packet
    - gives out
        + Invalid packet
        + TCP packet
        + UDP packet
        + ICMP packet
 * IPv6 module
    - accepts IPv6 packet
    - gives out
        + Invalid packet
        + TCP packet
        + UDP packet
        + ICMP packet
 * UDP module
    - accepts UDP packet
    - gives out
        + Invalid packet
        + DNS packet
        + NFS packet


Post-condition for IPv4
--> invalid or TCP or UDP or ICMP

Pre-condition for UDP
--> UDP



------------------------------------------------------------------------
Question: Can a node have more than one pre/post conditions?

ANSWER: Lets start with pre-condition.

------------------------------------------------------------------------
What is pre-condition?
It is a condition which must be meet.  It qualifies packet to go to next step.
eg: packet has IPv4 protocol tag.

------------------------------------------------------------------------
Can there be more than one pre-condition?
Which also translate to that can there be more than one path to reach some
particular processing element.  Certainly yes, but does that mean it needs
more than one pre-condition?

------------------------------------------------------------------------
Logical protocol graph should be made up of just graph nodes and their
pre-conditions and post-conditions.

ANSWER:

------------------------------------------------------------------------
Physical resource graph (eg: NIC dataflow) should be hard-wired.

ANSWER:
------------------------------------------------------------------------
Find out a way to generate different possible logical protocol graphs from
given pre and post conditions.

ANSWER:
------------------------------------------------------------------------
Use some kind of subgraph mapping algorithm to match one of the logical
protocol graph onto physical graph.

ANSWER:
------------------------------------------------------------------------
There are following elements:

 * Event: Input to the system.  May come with some new data
 * Action: Output of the system.  May produce some data
 * Computation: Processes some data, and tells result
 * Condition: Based on the data, tells true or false
 * Decision: Based on input data, chooses one of next step decision
  - Is pre-conditions, post-condition part of decision?

------------------------------------------------------------------------
Question: How to connect elements into graph?
Answer:

------------------------------------------------------------------------
Problem seems too complex when I see it in full details.  So, I am planning
to simplify it by making some assumptions.

I will be ignoring the impact of configuration space.  I am going to assume
that card works in one configuration for time being.

I am also planning to ignore the differences introduced by different hardware
implementation.

For time being, my objective is to map given logical network stack on top of
hardware.

Where exactly the alternatives will come from?

They come from the possibility that I may not use all of hardware features.
So, for that case, I need multiple graphs capturing every possible hardware
configuration combination.

In current case where I have one hardware configuration and one logical graph,
what I would end up with is one physical allocation plan.  This would also
involve expanding logical protocol graph with more details.

------------------------------------------------------------------------
Condition compare is not working properly right now.
What I need is a way to compare conditions and if condition is "Or" then
as long as one of them matches then it should be fine

------------------------------------------------------------------------
Had a discussion with Kornilios about the problem of comparing Conditions.
He gave an idea of using functions for pre and post conditions instead
of current complex expression.  Precondition will take current
status(or tree) and return True/False.
And Postcondition will take current status(or tree) and return new status.

------------------------------------------------------------------------
As per him, he localized the problem of expression matching.  I still
don't know precisely that what exactly went through his mind to lead to
this conclusion.  I should ask him this question and talk about how he
approaches these problems.

------------------------------------------------------------------------
Done with rewriting data-structures, now I have to rewrite Elements.hs
to reflect the changes

------------------------------------------------------------------------
TODO:
    --> Rewrite Elements.hs to reflect new data-structure
    --> Implement the algorithm to generate all possible combinations
    -->
------------------------------------------------------------------------
My current problem is that even though modules are in reasonable order
they also include modules which should not be there. For example, having
module TCP before ICMP does not make any sense.  I can solve this problem
by:
 * Adding a condition function with more strict search
 * Adding list of modules which must not be in action (eg: both IPv4 and IPv6
    should not be present in same processing path)
 * Making a tree and not a list in Nodes.

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
