


next question
---------------------
Qus: What does it means by satisfying dependencies?
Ans:
two nodes are same
Status: NOT_DONE

next question
---------------------
Qus:  Assumption
Ans:
 - There will be atleast one node in PRG and it will be the starting node of
   LPG.

Status: NOT_DONE

next question
---------------------
Qus: What happens when you have more than one previous nodes?
Ans: Example : L3Classify has L3IPv4Classify  L3IPv6Classify  as previous nodes.

 - Are all of them in HW?
    -> yes -> Then you are good.  Put this one in h/w as well
    -> No
        -> is this AND node?
            -> Yes: implement it in S/W with all the previous nodes which
            are in software as software dependencies
        -> is this OR node?
            -> Yes: Create separate flows for hardware and software.
                Duplicate the node in both software and hardware.
                for hardware node, put it in hardware path
                and software path should take care of the case which is not
                handled by hardware


Status: NOT_DONE

next question
---------------------
Qus: Howto detect the previous node?
Ans:

Status: NOT_DONE




next question
---------------------
Qus:  Algorithm to check if node can be mapped onto h/w
Ans: is previous node in hardware?
 - Yes:
    - Can this node go in H/W based on embedding test?
        - Yes
           - Add it in hardware and follow the dependency from PRG
        - Nope
           - Add this as a first node in Software after the ``copy to queue``
                with ``copy to queue`` as dependency
 - Nope:
    Append behind the previous node which is in the software and follow the
        LPG dependency

Status: NOT_DONE



next question
---------------------
Qus:  Figure out a way to separate virtual nodes from computation nodes
    and also maybe classification nodes
Ans: Virtual nodes work only as a tag, and they do not represent any compuation
as such.  So, in theory they can't be present in hardware.  So, if I know
which one are virtual node, then I can treat them specially in my embedding
algorithm.

Status: NOT_DONE



next question
---------------------
Qus:  Why would any LPG node be mapped onto PRG node?
Ans: It can be one of the following reasons :
 * Not present in H/W (take care of virtual nodes)
 * Node is present, but dependencies are not met


Status: NOT_DONE


next question
---------------------
Qus:  Mapping LPG node into PRG node
Ans: For given LPG node, is there corrusponding PRG node?
    if yes, then check if it satisfies all the dependencies of LPG node.
        if yes, it can be mapped into PRG
    if No, emulate it into the software

Status: NOT_DONE


next question
---------------------
Qus:  Formal definition of embedding
Ans: Graph embedding means :
 * For every node in LPG, try to map it onto the PRG node.

Status: NOT_DONE


next question
---------------------
Qus:  Write down the tentative algorithm for embedding
Ans:

Status: NOT_DONE




next question
---------------------
Qus: Implement embedding of sample LPG and E1k
Ans: I have managed to implement very simple embedding where I highlight
nodes which are in both graphs and either merge edges or only show
once from large graph.  This looks somewhat sensible, but this solution
does not take into consideration the dependencies between nodes.

Status: NOT_DONE


next question
---------------------
Qus: Figure out a way to make LPG, Ek1 as modules while generating
    associated diagram.
Ans: This essentially means that I can't treat these as separate application.
This is specially needed so that I can do the embedding.  So, the current
plan is to create a single application which will write separate dot file
for every graph.

Status: DONE


next question
---------------------
Qus: Create Socket abstraction
Ans: Added another datatype  which can also work as computation

Status: DONE



next question
---------------------
Qus: Create application abstraction
Ans: Added another datatype  which can also work as computation. This
may not be the best way.
Status: DONE

Next question
---------------------
Qus:  Get simple LPG implemented over NetworkGraph
Ans: I am able to support Socket and Bind API right now.  Connect will
be a special case of bind, and I may have to work more for close.

Status: DONE

Next question
---------------------
Qus:  How am I going to deal with filters and queues?
Ans: I have managed to add them into the Computation Datatype.
Status: DONE

Next question
---------------------
Qus:  Need a code to get a subgraph from full graph which will include all
the computations needed to reach particular computation/tag/decision
Ans: This is useful to have feature, specially when **NetworkProcessing**
graph is becoming too big.

Status: NOT_DONE


Next question
---------------------
Qus:  Show ``queue 0`` as a default queue, and use different shape for
queues in the graph printing.
Ans: Fixed the issue of marking ``queue 0`` as default.  For giving proper
shape, I need to introduce another class instance **ToVertex** which will
give the properties of the vertex (eg: color shape).  The problem here is that
I am worried how slow will it become (as it became slow when I tried to add
custom Show instance to Computations datatype)

Status: NOT_DONE


Next question
---------------------
Qus:  Problem: defining Show instance for Computation makes whole execution
extremely slow.  I don't know why, and I don't know how to fix it.
Ans: Even if I provide most stupid implementation of Show, it is still
ridiculously slow.  And I don't know why.

Status: Unresolved

Next question
---------------------
Qus: How to add parameterized support for computations.
Ans: This is needed to add support for filters and queues.  I have managed
to add this support.  The tricky part here is howto print these parameterized
datatypes.

Status: DONE

Next question
---------------------
Qus: translate e1k PRG
Ans: Partially done.  e1k PRG is there but without queues.  Currently the
last thing it does is to copy to kernel memory.  What I need is queue support
(which means parameterized computations)
Status: Done


Next question
---------------------
Qus: How to rewrite existing PRG by using new language
Ans: I am directly using a way to describe the graph by specifying all the
dependencies (edges).

Status: Done


Next question
---------------------
Qus: Make sure the verification nodes are clearly marked as **AND** nodes.
Ans:  This is needed to avoid confusion between AND and OR nodes.
The main fact is that, all the inputs for AND nodes needs to be executed,
but the order does not matter.  But in case of OR nodes, the tag is valid as
long as any input is active.

I plan to do this by distinguishing AND and OR nodes with different node boxes.
This also means that I need to store the information about every node
specifying whether it is AND or OR node.  It is already captured in the
data-structure (all computations in same list are AND, and lists associated
with same TAG are OR), but I need to push it into the node-list as well.

Status: Done

Next question
---------------------
Qus:  Create a data-structure for a graph with overloaded datatype
Ans: Done
Status: Done

Next question
---------------------
Qus: Create a graph of all dependencies in for basic computations.
Ans:  It seems that even basic computations have dependencies.  I tried to
capture these dependencies using list, but it became too complicated and error
prone.  I need a data-structure which is natural graph, and can work with
overloaded datatypes



Next question
---------------------
Qus: Create a graph of all dependencies of pre and post conditions
Ans: If I have all dependencies in the dependency-List then these can be
used for pre and post conditions.

Next question
---------------------
Qus: Where exactly the pre and post conditions be?
Ans: Should they be on Conditions? or should they be on Modules?

example:
L4 tests depend on which L3 it is. As L3 will change where the L4 header starts.
You need to verify that it is TCP before making any tests
You need to verify that it is IPv4 or IPv6 before testing any of later tests.

Next question
---------------------
Qus: Create a graph out of it.
Ans:
Problem: can't use Data.Graph because I don't know howto override defination
of Vertex.  Also, it is based on quite old paper (1994), so going with
web-tutorial.

It seems that the web-tutorial code works just by creating a representative
graph where vertices are integers.  You can map these integers to some
different datatypes to get graph of some other types, but these types will
not be embedded into the graph.

So, I have two options :
 * Create a recursive data-type and use it to generate graph.
 * Create list of nodes and the graph will be overlay in the index based
   adjecency  list

Answer depends on what you want to do with these graphs later.
 * Find different paths
 * Find all nodes


Next question
---------------------
Qus: Can I make module dataType able to work with Ix?
Ans: It might work as long as this is graph and there are no expectations like
it needs to be sorted or anything like that.


Next question
---------------------
Qus: Read about how TCP segmentation works
Ans:

Next question
---------------------
Qus: Figure out how to introduce filter rule which will enable modelling
each and every flow.
Ans: idea: some sort of agregation of every flow selection.





Next question
---------------------
Qus: Can I use readymade graph libraries for my graphs?
Ans: There is Data.Graph library but it has a requirement on Vertex dataType
is that it is indexable. Which means that it should be able to answer
questions like index, range, in_range.
http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Data-Ix.html

I can try and do this by adding an integer in the structure as integer,
but I am not able to comprehend it's implications


Next question
---------------------
Qus: Support for heterogeneous list/collections
Ans: Yes!
http://www.haskell.org/haskellwiki/Heterogenous_collections

Next question
---------------------
Qus:  Write Module Datatype in details
Ans:


Next question
---------------------
Qus: Define conditions as basic operations.
Ans: Done, now working on Module data-structure which will encapsulate
few basic tests, and will give some name to it.


Next question
---------------------
Qus: How should I define the conditions?
Ans: What I want is that every test is different datatype.  But in that case,
I won't be able to get a list of tests.
So, I want a way to make a list of all the possible tests.  This can be done
by having one gigantic datatype which contains all the tests.

Every protocol module will have few of these tests

Should test be a composite dataType of elementory datatype?  If it is composite
then it can hold information like :
 + how many outcomes it can have.
 + Which protocol layer it is in.
 - We need to decide which are the bottom layer datatypes
 - there will be reduandant information as same information will be available
   in above graphs as well.
If we make it elementory datatype, then
 - Not all conditions are binary yes/no type
 - example: is this flow (TCP src_ip, dst_ip, src_port, dst_port)?

Next question
---------------------
Qus: What is the current state of V4?  where was I stuck?
Ans: The approach used in V4 was packet based, which is limiting.
So, I am abandoning it for condition based design and rewriting the code.


Next question
---------------------
Qus: Check if Data.Typeable works for you or not.
Ans: Yes, it works for me.  I had to enable some language extensions,
but it worked after that.

Next question
---------------------
Qus: Is there any way to look inside the datatype to find out the subtypes
involved in the datatype.
Ans: syb can travese any generic instance of data-type recursively and apply
given function to each of the element making sure that function works only
on specific types of elements.  This is essentially a way to reduce writing
boiler code, but it does not give any extended capability.  This method
needs a concrete object and not just dataype.

So the current answer is: Nope as per my current understanding.

Next question
---------------------
Qus: Can I find out the name of function?
Ans: Nope, you can't get the name of the function that easily.
Template Haskell might have some trick, but I am not sure if it will be worth
to explore it.


Next question
---------------------
Qus: Can I find out name of type?
Ans: Yes I can. It seems that I will have to use **Template Haskell** or use

http://stackoverflow.com/questions/5354431/put-in-string-of-type-name-in-haskell

##########################
Further Reading:
##########################

 * Template Haskell
 * scrap your boilerplate
   http://www.haskell.org/haskellwiki/Scrap_your_boilerplate
   http://www.cs.uu.nl/wiki/bin/view/GenericProgramming/SYB


##########################

Next question
---------------------

