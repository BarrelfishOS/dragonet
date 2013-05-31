

next question
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

next question
---------------------
Qus:  Create a data-structure for a graph with overloaded datatype
Ans: Done


next question
---------------------
Qus: Create a graph of all dependencies in for basic computations.
Ans:  It seems that even basic computations have dependencies.  I tried to
capture these dependencies using list, but it became too complicated and error
prone.  I need a data-structure which is natural graph, and can work with
overloaded datatypes



next question
---------------------
Qus: Create a graph of all dependencies of pre and post conditions
Ans: If I have all dependencies in the dependency-List then these can be
used for pre and post conditions.

next question
---------------------
Qus: Where exactly the pre and post conditions be?
Ans: Should they be on Conditions? or should they be on Modules?

example:
L4 tests depend on which L3 it is. As L3 will change where the L4 header starts.
You need to verify that it is TCP before making any tests
You need to verify that it is IPv4 or IPv6 before testing any of later tests.

next question
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


next question
---------------------
Qus: Can I make module dataType able to work with Ix?
Ans: It might work as long as this is graph and there are no expectations like
it needs to be sorted or anything like that.


next question
---------------------
Qus: Read about how TCP segmentation works
Ans:

next question
---------------------
Qus: Figure out how to introduce filter rule which will enable modelling
each and every flow.
Ans: idea: some sort of agregation of every flow selection.





next question
---------------------
Qus: Can I use readymade graph libraries for my graphs?
Ans: There is Data.Graph library but it has a requirement on Vertex dataType
is that it is indexable. Which means that it should be able to answer
questions like index, range, in_range.
http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Data-Ix.html

I can try and do this by adding an integer in the structure as integer,
but I am not able to comprehend it's implications


next question
---------------------
Qus: Support for heterogeneous list/collections
Ans: Yes!
http://www.haskell.org/haskellwiki/Heterogenous_collections

next question
---------------------
Qus:  Write Module Datatype in details
Ans:


next question
---------------------
Qus: Define conditions as basic operations.
Ans: Done, now working on Module data-structure which will encapsulate
few basic tests, and will give some name to it.


next question
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

next question
---------------------
Qus: What is the current state of V4?  where was I stuck?
Ans: The approach used in V4 was packet based, which is limiting.
So, I am abandoning it for condition based design and rewriting the code.


next question
---------------------
Qus: Check if Data.Typeable works for you or not.
Ans: Yes, it works for me.  I had to enable some language extensions,
but it worked after that.

next question
---------------------
Qus: Is there any way to look inside the datatype to find out the subtypes
involved in the datatype.
Ans: syb can travese any generic instance of data-type recursively and apply
given function to each of the element making sure that function works only
on specific types of elements.  This is essentially a way to reduce writing
boiler code, but it does not give any extended capability.  This method
needs a concrete object and not just dataype.

So the current answer is: Nope as per my current understanding.

next question
---------------------
Qus: Can I find out the name of function?
Ans: Nope, you can't get the name of the function that easily.
Template Haskell might have some trick, but I am not sure if it will be worth
to explore it.


next question
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

next question
---------------------

