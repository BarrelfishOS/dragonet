

next question
---------------------
Qus:
Ans:



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

