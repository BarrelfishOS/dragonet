

=====================
Requirements
=====================

You are assumed to have following tools to be able to compile the code ::

 * The Glorious Glasgow Haskell Compilation System, version 7.4.2
 * ``dot`` - graphviz version 2.26.3 (20100126.1600) (needed to compile the code)


dot can be installed with following command ::

    sudo apt-get install graphviz

=====================
Compilation
=====================

The ``Makefile`` quite simple and should compile everything just by running
the ``make`` command.  As a result of running make,  ``pdf`` and ``png`` files
will be generated to show the graphs for every target in the Makefile.
Currently available targets are :

 * NetworkProcessing_run: Generates a graph of all computations and their
        dependencies.
 * E1k_run : This is PRG for E1k NIC (Intel 82576 1GbE)

----------
OUTPUT:
----------

 * NetworkProcessing.{png,pdf} : The graph capturing all the computation
        and dependencies between them.  This is generic and hardware
        independent graph.
 * E1k.{png,pdf} : The PRG for Intel E1K driver.

----------
NOTE:
----------
There will be few warning printed while compiling the code.  These warnings
are due to the unused functions which were written for debugging purposes.
So, ignore them for time being.  I haven't found a way to supress these
specific warnings while showing others.

=====================
Code Organization
=====================

Following are the important files and their roles in the code :

 * Computations.hs : Most important file.  It defines a datatype which
    encompasses all possible computations that can happen in typical
    TCP/IP based network processing (type Computation).  It also has
    ``getNetworkDependency`` function which returns dependencies between
    all different computations.  Currently, this the core of the "language".
 * MyGraph.hs : This file contains a code which implements a parameterized
    graph structure which can work with datatypes like ``Computation``.
 * E1k.hs : It contains the code to define the PRG for E1k NIC.
 * plan.rst : I use to document all the different questions faced,
    and how they were resolved (if atall resolved)
 * README.txt : This file. Also tries to follow ReStructured Text format for
    documenting.

