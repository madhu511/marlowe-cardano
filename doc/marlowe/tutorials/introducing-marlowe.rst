.. _introducing-marlowe:

Introducing Marlowe
===================

This tutorial gives an overview of the ideas behind Marlowe, as a
domain-specific language embedded in Haskell. It also introduces
commitments and timeouts, which are central to how Marlowe works in a
blockchain context.

Programming Languages and Domain-Specific Languages
---------------------------------------------------

The first computers were programmed in “machine code”. Each kind of
system had a different code, and these codes were low-level and
inexpressive: programs were long sequences of very simple instructions,
incomprehensible to anyone who had not written them. Nowadays we are
able to use higher-level languages like C, Java and Haskell to program
systems. The same languages can be used on widely different machines,
and the structure of the programs reflects what they do. On blockchain,
their equivalents are languages like Plutus, Solidity and Simplicity.
These higher-level languages are general purpose – they can be used to
solve all sorts of different problems – but the solutions they express
are still programs, and they still require programming skills to use
them effectively.

In contrast, Marlowe is a special purpose or *domain-specific language* (DSL) that is
designed to be usable by someone who is expert in a particular field,
rather than requiring programming skills to use it. In the case of
Marlowe, the domain is the field of financial contracts.

Using a DSL has many advantages beyond its use by non-programmers:

-  We can ensure that certain sorts of bad programs cannot even be
   written, by designing those possibilities out of the language. By
   doing this we can aim to avoid some of the unanticipated exploits
   which have been a problem for existing blockchains.

-  We can also more easily check that programs have the properties that
   we want: for example, in the case of a Marlowe contract, we can make
   sure that the contract will never fail to make a payment that it
   should.

-  Because it is a DSL, we can build special-purpose tools to help
   people write programs in the language. In the case of Marlowe we can
   emulate how a contract will behave before it is run for real on the
   blockchain; this helps us to make sure that the contract we have
   written is doing what it is intended to.

Marlowe is also an *embedded* DSL, hosted in the
`Haskell <https://www.haskell.org>`_ programming language. While it is
possible to use “pure” Marlowe if we wish, being embedded in a
general-purpose language allows contract writers to selectively exploit
features of Haskell in writing Marlowe contracts, making them easier to
read and supporting re-use. In fact, Marlowe is not tied to Haskell, and
we have also developed a JavaScript environment for Marlowe. The Marlowe Playground, the online tool
to help you build Marlowe contracts, supports both, as well as a visual way of writing Marlowe.


Marlowe in a nutshell
---------------------

Marlowe is modelled on special-purpose financial contract languages popularised in the last
decade or so by academics and enterprises such as LexiFi, which provides
contract software in the financial sector. In developing Marlowe, we
have adapted these languages to work on blockchain. Marlowe is
implemented on the Cardano blockchain, but could equally well be
implemented on Ethereum or other blockchain platforms; in this respect
it is “platform agnostic” just like modern programming languages such as
Java and C++. The Marlowe Playground online simulation allows you to
experiment with, develop, simulate and analyse Marlowe contracts in your
web browser, without having to install any software. Marlowe Run is the client application 
that allows you to run Marlowe contracts on chain: it is available as a prototype 
and will presently be running "live" on the Cardano blockchain itself.

What does a Marlowe contract look like? It is built by combining a small
number of building blocks that describe making a payment, making an
observation of something in the “real world”, waiting until a certain
condition becomes true, and so on.

Timeouts, deposits and commitments
----------------------------------

Where we differ from non-blockchain approaches is in how we make sure
that the contract is followed. This means not only that the instructions
of the contract are not disobeyed – *“nothing bad happens”* – but also
that the participants participate and don’t walk away early, leaving
money locked up in the contract forever: “\ *good things actually
happen*\ ”. We do this using **timeouts**.

A contract can ask a participant to make a **deposit** of some funds,
but obviously the contract cannot actually force a participant to make a
deposit. Instead, the contract can wait for a period of time for the
participant to commit to the contract: when that period of time expires,
the contract moves on to follow some alternative instructions. This
prevents a participant stopping a contract by not taking part, thus
making sure that “things happen”.

All the constructs of Marlowe that require user participation –
including user deposits and user choices – are protected by timeouts.
Because of this, it is easy to see that the **commitment** made by a
participant to a contract is *finite*: we can predict when the contract
will have nothing left to do – when it can be closed. At this point any
unspent funds left in the contract are **refunded** to participants, and
the contract stops, or *terminates*. So, any funds put into the contract
by a participant *can’t be locked up forever*: at this point the
commitment effectively ends.

What is more, it is easy for us to *read off* from the contract when it
will terminate, we call this the *lifetime* of the contract: all
participants will therefore be able to find out this lifetime before
taking part in any contract,

In our model, a running contract cannot force a deposit or a choice to
happen: all it can do is to request a deposit or choice from a
participant. In other words, for these actions it cannot “\ *push*\ ”,
but it can “\ *pull*\ ”. On the other hand, it *can* make payments
automatically, so some aspects of a Marlowe contract can “push” to make
some things happen, e.g. ensuring that a payment is made to a
participant by constructing an appropriate transaction output.

Marlowe in action
-----------------

We are working on a production release of Marlowe on the Cardano
blockchain early in 2021. From today, you are able to explore Marlowe
for yourself, either by downloading it and using the Haskell
implementation directly, or by using the online Marlowe Playground
simulation tool; these are both covered in subsequent tutorials. These
will also cover the details of Marlowe, introduce a series of examples,
look deeper into the tools for Marlowe.

We have also worked on developing a set of templates for popular
financial instruments taken from the Actus standard, and are able to
generate particular contracts from these templates according to the
various parameters and options that can be set.

Because Marlowe is a DSL we can work out how Marlowe contracts will
behave without running them: this means that we can provide valuable
diagnostics to potential participants before they commit to a contract,
using *static analysis*. We can also use logic tools to *formally prove
properties* of Marlowe contracts, giving users the highest level of
assurance that their contracts behave as intended.

Research-based
--------------

Marlowe is based on original, peer reviewed, research conducted by the 
Marlowe team, initially at the University of Kent supported by a research grant 
from IOHK, and latterly as an internal engineering team in the company. We are also
working jointly with Wyoming Advanced Blockchain R&D Laboratory (WABL) at the University of Wyoming.
If you are interested in working with us, please get in touch.


Our research work is reported in these published papers.

-  `Marlowe: financial contracts on
   blockchain <https://iohk.io/en/research/library/papers/marlowefinancial-contracts-on-blockchain/>`_
   The paper that introduced the Marlowe language. It is an earlier version, but 
   nevertheless it explains the principles and rationale behind its
   design and implementation.

-  `Marlowe: implementing and analysing financial contracts on
   blockchain <https://iohk.io/en/research/library/papers/marloweimplementing-and-analysing-financial-contracts-on-blockchain/>`_
   This paper describes the implementation of Marlowe on the Cardano
   blockchain, and the analysis supported by the Marlowe Playground
   web-based development and simulation environment.

-  `Efficient static analysis of Marlowe
   contracts <https://iohk.io/en/research/library/papers/efficient-static-analysis-of-marlowe-contracts/>`_
   This paper explains how we optimised the static analysis explained in
   the previous paper.

-  `Standardized crypto-loans on the Cardano blockchain <https://iohk.io/en/research/library/papers/standardized-crypto-loans-on-the-cardano-blockchain/>`_
   In this paper we explore a smart contract framework for building standardized crypto-loans using Marlowe, with the ACTUS standard at its core.

and in this eprints survey paper.

- `Scripting smart contracts for distributed ledger 
  technology <https://iohk.io/en/research/library/papers/scripting-smart-contracts-for-distributed-ledger-technology/>`_
  Here we give an overview of the scripting languages used in existing cryptocurrencies.

Finding out more
----------------

Systems 

-  `The Marlowe Playground <https://play.marlowe-finance.io>`_ an
   in-browser development, analysis and simulation environment.

-  `Marlowe Run <https://run.marlowe-finance.io>`_ the end-user client for downloading and running
   Marlowe contracts on the Cardano blockchain; currently in prototype.

-  `The Marlowe github
   repository <https://github.com/input-output-hk/marlowe>`_ from which
   you can download Marlowe.

-  `The Marlowe Website <https://marlowe-finance.io>`_ landing page for all things Marlowe.


Videos

-  `YouTube playlist: Marlowe: financial contracts on
   blockchain <https://www.youtube.com/playlist?list=PLqu19-ygE4ofUgGpslOs5zCr9Z6zCMibq>`_.
   A general introduction to Marlowe from October 2020.
   Some features of the Playground have been updated since then.

