# Macquarie University, Department of Computing #

## COMP332 Programming Languages 2017 ##

## Assignment 2 ##

Due: 11am Monday 9 October 2017 (week 9)  
Worth: 15% of unit assessment

Marks breakdown:

* Code: 50% (of which tests are worth 10%)
* Report: 50% (of which test description is worth 10%)

Submit a notice of disruption via [Ask@MQ](https://ask.mq.edu.au) if you are unable to submit on time for medical or other legitimate reasons.

Late penalty without proper justification: 20% of the full marks for the assessment per day or part thereof late.

### Overview ###

This assignment asks you to develop a type analyser for the [Hipster](https://en.wikipedia.org/wiki/Hipster_(contemporary_subculture)) cellular automaton language. We will build on this in assignment 3 to complete a full implementation of this language.

Building this implementation will give you insight into how compilers go about checking that the code you write satisfies the type rules of the programming language you are writing in. Overall these assignments are designed to reveal the way that domain programming language implementations work in general, as well as to provide you with specific experience of how language programs are written, how they are compiled, and how they are executed.

This kind of task often arises in programming situations other than language implementation. For example, many game engines provide specialised languages designed to make the process of specifying game mechanics easier, less error prone and more intuitive. The game engine software development kit (SDK) must provide tools to reliably read scripts written in such languages, understand their structure, and check the types of data they contain. The techniques used to construct those tools are no different to those used to build compilers for general purpose programming languages (like C and Java) or domain specific languages like Hipster.

The Hipster programming language, its syntax, and its name and type analysis rules are described in detail in the README file which you can find, in markdown and HTML formats, in the root directory of the assignment 2 skeleton.

### What you have to do ###

You have to write, document and test a Scala type inferencer and checker for the Hipster language. 

The compiler skeleton you have been given already handles all environment plumbing necessary for name analysis, in the `NameAnalysis.scala` module, and it contains a complete error reporting framework, in the `SemanticAnalysis.scala` module. It also provides some attributes to support the type inference and checking process, in the `TypeAnalysis.scala` module, but this code is incomplete and it will be your job to fix that.

The heart of the Hipster type analyser is comprised of two attributes:

* `tipe`: which attributes each expression node with the inferred type of that node computed according to the type inference rules outlined above.

* `expTipe`: which attributes each expression node with the set of types that it may legally assume. This is computed on the basis of the context in which the expression is located.

Their incomplete definitions may be found in the `TypeAnalysis.scala` module, just look for the `FIXME` comments.

The section entitled **Type Analysis** in the Hipster specification `README` file provides a detailed description of its type inference and checking rules. These should translate almost directly into `case` clauses in the definitions of the `tipe` and `expTipe` attributes. We've also left a bunch of comments in the Hipster source and test files to guide you in this task.

The framework bundle also includes a pretty comprehensive test suite covering lots of other aspects of the Hipster semantic analyser. Eight of these tests currently fail when run against the compiler code in the framework bundle, since they ultimately tests aspects of the type analyser that aren't currently working. 

These tests make only a scant attempt at testing the type analyser, so even if your code makes those eight tests pass that doesn't mean that your type checker is working correctly. You will need to write plenty of tests of your own to check the correctness of your work, and you should put those tests in the `TypeAnalysisTests.scala` module (look for the `FIXME` comment).

You can complete this assignment by only making changes to the `TypeAnalysis.scala` and `TypeAnalysisTests.scala` modules. Of course, if you absolutely feel that you need to change code in other modules, for some reason, then there is nothing preventing from doing so. 

You are strongly advised not to try to solve the whole assignment in one go. It is best to write code to handle the parsing and tree construction for some simple constructs first and then build up to the full language.

Your code must use the Kiama attribute library as discussed in lectures and practicals. You should use the expression language semantic analyser from the mixed classes as a guide for your implementation.

A skeleton sbt project for the assignment has been provided on BitBucket in the [dominicverity/comp332-hipster](https://bitbucket.org/dominicverity/comp332-hipster) repository or its Mercurial based clone [dominicverity/comp332-hipster-hg](https://bitbucket.org/dominicverity/comp332-hipster-hg). The modules are very similar to those used in the practical exercises for Week 5 onwards. The skeleton contains the modules you will need. 

### What you must hand in and how ###

A zip file containing all of the code for your project and a type-written report.

Submit every source and build file that is needed to build your program from source, including files in the skeleton that you have not changed. Do not add any new files or include multiple versions of your files. Do not include any libraries or generated files (run the sbt `clean` command before you zip your project). We will compile all of the files that you submit using sbt, so you should avoid any other build mechanisms.

Your submission should include all of the tests that you have used to make sure that your program is working correctly. Note that just testing one or two simple cases is not enough for many marks. You should test as comprehensively as you can.

Your report should describe how you have achieved the goals of the assignment. Do not neglect the report since it is worth 50% of the marks for the assignment.

Your report should contain the following sections:

* A title page or heading that gives the assignment details, your name and student number.
* A brief introduction that summarises the aim of the assignment and the structure of the rest of the report.
* A description of the design and implementation work that you have done to achieve the goals of the assignment. Listing some code fragments may be useful to illustrate your description, but don't give a long listing. Leaving out obvious stuff is OK, as long as what you have done is clear. A good rule of thumb is to include enough detail to allow a fellow student to understand it if they are at the stage you were at when you started work on the assignment.
* A description of the testing that you carried out. You should demonstrate that you have used a properly representative set of test cases to be confident that you have covered all the bases. Include details of the tests that you used and the rationale behind why they were chosen. Do not just print the tests out without explanation.

Submit your code and report electronically as a single zip file called `ass2.zip` using the appropriate submission link on the COMP332 iLearn website by the due date and time. Your report should be in PDF format.

DO NOT SUBMIT YOUR ASSIGNMENT OR DOCUMENTATION IN ANY OTHER FORMAT THAN ZIP and PDF, RESPECTIVELY. Use of any other format slows down the marking and may result in a mark deduction.

### Marking ###

The assignment will be assessed according to the assessment standards for the unit learning outcomes.

Marks will be allocated equally to the code and to the report. Your code will be assessed for correctness and quality with respect to the assignment description. Marking of the report will assess the clarity and accuracy of your description and the adequacy of your testing. 20% of the marks for the assignment will be allocated to testing.

---
[Dominic Verity](http://orcid.org/0000-0002-4137-6982)  
Last modified: 11 September 2017  
[Copyright (c) 2017 by Dominic Verity. All rights reserved.](http://www.mq.edu.au/legalstuff.html)

