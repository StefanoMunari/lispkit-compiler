# The SECD Machine
An abstract machine that takes a compiled function of a
Lispkit program (machine-language program) and its arguments,
executes the machine language program to compute the result of applying
that function to its arguments  

The name SECD is derived from the designation of its 4 registers:  
* **s - stack**: used to hold intermediate result when computing the values of
expressions  
* **e - enviroment**: used to hold variables bound to variables during
evaluation  
* **c - control list**: used to hold machine-language program being executed  
* **d - dump**: used as a stack to save values of other register when calling
a new function  

The entire state of the SECD machine can be denoted by giving the content of
its four registers, thus each instruction can be described as a transition
between two states of the SECD machine.  

The execution of the program, which instructions are stored in the control list,
terminates when the instruction STOP is encountered.  

# Set of machine instructions
* Add  : sum   
* Sub  : subtraction   
* Mult : multiplication    
* Div  : division  
* Eq   : equal  
* Leq  : less than equal  
* Car  : returns the first element of a list  
* Cdr  : returns a list without the first element  
* Cons : constructs an object with two values    
* Atom : returns true if the operand is not a cons cell   
* Sel  : select subcontrol   
* Join : join two branches to the main control (used with IF THEN ELSE)     
* Push : create a "place card" OGA for recursive functions     
* Ap   : apply function    
* Rap  : recursive apply     
* Ld   : load   
* Ldc  : load constant   
* Ldf  : load function  
* Rtn  : return   
* Stop : denotes the end of the program  

# References
[Functional Programming - Application and Implementation](http://www.amazon.com/Functional-Programming-Application-Implementation-Henderson/dp/0133315797)
