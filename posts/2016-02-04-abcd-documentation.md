----
title: abcd-esolang Documentation
summary: Documentation for my esolang written for Chaos'16
----

abcd is an esoteric language. In other words, it is a language deliberately made obfuscated and confusing.
Trivial tasks too might be quite tough to accomplish while programming in this language. That is the point of the competition.

In Chaos '16, you will be attempting to write computer programs to accomplish certain tasks using this language. These tasks would range from simple ones,
to some really tricky ones.

# Description
Cutting to the chase, **abcd** is based off another similar language which had some shortcomings, and needed some fixes to be able to work properly.

Programming in this language requires you to manage values, memory and execution control flow effectively between a very restricted amount of variables you have.
The execution is to a good extent very similar to how computer programs are run by CPUs in computers.

You will have 53 instructions which will give the program instructions. These will be the alphabets *a-z*, *A-Z*, and the character *?*. So your program will basically look like a long gibberish string of alphabets.
They will manipulate 3 regular memory values (integers), and one which is basically a tuple of 2 integers. We call these integer values as *Registers*, as is done for CPUs.

In addition, you will have an array of integers of size 1024 which can be used to store values outside the registers.

You can only do operations on the values inside registers. You will be able to retrieve values from the memory and put it inside the registers, and vice-verse.

### Register1 and Register2
These are general purpose registers used to provide arguments to operations. For example if you need to add 2 integers, you will need to store them in these registers.

### Register3 - The result register
This is the result register. This will store the result of instructions if any. For example in the above mentioned sum exercise, the result would be stored in this register.

The two registers available as a tuple are meant for specialized operations. They are modelled as follows:
Register4[2], where Register4[0] is the memory pointer, and Register4[1] is the position pointer.

### The memory pointer
One of them lets you write integer values to memory locations, and also to retrieve values from it. You can store intermediate results in the memory using this.
For example, if this register stores the value 0 right now and you call the instruction to write the value of Register3 to memory, it will write that value to Memory[0].

### The position pointer
The other lets you jump to the position in the program corresponding to the integer stored in it.
For example, if that register stores 0, you can jump to the 0th location of the program, that is, to the beginning. This is useful for looping statements.


You will also have a *?* instruction which will let you stop the program if the complete input has been read. This will be useful when you need to read an arbitrary amount of input.

There will be a Mode bit. This will be either 0 or 1. You have instructions to add or subtract some values to Register4[Mode].
This will come in handy if you need to modify the memory or the position pointer.

# Sample programs

### Hello World!
```
cccCISccccCIScccCIYx
SGSHaaCLgDLihhhDLDLgggDL
TTGaaCL
SGccbbbCLDLgggDLjggggDLSHDL
TTGaaaCL
```

### Print the character 'b'
```
ebbCL
```

### Print the number 98
```
ebbCM
```

# The implementation and the instructions

Here's the javascript code of the interpreter for more clarity.
Note: All these cases are separated by 'break;' statements in the actual code.
```
    case "a":
      Register1 += 1;
      
    case "b":
      Register1 -= 1;
      
    case "c":
      Register1 += 10;
      
    case "d":
      Register1 -= 10;
      
    case "e":
      Register1 += 100;

    case "f":
      Register1 -= 100;
      
    case "g":
      Register2 += 1;
      
    case "h":
      Register2 -= 1;
      
    case "i":
      Register2 += 10;
      
    case "j":
      Register2 -= 10;
      
    case "k":
      Register2 += 100;
      
    case "l":
      Register2 -= 100;
      
    case "m":
      Register3 = !Register1;
      
    case "n":
      Register3 = !Register2;
      
    case "o":
      Register3 = Register1 & Register2;
      
    case "p":
      Register3 = Register1 | Register2;
      
    case "q":
      Register3 = Register1 ^ Register2;
      
    case "r":
      Register3 = Register1 + Register2;
      
    case "s":
      Register3 = Register1 - Register2;
      
    case "t":
      Register3 = Register1 * Register2;
      
    case "u":
      Register3 = Register1 / Register2;
      
    case "v":
      Register3 = Register1 % Register2;
      
    case "w":
      Register3 = Register1 ^ Register2;
      
    case "x":
      Register1 = 0;
      
    case "y":
      Register2 = 0;
      
    case "z":
      Register3 = 0;
      
    case "A":
      Register2 = Register1;
      
    case "B":
      Register1 = Register2;
      
    case "C":
      Register3 = Register1;
      
    case "D":
      Register3 = Register2;
      
    case "E":
      Register1 = Register3;
      
    case "F":
      Register2 = Register3;
      
    case "G":
      Register1 = Memory[Register4[0]];
      
    case "H":
      Register2 = Memory[Register4[0]];
      
    case "I":
      Memory[Register4[0]] = Register3;
      
    case "J":
      Read the next character from input.
      Store it in register1
      
    case "K":
      Read next character from input.
      Store it in register2
      
    case "L":
      Basically printing the value of Register3 as a character.
      So 97 would print as 'a'.
      
    case "M":
      Printing value of Register3 as an integer.
      97 would print '97'
      
    case "N":
      if(Register1 === Register2) Position = Register4[1]-1;
      
    case "O":
      if(Register1 !== Register2) Position = Register4[1]-1;
      
    case "P":
      if(Register1 >= Register2) Position = Register4[1]-1;
      
    case "Q":
      if(Register1 <= Register2) Position = Register4[1]-1;
      
    case "R":
      if(Register3) Position = Register4[1]-1;
      
    case "S":
      Register4[Mode] += 1;
      
    case "T":
      Register4[Mode] -= 1;
      
    case "U":
      Register4[Mode] += 10;
      
    case "V":
      Register4[Mode] -= 10;
      
    case "W":
      Register4[Mode] += 100;
      
    case "X":
      Register4[Mode] -= 100;
      
    case "Y":
      Register4[Mode] = 0;
      
    case "Z":
      Mode = (Mode+1)%2;
      
    case "?":
      if(readInput >= input.length + 1)
        finished=1;

```

# Tips

* Characters which are not recognized are ignored. Use this to keep your code readable. You can use spaces and newline to have that effect.

# ASCII Table
You'll **NEED** this:

| DEC | Symbol |              Description               |
|-----|--------|----------------------------------------|
|  32 |        | Space                                  |
|  33 | !      | Exclamation mark                       |
|  34 | "      | Double quotes (or speech marks)        |
|  35 | #      | Number                                 |
|  36 | $      | Dollar                                 |
|  37 | %      | Procenttecken                          |
|  38 | &      | Ampersand                              |
|  39 | '      | Single quote                           |
|  40 | (      | Open parenthesis (or open bracket)     |
|  41 | )      | Close parenthesis (or close bracket)   |
|  42 | *      | Asterisk                               |
|  43 | +      | Plus                                   |
|  44 | ,      | Comma                                  |
|  45 | -      | Hyphen                                 |
|  46 | .      | Period, dot or full stop               |
|  47 | /      | Slash or divide                        |
|  48 | 0      | Zero                                   |
|  49 | 1      | One                                    |
|  50 | 2      | Two                                    |
|  51 | 3      | Three                                  |
|  52 | 4      | Four                                   |
|  53 | 5      | Five                                   |
|  54 | 6      | Six                                    |
|  55 | 7      | Seven                                  |
|  56 | 8      | Eight                                  |
|  57 | 9      | Nine                                   |
|  58 | :      | Colon                                  |
|  59 | ;      | Semicolon                              |
|  60 | <      | Less than (or open angled bracket)     |
|  61 | =      | Equals                                 |
|  62 | >      | Greater than (or close angled bracket) |
|  63 | ?      | Question mark                          |
|  64 | @      | At symbol                              |
|  65 | A      | Uppercase A                            |
|  66 | B      | Uppercase B                            |
|  67 | C      | Uppercase C                            |
|  68 | D      | Uppercase D                            |
|  69 | E      | Uppercase E                            |
|  70 | F      | Uppercase F                            |
|  71 | G      | Uppercase G                            |
|  72 | H      | Uppercase H                            |
|  73 | I      | Uppercase I                            |
|  74 | J      | Uppercase J                            |
|  75 | K      | Uppercase K                            |
|  76 | L      | Uppercase L                            |
|  77 | M      | Uppercase M                            |
|  78 | N      | Uppercase N                            |
|  79 | O      | Uppercase O                            |
|  80 | P      | Uppercase P                            |
|  81 | Q      | Uppercase Q                            |
|  82 | R      | Uppercase R                            |
|  83 | S      | Uppercase S                            |
|  84 | T      | Uppercase T                            |
|  85 | U      | Uppercase U                            |
|  86 | V      | Uppercase V                            |
|  87 | W      | Uppercase W                            |
|  88 | X      | Uppercase X                            |
|  89 | Y      | Uppercase Y                            |
|  90 | Z      | Uppercase Z                            |
|  91 | [      | Opening bracket                        |
|  92 | \      | Backslash                              |
|  93 | ]      | Closing bracket                        |
|  94 | ^      | Caret - circumflex                     |
|  95 | _      | Underscore                             |
|  96 | `      | Grave accent                           |
|  97 | a      | Lowercase a                            |
|  98 | b      | Lowercase b                            |
|  99 | c      | Lowercase c                            |
| 100 | d      | Lowercase d                            |
| 101 | e      | Lowercase e                            |
| 102 | f      | Lowercase f                            |
| 103 | g      | Lowercase g                            |
| 104 | h      | Lowercase h                            |
| 105 | i      | Lowercase i                            |
| 106 | j      | Lowercase j                            |
| 107 | k      | Lowercase k                            |
| 108 | l      | Lowercase l                            |
| 109 | m      | Lowercase m                            |
| 110 | n      | Lowercase n                            |
| 111 | o      | Lowercase o                            |
| 112 | p      | Lowercase p                            |
| 113 | q      | Lowercase q                            |
| 114 | r      | Lowercase r                            |
| 115 | s      | Lowercase s                            |
| 116 | t      | Lowercase t                            |
| 117 | u      | Lowercase u                            |
| 118 | v      | Lowercase v                            |
| 119 | w      | Lowercase w                            |
| 120 | x      | Lowercase x                            |
| 121 | y      | Lowercase y                            |
| 122 | z      | Lowercase z                            |
| 123 | {      | Opening brace                          |
| 124 | |      | Vertical bar                           |
| 125 | }      | Closing brace                          |
| 126 | ~      | Equivalency sign - tilde               |
| 127 |        | Delete                                 |

