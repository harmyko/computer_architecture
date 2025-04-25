# Computer Architecture - Vilnius University

This repository contains my work for the 1st semester module "Computer Architecture" at Vilnius University, graded by Vytautas Valaitis.

## lab_1: Octal to Decimal Converter

This program converts an octal number (within the range [0; 177777]) into its decimal form. It checks if the input number is valid, verifies that each digit is within the octal range (0-7), and then performs the conversion. The result is displayed in decimal format.

### Features:
- The program uses interrupts and handles errors for invalid input.
- It takes user input, validates it, converts the number, and displays the result.

## lab_2: File Comparison Program

This program compares the contents of two files. It takes the names of two files as input parameters and checks if they are identical or not. If there are differences, it records the positions of the differing bytes in a result file.

### Features:
- The program handles file opening, reading, and writing.
- It compares the files byte by byte.
- It stores the differing byte positions in a new file (`rez.txt`).
- If an error occurs during file operations, an error message is displayed.

### Steps:
1. The program accepts the names of two files.
2. It reads the contents of the files into buffers.
3. It compares the files byte by byte.
4. The differing byte positions are written into the `rez.txt` file.

# lab_3_1: POP reg Disassembly

## Overview
This lab simulates the `POP reg` instruction and outputs operation data each time it is called.

# lab_3_2: POP mem Disassembly

## Overview
This lab simulates the `POP mem` instruction and outputs operation data each time it is called.

# lab_3_3: PUSH reg Disassembly

## Overview
This lab simulates the `PUSH reg` instruction and outputs operation data each time it is called.

# lab_3_4 POP reg Disassembly

## Overview
This lab (also) simulates the `POP reg` instruction and outputs operation data each time it is called.

# lab_3_5 reg MOV reg Disassembly

## Overview
This lab simulates the `reg MOV reg` instruction and outputs operation data each time it is called.






