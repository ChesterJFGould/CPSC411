# Immediates
+ Earlier versions of this compiler had the ability to directly use immediates
  in instructions with an immediate limit of 32bits if the value was under 2^32.
  This however turns out to be unsound as the immediate will be sign extended.
  Therefore all immediates will be first assigned to a temporary register and
  then the temporary register will be used in place of the immediate.
# Error Codes
As previously specified but with the 0xF opcode standing for setvec
