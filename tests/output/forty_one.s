section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push rbp
  mov rbp, rsp
  mov eax, 41
  pop  rbp
  ret
