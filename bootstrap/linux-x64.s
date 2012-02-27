# Linux/x86-64 bytecode dynamic compiler | Spencer Tipping
# Licensed under the terms of the MIT source code license

# Introduction.
# This compiler translates Apostrophe core-level bytecode into machine instructions and gives you a way to execute them. It is implemented as a concatentative language over %rsp. The mapping
# from (debatably) human-readable ASCII bytecode to instructions is achieved by using code in a lookup table referenced by %rbp. This mapping encodes SDoc syntax by redirecting all capital
# letters to functions which detect two newlines. All input is read from file descriptor 0 (stdin).

# This interpreter is written in flat-style; that is, no function calls unless absolutely necessary. Some people would consider this to be "spaghetti code"; I wouldn't blame them for this
# interpretation.

.section .rodata
  mmap_error:      .ascii "error: could not allocate memory (mmap failed)\n"
  mmap_error_size: .quad . - mmap_error

  mmap_alignment_error:      .ascii "error: mmap failed to align memory to a 4096-byte boundary\n"
  mmap_alignment_error_size: .quad . - mmap_alignment_error

  start_sdoc_capitals: .byte 'A'
  end_sdoc_capitals:   .byte 'Z'
  sdoc_pipe:           .byte '|'
  comment_hash:        .byte '#'
  newline:             .byte '\n'

.text
  .globl _start
  .type _start, @function

  _start:
  jmp setup_dispatch_table
  setup_dispatch_table_cc:
  jmp setup_stdin_buffer
  setup_stdin_buffer_cc:

  movq $main_cc, %r12
  jmp stdin_loop

  main_cc:
  movq $141, %rdi                               # Sentinel exit
  jmp exit

  exit:
  movq $60, %rax
  syscall

  stderr:                                       # buf = %rsi, length = %rdx, continuation = %r12 -> result = %rax
  pushq %rdi
  movq $1, %rax                                 # syscall = write
  movq $2, %rdi                                 # fd      = 2 (stderr)
  syscall
  popq %rdi
  jmp *%r12

  allocate:                                     # size = %rsi, continuation = %r12 -> result = %rax (fails on error)
  pushq %rdi
  pushq %rdx
  pushq %r10
  pushq %r8
  pushq %r9
  movq $9, %rax                                 # syscall = mmap
  xorq %rdi, %rdi                               # *addr   = NULL
                                                # length  = %rsi
  movq $3, %rdx                                 # prot    = PROT_READ | PROT_WRITE
  movq $34, %r10                                # flags   = MAP_PRIVATE | MAP_ANONYMOUS
  movq $-1, %r8                                 # fd      = -1
  xorq %r9, %r9                                 # offset  = 0
  syscall
  popq %r9
  popq %r8
  popq %r10
  popq %rdx

  testq %rax, %rax                              # negative return values from mmap() indicate failure
  jns allocate_ok

  movq $mmap_error, %rsi
  movq mmap_error_size, %rdx
  negq %rax                                     # use errno as program return code
  movq %rax, %rdi
  movq $exit, %r12                              # no recovery; just exit immediately
  jmp stderr

  allocate_ok:
  popq %rdi
  jmp *%r12

# Dispatch table setup.
# The initial dispatch table is dynamically allocated using mmap and initialized from assembly code. Letters from A-Z and | are set up to consume input until a \n\n is reached. This allows you
# to write SDoc into the bytecode. The dispatch table is addressed using %rbp and is scaled by 8 bytes (2048 bytes per table).

  setup_dispatch_table:
  movq $4096, %rsi
  movq $setup_dispatch_table_mmap_ok, %r12
  jmp allocate

  setup_dispatch_table_mmap_ok:
  movq %rax, %rbp

  # Initializing the table.
#   Clear out the whole thing up front. By default, every character is a nop.

    leaq (%rbp), %rcx
    movq %rcx, %rbx
    addq $2048, %rbx

    movq $nop_character, %rdx

    setup_dispatch_table_clear_loop:
      movq %rdx, (%rcx)

    addq $8, %rcx
    testq %rcx, %rbx
    jb setup_dispatch_table_clear_loop

  # Writing dispatch instructions.
#   The dispatch table contains a bunch of jump offsets to functions defined here in assembly code.

    xorq %rax, %rax; movb start_sdoc_capitals, %al
    xorq %rbx, %rbx; movb end_sdoc_capitals,   %bl
    leaq (%rbp,%rax,8), %rcx
    leaq (%rbp,%rbx,8), %rbx
    movq $read_sdoc_paragraph, %rdx

    setup_dispatch_table_capital_letter_loop:
      movq %rdx, (%rcx)

    addq $8, %rcx
    testq %rcx, %rbx
    jb setup_dispatch_table_capital_letter_loop

    # Also setup the pipe and hash character mappings:
    xorq %rax, %rax; movb sdoc_pipe, %al
    movq %rdx, (%rbp,%rax,8)

    movb comment_hash, %al
    movq $hash_character, (%rbp,%rax,8)

  jmp setup_dispatch_table_cc

# Standard input reader.
# There are two steps here. First, allocate a buffer using mmap; second, read stuff into that buffer using the read syscall. The stdin buffer address is circular and the current offset is stored
# in %r15. It is aligned to a 4096-byte boundary, so %r15 & ~4095 gives you the base address of the allocation.

  setup_stdin_buffer:
  movq $4096, %rsi
  movq $setup_stdin_buffer_ok, %r12
  jmp allocate

  setup_stdin_buffer_ok:
  movq %rax, %r15
  andq $0xfffffffffffff000, %rax                # Check memory alignment
  cmpq %rax, %r15
  je setup_stdin_buffer_cc

  movq $mmap_alignment_error, %rsi              # Alignment error; use modulus as exit code
  movq mmap_alignment_error_size, %rdx
  andq $4095, %r15
  movq %r15, %rdi
  movq $exit, %r12
  jmp stderr

  fill_buffer:                                  # continuation = %r12 -> bytes = %rax
  xorq %rax, %rax                               # syscall = read
  xorq %rdi, %rdi                               # fd      = 0 (stdin)
  movq %r15, %rdx                               # buf     = %r15 (stdin buffer)
  movq $4096, %r10                              # length  = 4096 (stdin buffer size)
  syscall

  testq %rax, %rax                              # If no data, set %r15 to zero. Any reads will then segfault.
  jnz fill_buffer_nonzero
  xorq %r15, %r15

  fill_buffer_nonzero:
  jmpq *%r12

  stdin_loop:                                   # continuation = %r12, table = %rbp (assume it obliterates all registers)
  pushq %r12
  movq $stdin_loop_top, %r12
  jmp fill_buffer

  movq %rax, %r13                               # r13 marks buffer length

  stdin_loop_top:
  movq %r15, %r14                               # see if we're at the end yet
  andq $4095, %r14
  testq %r14, %r13
  ja stdin_loop_return

  testq %r15, %r15                              # stdin eof is indicated by %r15 being zero
  jz stdin_loop_return

  xorq %rax, %rax                               # zero out %rax just in case...
  movb (%r15), %al                              # load the next byte
  incq %r15

  movq $stdin_loop_top, %r12
  movq (%rbp,%rax,8), %rax
  jmp *%rax

  stdin_loop_return:
  popq %r12
  jmp *%r12

# SDoc reader.
# Munch stuff, storing state by jumping instead of by using registers.

  nop_character:
  jmp *%r12                                     # Immediately invoke continuation

  hash_character:
  movq $7, %rdi
  jmp exit

  read_sdoc_paragraph:
  movq $3, %rdi                                 # Sentinel return
  jmp exit

# Generated by SDoc 
