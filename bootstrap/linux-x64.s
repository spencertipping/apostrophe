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

  buffer_fill_error:      .ascii "error: failed to fill input buffer\n"
  buffer_fill_error_size: .quad . - buffer_fill_error

  start_sdoc_capitals: .byte 'A'
  end_sdoc_capitals:   .byte 'Z'
  sdoc_pipe:           .byte '|'
  comment_hash:        .byte '#'
  newline:             .byte '\n'

.section .data
  main_dispatch_table:         .quad 0
  sdoc_dispatch_table:         .quad 0
  sdoc_newline_dispatch_table: .quad 0
  line_comment_dispatch_table: .quad 0

  dispatch_table_stack:        .quad 0

  stdin_buffer:                .quad 0
  stdin_buffer_limit:          .quad 0

.text
  .globl _start
  .type _start, @function

# Toplevel entry point.
# Create the stdin buffer and dispatch table, then jump into the reader.

  _start:
  jmp setup_dispatch_tables
  setup_dispatch_tables_cc:

  # Install the initial dispatch table.
  movq main_dispatch_table, %rbp

  movq $exit_successfully, %r12
  jmp stdin_loop

# Toplevel functions.
# These are called from various places and use %r12 as a return address.

  # Exit-successfully function.
#   Called after the reader completes (i.e. reaches eof).

    exit_successfully:
    xorq %rdi, %rdi
    jmp exit

  # Exit function.
#   Takes the exit code as %rdi and dies immediately.

    exit:
    movq $60, %rax
    syscall

  # Stderr logger.
#   Takes the message to log as %rsi, the length of the message as %rdx, and the continuation as %r12. %rdi is preserved.

    stderr:                                     # buf = %rsi, length = %rdx, continuation = %r12 -> result = %rax
    pushq %rdi
    movq $1, %rax                               # syscall = write
    movq $2, %rdi                               # fd      = 2 (stderr)
    syscall
    movq $74, %rax                              # syscall = fsync
    syscall
    popq %rdi
    jmp *%r12

  # Memory allocator.
#   Uses mmap() to allocate a chunk of memory. The size is passed in as %rsi and the continuation is %r12. As per syscall convention, the newly allocated memory is returned as %rax. This
#   function won't return an error value; instead, it will simply kill the process with a nonzero error code.

    allocate:                                   # size = %rsi, continuation = %r12 -> result = %rax (exits on error)
    pushq %rdi
    pushq %rdx
    pushq %r10
    pushq %r8
    pushq %r9
    movq $9, %rax                               # syscall = mmap
    xorq %rdi, %rdi                             # *addr   = NULL
                                                # length  = %rsi
    movq $3, %rdx                               # prot    = PROT_READ | PROT_WRITE
    movq $34, %r10                              # flags   = MAP_PRIVATE | MAP_ANONYMOUS
    movq $-1, %r8                               # fd      = -1
    xorq %r9, %r9                               # offset  = 0
    syscall
    popq %r9
    popq %r8
    popq %r10
    popq %rdx

    testq %rax, %rax                            # negative return values from mmap() indicate failure
    jns allocate_ok

    movq $mmap_error, %rsi
    movq mmap_error_size, %rdx
    negq %rax                                   # use errno as program return code
    movq %rax, %rdi
    movq $exit, %r12                            # no recovery; just exit immediately
    jmp stderr

    allocate_ok:
    popq %rdi
    jmp *%r12

  # Dispatch table initialization.
#   This sets up a dispatch table whose address is specified in %rbp. It just sets up every entry to point to the nop function and then jumps to the continuation in %r12.

    initialize_dispatch_table_with_nops:
    pushq %rdx
    pushq %rbx
    movq %rbp, %rbx
    addq $2048, %rbx
    movq $nop_character, %rdx

    initialize_dispatch_table_with_nops_loop:
    movq %rdx, (%rax)

    addq $8, %rax
    cmpq %rax, %rbx
    jae initialize_dispatch_table_with_nops_loop
    popq %rbx
    popq %rdx
    jmp *%r12

# Reader functions.
# These govern the program's execution post-initialization.

  # Static link points.
#   These functions are referenced from dispatch tables.

    nop_character:
    movq $7, %rdi
    jmp exit
    jmp *%r12                                     # Immediately invoke continuation

    sdoc_advance_to_second_state:
    movq sdoc_newline_dispatch_table, %rbp
    jmp *%r12

    sdoc_restore_first_sdoc_table:
    movq sdoc_dispatch_table, %rbp
    jmp *%r12

    sdoc_start_paragraph:
    movq %rbp, (dispatch_table_stack)
    subq $8, dispatch_table_stack
    movq sdoc_dispatch_table, %rbp
    movq $3, %rdi
    jmp exit

    pop_table:
    movq (dispatch_table_stack), %rbp
    addq $8, dispatch_table_stack
    jmp *%r12

    start_line_comment:
    movq %rbp, (dispatch_table_stack)
    subq $8, dispatch_table_stack
    movq line_comment_dispatch_table, %rbp
    movq $7, %rdi
    jmp exit

  # Standard input reader.
#   There are two steps here. First, allocate a buffer using mmap; second, read stuff into that buffer using the read syscall. The stdin buffer address is circular and the current offset is
#   stored in %r15. It is aligned to a 4096-byte boundary, so %r15 & ~4095 gives you the base address of the allocation.

    stdin_loop:                                 # continuation = %r12, table = %rbp (returns after stdin eof)
    pushq %r12

    movq $4096, %rsi
    movq $stdin_loop_buffer_cc, %r12
    jmp allocate
    stdin_loop_buffer_cc:
    movq %rax, stdin_buffer

    movq $stdin_loop_init, %r12
    jmp fill_buffer

    movq $mmap_alignment_error, %rsi            # Alignment error; use modulus as exit code
    movq mmap_alignment_error_size, %rdx
    andq $4095, stdin_buffer
    movq stdin_buffer, %rdi
    movq $exit, %r12
    jmp stderr

    fill_buffer:                                # continuation = %r12 -> bytes = %rax (resets %r15)
    andq $0xfffffffffffff000, stdin_buffer      # realign %r15 to next-lowest 4096-byte boundary
    xorq %rax, %rax                             # syscall = read
    xorq %rdi, %rdi                             # fd      = 0 (stdin)
    movq stdin_buffer, %rsi                     # buf     = %r15 (stdin buffer)
    movq $4096, %rdx                            # length  = 4096 (stdin buffer size)
    syscall
    testq %rax, %rax                            # If no data, set stdin_buffer to zero. Any erroneous reads against it will then segfault.
    js  fill_buffer_error
    jnz fill_buffer_cc
    xorq %rax, %rax
    movq %rax, stdin_buffer

    fill_buffer_cc:
    jmp *%r12

    fill_buffer_error:
    negq %rax
    movq %rax, %rdi
    movq $buffer_fill_error, %rsi
    movq buffer_fill_error_size, %rdx
    movq $exit, %r12
    jmp stderr

    stdin_loop_init:
    movq %rax, stdin_buffer_limit
    movq stdin_buffer, %rax
    addq %rax, stdin_buffer_limit

    stdin_loop_top:
    movq stdin_buffer, %rax
    testq %rax, %rax                            # stdin eof is indicated by stdin_buffer (%rax) being 0
    jz stdin_loop_return
    cmpq %rax, stdin_buffer_limit               # see if we're at or past the end of the buffer...
    jbe fill_buffer                             # ... and if we are, fill the buffer and return to the loop top

    movq %rax, stdin_buffer

    xorq %rax, %rax                             # zero out %rax just in case...
    movb (stdin_buffer), %al                    # load the next byte
    incq stdin_buffer
    movq $stdin_loop_top, %r12
    movq (%rbp,%rax,8), %rax

    jmp *%rax                                   # tail call, so we wouldn't be able to restore registers after this

    stdin_loop_return:
    popq %r12
    jmp *%r12

# Dispatch table setup.
# The initial dispatch table is dynamically allocated using mmap and initialized from assembly code. Letters from A-Z and | are set up to consume input until a \n\n is reached. This allows you
# to write SDoc into the bytecode.

  setup_dispatch_tables:

  # Allocate the table stack.
  movq $4096, %rsi
  movq $setup_dispatch_stacks_allocated, %r12
  jmp allocate
    setup_dispatch_stacks_allocated:
    movq %rax, dispatch_table_stack

  movq $16384, %rsi                             # enough space for 8 dispatch tables
  movq $setup_dispatch_tables_allocated, %r12
  jmp allocate
    setup_dispatch_tables_allocated:
    movq %rax, %rbp

  # Main dispatch instructions.
#   The dispatch table contains a bunch of jump offsets to functions defined here in assembly code.

    movq $customize_main_dispatch_table, %r12
    jmp initialize_dispatch_table_with_nops

    customize_main_dispatch_table:
    xorq %rcx, %rcx; movb start_sdoc_capitals, %cl; leaq (%rbp,%rcx,8), %rcx
    xorq %rbx, %rbx; movb end_sdoc_capitals,   %bl; leaq (%rbp,%rbx,8), %rbx
    movq $sdoc_start_paragraph, %rdx

    setup_dispatch_table_capital_letter_loop:
    movq %rdx, (%rcx)

    addq $8, %rcx
    cmpq %rcx, %rbx
    jae setup_dispatch_table_capital_letter_loop

    # Also setup the pipe and hash character mappings:
    xorq %rcx, %rcx; movb sdoc_pipe, %cl
    movq %rdx, (%rbp,%rcx,8)

    movb comment_hash, %cl
    movq $start_line_comment, (%rbp,%rcx,8)

    # Save the dispatch table for later use
    movq %rbp, main_dispatch_table

  # SDoc dispatch instructions.
#   This is the second dispatch table and is activated when parsing an SDoc paragraph. It serves only to activate the second SDoc dispatch table; this one is used to handle the second
#   consecutive newline.

    addq $2048, %rbp
    movq $customize_sdoc_dispatch_table, %r12
    jmp initialize_dispatch_table_with_nops

    customize_sdoc_dispatch_table:
    xorq %rcx, %rcx
    movb newline, %cl
    movq $sdoc_advance_to_second_state, (%rbp,%rcx,8)

    movq %rbp, sdoc_dispatch_table

    # Second table (maps the second newline to a function that restores the main dispatch table; everything else goes to a function that reverts to the first SDoc table)
    addq $2048, %rbp
    xorq %rcx, %rcx

    movq $pop_table, %rbx
    customize_sdoc_newline_dispatch_table_loop:
    movq $sdoc_restore_first_sdoc_table, %rdx
    cmpb newline, %cl
    cmove %rbx, %rdx
    movq %rdx, (%rbp,%rcx,8)

    incq %rcx
    testq $255, %rcx
    jnz customize_sdoc_newline_dispatch_table_loop

    movq %rbp, sdoc_newline_dispatch_table

  # Line comment dispatch table.
#   This is the same idea as the second SDoc table but simpler: everything is a nop except for a newline, which restores the previous table.

    addq $2048, %rbp
    movq $customize_line_comment_table, %r12
    jmp initialize_dispatch_table_with_nops

    customize_line_comment_table:
    xorq %rcx, %rcx
    movb newline, %cl
    movq $pop_table, (%rbp,%rcx,8)

    movq %rbp, line_comment_dispatch_table

  jmp setup_dispatch_tables_cc

# Generated by SDoc 
