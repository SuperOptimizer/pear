/* Linker script for RISC-V usermode programs */

OUTPUT_ARCH("riscv")
ENTRY(_start)

SECTIONS
{
    . = 0x10000;
    
    .text : {
        *(.text.startup)
        *(.text .text.*)
    }
    
    .rodata : {
        *(.rodata .rodata.*)
    }
    
    .data : {
        *(.data .data.*)
    }
    
    .bss : {
        *(.bss .bss.*)
        *(COMMON)
    }
    
    /DISCARD/ : {
        *(.comment)
        *(.note.*)
        *(.eh_frame)
    }
}