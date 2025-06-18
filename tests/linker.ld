/* Simple linker script for RISC-V user programs */

ENTRY(_start)

MEMORY
{
    RAM : ORIGIN = 0x10000, LENGTH = 0x70000000
}

SECTIONS
{
    . = 0x10000;
    
    .text : {
        *(.text.start)
        *(.text)
        *(.text.*)
    } > RAM
    
    .rodata : {
        *(.rodata)
        *(.rodata.*)
    } > RAM
    
    .data : {
        *(.data)
        *(.data.*)
    } > RAM
    
    .bss : {
        __bss_start = .;
        *(.bss)
        *(.bss.*)
        *(COMMON)
        __bss_end = .;
    } > RAM
    
    . = ALIGN(16);
    __stack_top = 0x7fff0000;
}