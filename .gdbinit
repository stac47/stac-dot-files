set sysroot /
add-auto-load-safe-path ~
add-auto-load-safe-path .gdbinit
add-auto-load-safe-path /lib
add-auto-load-safe-path /lib64
add-auto-load-safe-path /opt/1A/toolchain/*/lib
add-auto-load-safe-path /softntools/opt/1A/toolchain/*/lib
add-auto-load-safe-path /opt/1A/toolchain/*/share/*/python/libstdcxx
add-auto-load-safe-path /opt/1A/toolchain/*/share/*/python/libstdcxx/v6


set print pretty on
set print object on
set print static-members on
set print vtbl on
set demangle-style gnu-v3
set print sevenbit-strings off
set print demangle on
set print asm-demangle on
set print symbol on
set print symbol-filename on
