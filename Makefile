default: simulation-iverilog

TESTBENCH = $(wildcard testbench/*.sv)

.PHONY: target/debug/libfstdumper.so
target/debug/libfstdumper.so:
	cargo build

Vtop.vvp: $(TESTBENCH)
	iverilog -o $@ -s div_int_tb $(TESTBENCH) -g2012

fstdumper.so.vpi : fstdumper.so
	@cp $< $@

simulation-iverilog: target/debug/libfstdumper.so Vtop.vvp
	vvp -M . -mtarget/debug/libfstdumper.so Vtop.vvp

simulation-xrun: target/debug/libfstdumper.so
	xrun -64bit +access+r -loadvpi ./target/debug/libfstdumper.so:vlog_startup_routines_bootstrap $(TESTBENCH) -top div_int_tb

simulation-vsim: target/debug/libfstdumper.so
	vlog -64 $(TESTBENCH)
	vsim -64 -c div_int_tb -vpicompatcb -plicompatdefault latest -pli fstdumper.so -do "run -all"

.PHONY: clean
clean:
	rm -f $(obj) *.so *.so.vpi *.vvp *.fst

