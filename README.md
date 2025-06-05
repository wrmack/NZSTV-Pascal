# Single Transferable Vote (Meek's method)
This is pascal code based on David Hill's [code](./Meek_method_STV_Hill.pdf) for implementing Meek's method of transfering votes. This demonstrates the method used by New Zealand.

The input file containing votes must be in blt format as described in the pdf document. There are two example blt files - one based on the file used by David Hill.

The input file is hard-wired into the code:
```pascal
Begin{main program}
... 
  assign(datafile,'Sydney.blt');
```
## To install

Install a pascal compiler - in Linux I used the free pascal compiler - fpc

```bash
# Compile the executable
fpc -gw fullalg123.pas

# Run the executable
./fullalg123.pas
```
Results are written to the files: votes.oei and votes.res
