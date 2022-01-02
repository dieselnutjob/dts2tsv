# dts2tsv
Tools to compare dtb &amp; dts files

These tools are compiled with freepascal. To compile:-

```
fpc multidtc.pas

fpc dts2tsv.pas
```

Multidtc is simply a wrapper for dtc which you need to install yourself.
dtc will only decompile one dtb file at a time. Multidtc will decompile multiple files in one go.

Assuming that you have some arm linux kernel already compiled (using something like 
```
make ARCH=arm64 CROSS_COMPILE=~/gcc-arm-10.3-2021.07-x86_64-aarch64-none-linux-gnu/bin/aarch64-none-linux-gnu- dtbs
```
)

Let's say you are interested in a Rockchip device with an RK356x Soc:-

mkdir dtbdump

cp arch/arm64/boot/dts/rockchip/*.dtb dtbdump/.

```
./multidtc dtbdump/rk356*.dtb
```

Now you have a bunch of rk3566-whatever.dtb2s files in dtbdump/

Suppose that you have some new not well understood device that you ripped the dtb file out of.

```
./multidtc rk3566-newdevice.dtb

./dts2tsv -s *.dts *.dtsi -d rk3566-newdevice.dtb2s dtbdump/*.dtb2s -o out.tsv

```
Load the out.tsv file into a spreadsheet like Libreoffice setting all columns to text, and tab as the only delimiter.

If you don't want to compare a new device and just want to compare all dts files in a folder then you can do that with
```
./dts2tsv -d dtbdump/*.dtb2s -o out.tsv
```

Note that putting the new device first sets that as the "template" that all other ones are compared to, in the sense that the first column of the spreadsheet is built with the first file, and then nodes in the subsequent dts files that don't appear in the first one appear at the bottom of the spreadsheet.

What does this program actually do?

1. It uses the -s switch to load in dts and dtsi files.
It uses these files only to identify values which are phandles instead of u32 values.
The reason for this is that decompiled dtb files (which are called dtb2s files here) are easy to compare, but, certain pieces of information get lost in compilation, for example which values in a node are variables (u32 vales) and which are phandles (so pointers to other variables).
This makes direct comparison of decompiled dtb files slightly hit and miss.
The information concerning which values are variables and which are phandles is present in original source files, and dts2tsv is able to use this information to ascertain which fields in a related decompiled dtb file are which.

2. It uses the -d switch to load decompiled dtb files.
Each file is scanned for node and subnode names that match ones in dts and dtsi files which are known to be phandles.
When a value is found which might be a phandle, the rest of file is then scanned for nodes which have a phandle which matches, and the value is substituted for the node name that owns the matching phandle.

3. Each decompiled dtb file is given a new column in the output spreadsheet and nodenames are sorted so that each node can be compared with other nodes that have the same name.
