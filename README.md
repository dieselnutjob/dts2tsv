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

```
mkdir dtbdump

cp arch/arm64/boot/dts/rockchip/*.dtb dtbdump/.

./multidtc dtbdump/rk356*.dtb
```

Now you have a bunch of rk3566-whatever.dtb2s files in dtbdump/

Suppose that you have some new not well understood device that you ripped the dtb file out of.

Firstly you need to identify another device which is similar and for which source code files (dts, dtsi and includes) are available.

Create a folder and put the device dts and all of the dsti and h files into that folder. 

```
./multidtc rk3566-newdevice.dtb

./dts2tsv -s source/rk3566-quartz64-a.dts -d rk3566-newdevice.dtb2s dtbdump/*.dtb2s -o out.tsv

```
Load the out.tsv file into a spreadsheet like Libreoffice setting all columns to text, and tab as the only delimiter.

Note that putting the new device first sets that as the "template" that all other ones are compared to, in the sense that the first column of the spreadsheet is built with the first file, and then nodes in the subsequent dts files that don't appear in the first one appear at the bottom of the spreadsheet.

What does this program actually do?

1. It uses the -s switch to load in a dts file and dtsi/include/define files which mentioned in the dts file.
It uses these files only to identify values which are phandles instead of u32 values.
The reason for this is that decompiled dtb files (which are called dtb2s files here) are easy to compare, but, certain pieces of information get lost in compilation, for example which values in a node are variables (u32 vales) and which are phandles (so pointers to other variables).
This makes direct comparison of decompiled dtb files slightly hit and miss.
The information concerning which values are variables and which are phandles is present in original source files, and dts2tsv is able to use this information to ascertain which fields in a related decompiled dtb file are which.
From version 3 the program converts nodenames to labels if the label and nodename are different, and then searches for phandles associated with those labels as well.

Note that loading dts or dtsi files with overlapping or contradictory nodenames with associated phandles is not tested.  It is probably best to only load dts and dtsi files which relate to hardware which is as similar as possible to the device of interest.

2. It uses the -d switch to load decompiled dtb files.
Each file is scanned for node and subnode names that match ones in dts and dtsi files which are known to be phandles.
When a value is found which might be a phandle, the rest of file is then scanned for nodes which have a phandle which matches, and the value is substituted for the node name that owns the matching phandle.

3. Each decompiled dtb file is given a new column in the output spreadsheet and nodenames are sorted so that each node can be compared with other nodes that have the same name.

Optional swithes are:-

-c [filename]  This dumps out a single big dts file consisting of the original file specified in -s along with all of the files mentioned in the original file.

-h  Don't bother with files containing defines and don't convert defines strings into values.

-p  Show nodenames that end in phandle.  By default they are suppressed in the output tsv file.
