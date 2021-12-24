# dts2tsv
Tools to compare dtb &amp; dts files

These tools are compiled with freepascal. To compile:-
fpc multidtc.pas
fpc dts2tsv.pas

Multidtc is simply a wrapper for dtc which you need to install yourself.
dtc will only decompile one dtb file at a time. Multidtc will decompile multiple files in one go.
Yes YOU can probably do it with shell script.  I prefer pascal.

Assuming that you have some arm linux kernel already compiled (using something like 
make ARCH=arm64 CROSS_COMPILE=~/gcc-arm-10.3-2021.07-x86_64-aarch64-none-linux-gnu/bin/aarch64-none-linux-gnu- dtbs
)

Let's say you are interested in a Rockchip device with an RK356x Soc:-

mkdir dtbdump
cp arch/arm64/boot/dts/rockchip/*.dtb dtbdump/.
./multidtc dtbdump/rk356*.dtb

Now you have a bunch of rk3566-whatever.dtb-decompiled.dts files in dtbdump/

Suppose that you have some new not well understood device that you ripped the dtb file out of.
./multidtc newdevice/rk3566-newdevice.dtb
./dts2tsv 
./dts2tsv newdevice/rk3566-newdevice.dts dtbdump/rk356*.dtb-decompiled.dts out.tsv

Don't forget the out.tsv because otherwise dts2tsv will overwrite the last dts file with the output (but it will ask you if you are sure).

Load the out.tsv file into a spreadsheet like Libreoffice setting all columns to text, and tab as the only delimiter.

If you don't want to compare a new device and just want to compare all dts files in a folder then you can do that with
./dts2tsv dtbdump/*.dts out.tsv

Note that putting the new device first sets that as the "template" that all other ones are compared to, in the sense that the first column of the spreadsheet is built with the first file, and then nodes in the subsequent dts files that don't appear in the first one appear at the bottom of the spreadsheet.
