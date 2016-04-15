#!/usr/bin/perl -w
use strict;
use File::Basename;
use Getopt::Std;
use lib '/opt/seismo-util/lib/perl';
use CMT_TOOLS;
use DELAZ5;


system("rm fout*");

my $filer = "file0001.vtk";
my $head = "header.vtk";
my (@lines,@header);
my ($num,$files,$fout);

#abro la unidad y leo
open (FH, "< $head") or die "Can't open $head for read: $!";
while (<FH>) {
    push (@header, $_);
}
close FH or die "Cannot close $filer: $!";


for ($num=1; $num<=45; $num++)
{
open my $fh, '>', \$files or die "Can't open variable: $!";
printf $fh "file%04d.vtk",$num;
open my $fh2, '>', \$fout or die "Can't open variable: $!";
printf $fh2 "fout%04d.vtk",$num;
#abro la unidad y leo
open (FH, "< $files") or die "Can't open $files for read: $!";
while (<FH>) {
    push (@lines, $_);
}
close FH or die "Cannot close $files: $!";
open (FH, ">> $fout") or die "Can't open for write: $!";
        printf FH $header[0];
        printf FH $header[1];
        printf FH $header[2];
        printf FH $header[3];
        printf FH $header[4];
        printf FH $header[5];
        printf FH $header[6];
        printf FH $header[7];
        printf FH $header[8];
        printf FH $header[9];
        printf FH @lines;
@lines=();
close FH or die "Cannot close";
}
