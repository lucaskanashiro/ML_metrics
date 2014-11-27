#!/usr/bin/perl -w

use strict;
use warnings;

my $num_args = $#ARGV + 1;

if($num_args != 2){
  print "\nUsage: pre-processor.pl <directory> <language>\n";
  exit;
}

my $path = $ARGV[0];
my $language = $ARGV[1];

sub process_files {
    my $path = shift;
    my $language = shift;

    opendir (DIR, $path) or die "Unable to open $path: $!";

    my @files = grep { !/^\.{1,2}$/ } readdir (DIR);

    closedir (DIR);

    @files = map { $path . '/' . $_ } @files;

    for (@files) {
        if (-d $_) {
            process_files ($_);
        } else { 
            if($language eq "Java") { `./pre-processor-java.pl $_`; }
            if($language eq "C") { `./pre-processor-c.pl $_`; }
            if($language eq "C++") { `./pre-processor-c++.pl $_`; }
        }
    }   
}

process_files($path, $language);

