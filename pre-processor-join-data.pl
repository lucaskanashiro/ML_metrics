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

    my @files = grep { /^.+-processed.csv/ } readdir (DIR);

    closedir (DIR);

    @files = map { $path . '/' . $_ } @files;

    my $output_handler;
    my $output_file;
    #print $path;
    ($output_file = $path."/all-data-".$language) =~ s/\.[^.]+$//;
    open($output_handler, '>', $output_file.'.csv') or die $!;
 

    for (@files) {
        my $file = $_;
        my $file_handler;

        open($file_handler, '<', $file) or die $!;

        #my $trash = <$file_handler>;

        while(<$file_handler>) {
          chomp $_;
          print $output_handler $_."\n";
        }

        close $file_handler;
        print "Processed ".$file."\n";
    }   
        close $output_handler;

}

process_files($path, $language);

