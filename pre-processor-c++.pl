#!/usr/bin/perl -w

use strict;
use warnings;

my $num_args = $#ARGV + 1;

if($num_args != 1){
  print "\nUsage: pre-processor.pl <file>\n";
  exit;
}

my $file = $ARGV[0];
my $output_file;
my $file_handler;
my $output_handler;

open($file_handler, '<', $file) or die $!;
($output_file = $file) =~ s/\.[^.]+$//;
open($output_handler, '>', $output_file.'-processed.csv') or die $!;

my $trash = <$file_handler>;

while(<$file_handler>) {
  chomp $_;
  my @values = split /,/, $_; 
  my $great = 0; 

  if($values[0] <= 7) { $great += 1; }
  if($values[1] <= 4) { $great += 1; }
  if($values[2] <= 19.5) { $great += 1; }
  if($values[3] <= 3) { $great += 1; }
  if($values[4] <= 5) { $great += 1; }
  if($values[5] <= 3) { $great += 1; }
  if($values[6] <= 10) { $great += 1; }
  if($values[7] <= 84) { $great += 1; }
  if($values[9] <= 8) { $great += 1; }
  if($values[10] <= 1) { $great += 1; }
  if($values[11] <= 17) { $great += 1; }
  if($values[12] <= 1) { $great += 1; }
  if($values[13] <= 7) { $great += 1; }
  if($values[14] <= 64) { $great += 1; }
  if($values[15] <= 28) { $great += 1; }

  my $great_value;

  if($great / 15 >= 0.5) { $great_value = 1; }
  else { $great_value = 0; }

  print $output_handler $values[0].','.$values[1].','.$values[2].','.$values[3].','.$values[4].','.$values[5].','.$values[6].','.$values[7].','.$values[9].','.$values[10].','.$values[11].','.$values[12].','.$values[13].','.$values[14].','.$values[15].','.$great_value."\n";
}

close $output_handler;
close $file_handler;

print "Processed ".$file."\n";

