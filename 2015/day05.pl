#!/usr/bin/perl6

sub part1($filename) {
    my $nice_cnt = 0;
    
    for $filename.IO.lines -> $line {
	if ($line ~~ m/<[aeiou]>.*<[aeiou]>.*<[aeiou]>/) &
	    ($line ~~ m/(<:L>)$0/) &
	    !(($line ~~ m/ab/) |
	      ($line ~~ m/cd/) |
	      ($line ~~ m/pq/) |
	      ($line ~~ m/xy/)) {
	    $nice_cnt++;
	}
    }

    say "Number of nice words: $nice_cnt";
}

sub part2($filename) {
    my $nice_cnt = 0;
    
    for $filename.IO.lines -> $line {
	if ($line ~~ m/(<:L> ** 2).*$0/) &
	    ($line ~~ m/(<:L>)<:L>$0/) {
	    $nice_cnt++;
	}
    }

    say "Number of nice words: $nice_cnt";
}

sub MAIN($filename) {
    part2($filename)
}

# -*- mode: perl -*-
