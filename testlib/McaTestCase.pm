#! /usr/local/bin/perl -wc

package McaTestCase;

use strict;
use base 'Test::Unit::TestCase';
use Data::Dumper;

# inherit perfectly adequate new, set_up
my %test_times; # key="testclass->test_name", value = [ starttime, endtime ]
my $begin_T;
my $hi_time;
BEGIN {
    $begin_T = $^T;
    $hi_time = eval "use Time::HiRes; 1";
}

sub time2 {
    if ($hi_time) {
	return Time::HiRes::tv_interval( [$begin_T,0] );
    } else {
	return time() - $begin_T;
    }
}

sub set_up {
    my $self = shift;
    $self->SUPER::set_up;

    my $t = $self->global_test_name;
    warn "Test $t: info already defined?!\n" if defined $test_times{$t};
    $test_times{$t}->[0] = time2();
}

sub tear_down {
    my $self = shift;

    # Display data structures from testcase, if we fail
    if ($self->{annotate}) {
	my $d = Data::Dumper->new([values %{ $self->{annotate} }], [keys %{ $self->{annotate} }]);
	$d->Indent(1)->Purity(1)->Quotekeys(0);
	$self->annotate($d->Dump);
    }

    $self->SUPER::tear_down;

    my $t = $self->global_test_name;
    $test_times{$t}->[1] = time2();
}

sub global_test_name { return ref($_[0])."->".$_[0]->name }

END {
    my $tot_time = time2();
    $tot_time = 0.1 if $tot_time == 0;
    my $tests = (scalar keys %test_times) || 1;
    my $time_lim = $tot_time / $tests * 3;
    my @timings;
    foreach my $t (sort keys %test_times) {
	my ($start, $end) = @{ $test_times{$t} };
	if (defined $start && defined $end) {
	    my $elapsed = $end - $start;
	    push @timings,
	      sprintf("%-60s %3.1fs %3d%%\n", $t, $elapsed, 100 * $elapsed / $tot_time)
		if $elapsed > $time_lim;
	} else {
	    push @timings, "$t\tdata missing\n";
	}
    }
    print( "-" x 70,
	   "\nSlow test info (threshold ",
	   sprintf("%3.1fs)\n", $time_lim),
	   @timings)
      if @timings;
}

# Assert that each coderef dies with a message matching the regex
sub assert_dies {
    my ($self, $regex, @coderef) = @_;

    # Need to include caller in failure message, since the builtin
    # line number reporting is no help
    my @caller = caller();
    my $caller = "caller at $caller[1] line $caller[2]";

    $self->fail("arg1 not Regexp from $caller") unless ref($regex) eq 'Regexp';
    $self->fail("Bad args (no coderefs) from $caller") unless @coderef;
    $self->fail("Bad args (coderefs aren't) from $caller") if grep {ref($_) ne 'CODE'} @coderef;

    for (my $i=0; $i<@coderef; $i++) {
	eval {
	    $coderef[$i]->();
	};
	my $err = $@;
	$self->fail("Code#$i did not die, $caller") unless $err;
	$self->assert_matches($regex, "Code#$i '$err', $caller");
    }
}

# Self-tests for this base class, might as well piggyback unless/until
# that wastes too much time

sub test_baseselftest_assert_dies {
    my $self = shift;

    # Run our assertion so it should pass, check it ran
    my $flag = 0;
    $self->assert_dies(qr/dodo style/, sub { $flag = "captured"; die "dodo style"; });
    $self->assert_str_equals("captured", $flag);

    ### Use the builtin Test::Unit::Assert to check the assertion fails:

    # doesn't die
    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_dies( qr/wibble/, sub { return 1; });
			 });

    # wrong arg
    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_dies( "foo", sub { return 1; });
			 });

    # multi coderef, second has wrong message
    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_dies( qr/wibble/,
						 sub { die "wibble" },
						 sub { die "spong"  });
			 });

    # multi coderef, third doesn't die
    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_dies( qr/wibble/,
						 sub { die "eek wibble bonk" },
						 sub { die "boo wibble spong" },
						 sub { return 1; });
			 });

}


1;
