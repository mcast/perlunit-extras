#! /usr/local/bin/perl -wc

package McaTestCase;

use strict;
use base 'Test::Unit::TestCase';
use Data::Dumper;

# inherit perfectly adequate new, set_up

sub tear_down {
    my $self = shift;

    # Display data structures from testcase, if we fail
    if ($self->{annotate}) {
	my $d = Data::Dumper->new([values %{ $self->{annotate} }], [keys %{ $self->{annotate} }]);
	$d->Indent(1)->Purity(1)->Quotekeys(0);
	$self->annotate($d->Dump);
    }

    $self->SUPER::tear_down;
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
