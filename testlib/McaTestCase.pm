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

sub assert_dies {
    my ($self, $regex, $coderef) = @_;
    eval {
	$coderef->();
    };
    my $err = $@;
    $self->fail("Code did not die") unless $err;
    $self->assert_matches( $regex, $err );
}


1;
