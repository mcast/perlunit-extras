package Test::Unit::Assert::CodeBehaviour;

# Interleaved test methods appear in the Test::Unit::Assert::SelfTest
# package.  You can add it to your suite or not.
#   package Test::Unit::Assert::SelfTest;
#   our @ISA = qw(McaTestCase);
#   # require Carp; # needed for self-testing only

use strict;
use warnings;

use Exporter 'import'; # have Exporter's import() method directly
our @EXPORT_OK = qw(wantarray_type);  # exportable utility


=head1 NAME

Test::Unit::Assert::CodeBehaviour - assertions for testing CODErefs

=head1 SYNOPSIS

 use base qw( Test::Unit::TestCase Test::Unit::Assert::CodeBehaviour );

=head1 DESCRIPTION

This is styled as a mixin class, so it mostly provides a bunch of
extra object methods.

=cut

# $Id $

#----------------------------------------------------------------------------


## Mixin class should NOT provide set_up and tear_down, else other
## callers will have to mess with NEXT.


=head2 Extra C<assert_*> methods

These can be used in the usual way.

=over 4

=item C<assert_dies( qr/message regex/, @coderefs, $descr)>

Run each piece of code (in void context).  Assert that each one dies,
and that the resulting error message matches the regex.

Extra description C<$descr> is optional.

=item C<assert_warns( qr/message regex/, @coderefs, $descr)>

Run each piece of code (in void context).  Assert that each one
generates a warning, and that the message matches the regex.

Extra description C<$descr> is optional.

=back

=cut


# Assert that each coderef dies with a message matching the regex
sub assert_dies {
    my ($self, $regex, @coderef) = @_;

    # Need to include caller in failure message, since the builtin
    # line number reporting is no help
    my @caller = caller();
    my $caller = "caller at $caller[1] line $caller[2]";

    if (@coderef > 1 && !ref($coderef[-1])) {
	# Last item isn't code, take it as an extra piece of message
	$caller .= ", ".pop(@coderef);
    }

    $self->fail("arg1 not Regexp from $caller") unless ref($regex) eq 'Regexp';
    $self->fail("Bad args (no coderefs) from $caller") unless @coderef;
    $self->fail("Bad args (coderefs aren't) from $caller") if grep {ref($_) ne 'CODE'} @coderef;

    for (my $i=0; $i<@coderef; $i++) {
	eval {
	    $coderef[$i]->();
	};
	my $err = $@;
	my $which = "";
	$which = join "", "[", $i+1, " of ", scalar @coderef, "]" if @coderef > 1;
	$self->fail("Code$which did not die, $caller") unless $err;
	$self->assert_matches($regex, $err,
			      "Code$which, $caller:\n  Died with '$err'\n  which didn't match $regex");
    }
}

sub McaTestCaseTest::test_assert_dies {
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

    # wrong args
    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_dies( "foo", sub { return 1; });
			 });
    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_dies( qr/foo/,
						 sub { return 1; },
						 "This is not code",
						 "Could be a message?");
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

    # old bug check
    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_dies( qr/caller/i,
						 sub { die "some other thing" } );
			 });

    # Check the error messages are useful
    # Working so far, so can use the method under test!
    $self->assert_dies( qr/Code\[2 of 2\]/,
			sub {
			    $self->assert_dies( qr/butter/,
						sub { die "butterfingers" },
						sub { die "aiee" });
			}, sub {
			    $self->assert_dies( qr/oops/,
						sub { die "oops" },
						sub { return 1 });
			});
    $self->assert_dies( qr/vital/,
			sub {
			    $self->assert_dies( qr/foo/,
						sub { die "unhelpful message" },
						"vital piece of info");
			});

    $self->assert_dies(qr{wantarray check},
		       sub {
			   my $T = wantarray_type(wantarray);
			   die "void context: wantarray check is ok" if $T eq 'void';
			   $self->fail("expected void context, got $T");
		       });
}


# Assert that each coderef generates one warning message matching the regex
sub assert_warns {
    my ($self, $regex, @coderef) = @_;

    # Need to include caller in failure message, since the builtin
    # line number reporting is no help
    my @caller = caller();
    my $caller = "caller at $caller[1] line $caller[2]";

    if (@coderef > 1 && !ref($coderef[-1])) {
	# Last item isn't code, take it as an extra piece of message
	$caller .= ", ".pop(@coderef);
    }

    $self->fail("arg1 not Regexp from $caller") unless ref($regex) eq 'Regexp';
    $self->fail("Bad args (no coderefs) from $caller") unless @coderef;
    $self->fail("Bad args (coderefs aren't) from $caller") if grep {ref($_) ne 'CODE'} @coderef;

    for (my $i=0; $i<@coderef; $i++) {
	my @warn;
	{
	    local $SIG{__WARN__} = sub { push @warn, "@_" };
	    $coderef[$i]->();
	}
	my $which = "";
	$which = join "", "[", $i+1, " of ", scalar @coderef, "]" if @coderef > 1;
	$self->fail("Code$which did not warn, $caller") unless @warn;
	$self->fail("Code$which warned more than once, $caller\n@warn") unless 1 == @warn;
	$self->assert_matches($regex, $warn[0],
			      "Code$which, $caller:\n  Warned with '$warn[0]'\n  which didn't match $regex");
    }
}

sub McaTestCaseTest::test_assert_warns {
    my $self = shift;

    my @leaked_warnings;
    local $SIG{__WARN__} = sub {
	push @leaked_warnings, "@_";
	warn @_ unless $_[0] =~ /^Deliberate/;
    };

    warn "Deliberate leakage of one warning";
    require Carp; # needed for self-testing only

    my $num = 0;
    $self->assert_warns(qr{\bfoo\b},
			sub { $num++; warn "this foo"; return qw( a b c ) },
			sub { $num++; warn "that foo\n"; return 1 },
			sub { $num++; Carp::carp "foo applied via Carp module" },
			sub { $num++; Carp::cluck "foo warning with stacktrace" },
			sub { $num++; warn "foo the last"; return () });
    $self->assert_num_equals(5, $num);

    $self->assert_dies(qr{don't trap die},
		       sub {
			   $self->assert_warns(qr{warntest}, sub { die "don't trap die" });
		       });

    $self->assert_raises("Test::Unit::Failure",
			 sub {
			     $self->assert_warns
			       (qr{\bfoo\b}, sub { return "no warning" });
			 });
    $self->assert_raises("Test::Unit::Failure",
			 sub {
			     $self->assert_warns
			       (qr{\bfoo\b}, sub { warn "unmatched warning" });
			 });
    $self->assert_raises("Test::Unit::Failure",
			 sub {
			     $self->assert_warns
			       (qr{bar},
				sub { warn "bar bar black sheep" },
				sub { return "no wool" },
				sub { die "Last code should not run" });
			 });
    $self->assert_raises("Test::Unit::Failure",
			 sub {
			     $self->assert_warns
			       (qr{moof}, sub { warn "first is moofy";
						warn "second also moofy but unwanted"; });
			 });
    $self->assert_raises("Test::Unit::Failure",
			 sub {
			     $self->assert_warns
			       (qr{spong}, sub { warn "spong"; warn "second doesn't" });
			 });

# XXX: should check that the assert_warns input args are sanitised carefully; this is shared with other asserts and should be tested together or refactored
    $self->assert_warns(qr{wantarray check},
			sub {
			    my $T = wantarray_type(wantarray);
			    $self->fail("expected void context, got $T") if $T ne 'void';
			    warn "wantarray check";
			});

    # Check the 'last arg can be text' thing works
    $self->assert_dies(qr/vital/,
		       sub {
			   $self->assert_warns(qr/foo/,
					       sub { warn "trivial message" },
					       "vital piece of info");
		       });

}


=head1 EXPORTABLE UTILITY SUBROUTINES

This module also provides subroutines which can be imported in the
usual way.  This is separate from the object or class method calling
style.

=head2 wantarray_type($val)

Simple tristate conversion to call as C< my $T =
wantarray_type(wantarray) > so you can print the callers expectation
neatly.

=cut

sub wantarray_type {
    die "expected one arg (the wantarray value), got @_" unless 1 == @_;
    my $W = shift; # the wantarray value to show
    return ($W
	    ? "list"
	    : (defined $W
	       ? "scalar"
	       : "void"));
}

sub McaTestCaseTest::test_wantarray_type {
    my $self = shift;

    my $T;
    my $num = 0;
    my $code = sub { $T = wantarray_type(wantarray); $num++ };

    $code->("foo");
    $self->assert_str_equals("void", $T);

    my $junk = $code->(1, 2, 3);
    $self->assert_str_equals("scalar", $T);

    my @junk = $code->();
    $self->assert_str_equals("list", $T);

    $self->assert_num_equals(3, $num);
}


1;



=head1 AUTHOR

Matthew Astley E<lt>mca@sanger.ac.ukE<gt> 2004-08ish, 2005-06 (D:L:O stuff)

=cut
