#! /usr/local/bin/perl -wc

package McaTestCase;
# $Id$

use strict;
use base 'Test::Unit::TestCase';

use Data::Dumper;
use B 'svref_2object';
use Scalar::Util 'weaken';

=head1 NAME

McaTestCase - souped up Test::Unit::TestCase subclass

=head1 SYNOPSIS

 use base 'McaTestCase';
 sub test_foo {
   $self->{annotate} = { debug => "stuff", more => "stuff" };
   $self->assert_dies( qr/dodo/, sub { die "dodo-style" } );
 }

=head1 DESCRIPTION

This is a mixed bag of stuff collected together to help write test
cases.  I use it as a base class, it inherits in turn from
L<Test::Unit::TestCase> .

It includes its own testsuite in each of yours.  This is false
laziness on my part, the testsuite should be elsewhere.

=head2 Dump debug data when needed

During C<tear_down>, will annotate the test with the L<Data::Dumper>
processed dump of the contents of C<$self-E<gt>{annotate}> , if
present.  This data will then be seen iff the test fails - handy for
debugging.

=head2 Tattle on slow tests

Times each test method with the wallclock (L<Time::HiRes> when
available), prints summary of slow tests during C<END> block.  Slow is
defined as taking more than 3x the average wall-clock time of all the
tests.  It's designed to keep quiet unless there are hogs to report
on.

=cut


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

    if (0) {
	$self->objleak_reset
	  if Devel::Leak::Object->can('get_status');
    }
}

sub tear_down {
    my $self = shift;

    # Display data structures from testcase, if we fail
    if ($self->{annotate}) {
	$self->fail("annotate value exists but is not hashref") if ref($self->{annotate}) ne 'HASH';
	$self->_dump_order_add($self->{annotate}); # ensure it will come out sorted, but don't mess with any existing order
	$self->annotate($self->datadump([ $self->{annotate} ], ['*annotate']));
    }

    $self->SUPER::tear_down;

    my $t = $self->global_test_name;
    $test_times{$t}->[1] = time2();

    if (0) {
	my @leak_ignore = qw(DBI::st B::Deparse SqlEngine2::Exception);
	# SqlEngine2::Exception stores last-seen exception per package
	# Test::Unit::Assertion::CodeRef caches one B::Deparse
	# SqlEngine2 caches many DBI::st

	my $leaked = ($self->{leak_before}
		      ? $self->objleak_check('tear_down', @leak_ignore)
		      : 0);
	$self->fail("Leaked object count = $leaked") if $leaked; # XXX: will mask other failures
    }
}

sub objleak_reset {
    my $self = shift;
    $self->{leak_before} = [ Devel::Leak::Object::get_status() ];
    delete $self->{leak_significant};
}

sub _weak_get_refcnt {
    my $self = shift;
    my $h = $self->{weaklist};
    return unless $h;
    while (my ($name, $list) = each %$h) {
	if (ref($list->[1])) {
	    my $bsv = svref_2object($list->[1]);
	    $list->[0] = ref($bsv)." refcnt=".$bsv->REFCNT;
	} elsif (defined $list->[1]) {
	    $list->[0] = 'not a ref';
	} else {
	    $list->[0] = 'went away';
	}
    }
}

sub _dump_order_add {
    my ($self, $hashref, @keys) = @_;
    my $orders = $self->{_dumper_order} ||= {}; # key = hashref, value = list of hash keys
    my $keylist = $orders->{$hashref} ||= [];
    push @$keylist, @keys;
}

sub objleak_significant {
    my ($self, @pairs) = @_;
    my $h = $self->{annotate} ||= {};
    my $leak = $self->{leak_significant} ||= {};
    my $weaks = $self->{weaklist} ||= {};
    while (my ($name, $value) = splice @pairs, 0, 2) {
	$self->_dump_order_add($h, $name) unless exists $h->{$name};
	my $list = $h->{$name} = [ undef, $value ];
	$weaks->{$list} = $list;
	$leak->{$list} = $list;
	weaken $list->[1];
    }
    return $h;
}

sub dumpnote_weak {
    my ($self, @pairs) = @_;
    my $h = $self->{annotate} ||= {};
    my $weaks = $self->{weaklist} ||= {};
    while (my ($name, $value) = splice @pairs, 0, 2) {
	$self->_dump_order_add($h, $name) unless exists $h->{$name};
	my $list = $h->{$name} = [ undef, $value ];
	$weaks->{$list} = $list;
	weaken $list->[1];
    }
    0;
}

sub dumpnote {
    my ($self, @pairs) = @_;
    my $h = $self->{annotate} ||= {};
    while (my ($name, $value) = splice @pairs, 0, 2) {
	$self->_dump_order_add($h, $name) unless exists $h->{$name};
	$h->{$name} = $value;
    }
}

sub datadump {
    my ($self, $vals, $keys) = @_;

    my $d = Data::Dumper->new($vals, $keys);
    $d->Indent(1)->Quotekeys(0);
    $d->Sortkeys(sub {
		     my $hashref = shift;
		     if ($self->{_dumper_order}->{$hashref}) {
			 # Special treatment - order may be important
			 my @keys = @{ $self->{_dumper_order}->{$hashref} };

			 # Check we have them all
			 my %keys;
			 @keys{(keys %$hashref)} = ();
			 delete @keys{@keys};
			 push @keys, sort keys %keys;

			 return \@keys;
		     }
		     return [ keys %$hashref ];
		 });

    $self->_weak_get_refcnt;

#    $d->Seen($self->{_dumper_seen}) if $self->{_dumper_seen};
    my $dump = $d->Dump;

## doesn't work?
#    my %seen = $d->Seen;
#    $self->{_dumper_seen} = \%seen;
## because some items come back ($k, $v) and others come back ($k, $v, 1); all in a flat list

    return $dump;
}


##### somewhat broken / unused...
#####
##
##

sub objleak_check {
    my ($self, $when, @ignore) = @_;

    $_ = %_ = @_ = (); # just in case...  no evidence yet that it had been a problem

#    { # Prod the PEs' contents
#	my %seen = Devel::Leak::Object::get_seen();
#	foreach my $pe (@{ $seen{'GT::Physical::Entity'} }) {
#	    for my $i (0 .. 100) {
#		PRJ::GTx->singleton->getStatus(-pe => $pe);
#	    }
##	    $pe->{_EntireEntity} = $pe->{_SubEntities} = 'ZAPPED'; # lots of stuff should go Away, but doesn't
#	}
#    }

    # Subtract out what we had before the test
    my %leak_before = @{ $self->{leak_before} };
    my %leak_after = Devel::Leak::Object::get_status();
    foreach my $k (keys %leak_after) {
	$leak_after{$k} -= $leak_before{$k} if defined $leak_before{$k};
	delete $leak_after{$k} if $leak_after{$k} == 0;
    }

    delete @leak_after{@ignore};
    my $significant = $self->{leak_significant};
    my $remain = grep { defined } values %$significant;

    return 0 if 0 == $remain && 0 == scalar keys %leak_after;

    my $tot = $remain;
    my $t = $self->global_test_name;
    my $msg = "  objleak_check($when) for $t:\n";

    # nb. this lists all outstanding objects of the class, not just the ones we're interested in
    my %seen = Devel::Leak::Object::get_seen();
    foreach my $seen (keys %seen) {
	for (my $i=0; $i<@{$seen{$seen}}; $i++) {
	    weaken($seen{$seen}[$i]); # don't count the refs we're griping about
	}
    }

    for my $class (sort keys %leak_after) {
	$tot += $leak_after{$class};
	$msg .= sprintf "    %-40s %d\n", $class, $leak_after{$class};
	my $objlist = $seen{$class};
	for (my $i=0; $i < @$objlist; $i++) {
	    my ($type, $count) = Devel::Leak::Object::get_refinfo($$objlist[$i]);
	    $msg .= sprintf("      %-38s %s refcnt=%d\n", $$objlist[$i], $type, $count);
	}
    }
    $self->annotate($msg);


#    if ($self->{FC_made}) {
#	warn "\n\n";
#	warn ">> Kept item\n";
#	Dump($self->{FC_made});
#	warn ">> PEs\n";
#	Dump($seen{'GT::Physical::Entity'});
#	warn "\n\n";
#    }

#    my $symdump = Devel::Symdump->rnew; # recursive package dump, from main

    # Dump out "significant" objects, if they remain; then leaked objects
    # Rely on Data::Dumper doing the backreference thing to make reading easier
    $self->annotate("    Seen objects:\n"
		    .$self->datadump
		    ([ $significant,
		       $seen{'GT::Physical::Entity'},
		       \%seen,
		     ],
		     [qw(*SIGNIFICANT PROBLEM_REFS seen)]));

    return $tot;
}

##
##
#####
##### somewhat broken / unused...


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


=head2 Extra C<assert_*> methods

These can be used in the usual way.

=over 4

=item C<assert_dies( qr/message regex/, @coderefs, $descr])>

Run each piece of code.  Assert that each one dies, and that the
resulting error message matches the regex.  Extra description
C<$descr> is optional.

=item C<assert_samerefs([$expect_object1, $expect_object2], [$actual_object1, $actual_object2 ], $descr)>

First two args must be lists, items in the list are compared with
'=='.  Purpose is for checking a bunch of references point to the
expected places.

C<$descr> will be set to the calling file:line unless a description is
given.  Descriptions are prepended to "expected foo, got spong"-style
failure messages.

=item C<assert_is_idnum($database_id, $descr)>

C<$database_id> must be a plain non-negative integer.

=back

=head2 Self-tests

Includes tests of its new C<assert_*> methods, these will piggyback on
each of your test classes.  If this turns out to be a major time
waster then I'll move them.

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
}


# Compare two lists of refs (or numbers) for '==' equality
sub assert_samerefs {
    my ($self, $expect_list, $actual_list, $descr, @more) = @_;
    my @c = caller();
    $descr = "$c[1]:$c[2]" unless defined $descr;

    $self->assert_str_equals(0, scalar @more, "$descr: too many args");
    $self->assert_str_equals('ARRAY', ref($expect_list), "$descr: bad expect");
    $self->assert_str_equals('ARRAY', ref($actual_list), "$descr: bad actual");
    $self->fail("$descr: compared to itself") if $actual_list == $expect_list;
    my $en = scalar @$expect_list;
    my $an = scalar @$actual_list;
    $self->assert_str_equals($en, $an, "$descr: expect $en, got $an");
    for (my $i=0; $i<$en; $i++) {
	my $e = $expect_list->[$i];
	my $a = $actual_list->[$i];
	$self->fail("$descr: Items at #$i ($e, $a) not same object")
	  unless $a == $e;
    }
}

sub test_baseselftest_assert_samerefs {
    my $self = shift;
    my @o = ( 34, [ 78 ], { foo => 'bar' } );

    $self->assert_samerefs([ @o ], [ @o ]);
    $self->assert_samerefs([ @o ], [ @o ], "with message");

    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_samerefs( \@o, \@o ); # compare list to itself - weird
			 });

    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_samerefs( \@o, [ ] ); # lists wrong size
			 });

    $self->assert_raises('Test::Unit::Failure',
			 sub {
			     $self->assert_samerefs( \@o, [ 34, [78], $o[2] ] ); # [78] is different
			 });
    $self->assert_samerefs( \@o, [ 34, $o[1], $o[2] ] ); # [78] is different
}


sub assert_is_idnum {
    my ($self, $id, $descr, @more) = @_;
    my @c = caller();
    $descr = "$c[1]:$c[2]" unless defined $descr;

    $self->assert_str_equals(0, scalar @more, "$descr: too many args");
    $self->assert_not_null($id, "$descr: Was null");
    $self->assert_str_equals("", ref($id), "$descr: '$id' is a ref");
    $self->fail("$descr: '$id' is a ref") if ref($id);
    $self->assert_matches(qr/^\d+$/, $id, "$descr: '$id' contains non-numeric");
}

sub test_baseselftest_assert_is_idnum {
    my $self = shift;

    foreach my $id ("0", "634535345345", 5 .. 10) {
	$self->assert_is_idnum($id);
    }

    foreach my $not_id (undef, "eek", "", "_", "5.0", -5) {
	$self->assert_raises('Test::Unit::Failure', sub {
	    $self->assert_is_idnum($not_id);
	});
    }

    $self->assert_dies(qr/descrstring/, sub { $self->assert_is_idnum("wibble", "descrstring") });
    $self->assert_dies(qr/too many args/, sub { $self->assert_is_idnum(1, 2, 3) });
}


sub assert_isa {
    my ($self, $obj, @isa) = @_;
    my @c = caller();
    my $descr = "$c[1]:$c[2]";

    # Check it's a ref
    my $what = (defined $obj
		? (ref($obj) ? ref($obj) : "non-ref")
		: "undef");

    for (my $i=0; $i<@isa; $i++) {
	$self->fail("$descr: $what is not a $isa[$i] (isa #$i)") unless UNIVERSAL::isa($obj, $isa[$i]);
    }
}

sub test_assert_isa {
    my $self = shift;
    $self->assert_raises('Test::Unit::Failure', sub {
			     $self->assert_isa(undef, "Foo");
			 }, "Cope with undef");
    $self->assert_raises('Test::Unit::Failure', sub {
			     $self->assert_isa(56, "Foo");
			 }, "Cope with 56");
    $self->assert_raises('Test::Unit::Failure', sub {
			     $self->assert_isa({}, "Foo");
			 }, "Cope with HASH");

    $self->assert_isa($self, qw(McaTestCase Test::Unit::Assert Test::Unit::Test Test::Unit::TestCase));

    $self->assert_raises('Test::Unit::Failure', sub {
			     $self->assert_isa($self, qw(Test::Unit::Test Gronk Test::Unit::TestCase));
			 }, "Cope with wrong class");
}


=head2 Hack-around for v0.24 C<Test::Unit::Assert::is_numeric> problem

Redefines the subroutine to be more strict.  Hopefully a temporary
measure.

This fixes the problem described at
http://sourceforge.net/tracker/index.php?func=detail&aid=1014540&group_id=2653&atid=102653
(perlunit.sf.net bug number 1014540)

=cut

BEGIN {
    warn __PACKAGE__, ": Sick bodge over Test::Unit v0.24\n";
    no warnings;
    sub Test::Unit::Assert::is_numeric {
	my $str = shift;
	local $^W;
	return defined $str && ! ($str == 0 && $str !~ /^\s*[+-]?0(e0)?\s*$/i);
    }
}


1;



=head1 AUTHOR

Matthew Astley E<lt>mca@sanger.ac.ukE<gt> 2004-08ish

=cut
