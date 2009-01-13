package Test::Unit::Assert::DataRefs;

# Interleaved test methods appear in the Test::Unit::Assert::SelfTest
# package.  You can add it to your suite or not.
#   package Test::Unit::Assert::SelfTest;
#   our @ISA = qw(McaTestCase);

use strict;
use warnings;


=head1 NAME

Test::Unit::Assert::DataRefs - assertions for testing data

=head1 SYNOPSIS

 use base qw( Test::Unit::TestCase Test::Unit::Assert::DataRefs );

=head1 DESCRIPTION

This is styled as a mixin class, so it mostly provides a bunch of
extra object methods.

=cut

#----------------------------------------------------------------------------


## Mixin class should NOT provide set_up and tear_down, else other
## callers will have to mess with NEXT.


=head2 Extra C<assert_*> methods

These can be used in the usual way.

=over 4

=item C<assert_samerefs([$expect_object1, $expect_object2], [$actual_object1, $actual_object2 ], $descr)>

First two args must be lists, items in the list are compared with
'=='.  Purpose is for checking a bunch of references point to the
expected places.

C<$descr> will be set to the calling file:line unless a description is
given.  Descriptions are prepended to "expected foo, got spong"-style
failure messages.

=item C<assert_isa($obj, @isa)>

Assert that C<$obj> is an object and is in B<all> of the classes
C<@isa>.

=back

=cut


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

sub McaTestCaseTest::test_assert_samerefs {
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

warn 'assert_isa($obj, @isa) here now falls back to framework\'s assert_isa($obj, $class[, $message])';
sub assert_isa_many {
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

sub McaTestCaseTest::test_assert_isa {
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


1;



=head1 AUTHOR

 Copyright (c) 2004, 2005 Genome Research Ltd.
 Author: Matthew Astley E<lt>mca@sanger.ac.ukE<gt>

This file is part of perlunit-extras.

perlunit-extras  is  free software;  you  can  redistribute it  and/or
modify  it  under the  terms  of the  GNU  General  Public License  as
published by  the Free  Software Foundation; either  version 2  of the
License, or (at your option) any later version.

This program  is distributed in the  hope that it will  be useful, but
WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
General Public License for more details.

You  should have received  a copy  of the  GNU General  Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

=cut
