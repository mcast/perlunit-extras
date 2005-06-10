
# Interleaved test methods go in this package, you can add it to your
# suite or not.
package McaTestCaseTest;
our @ISA = qw(McaTestCase);

package McaTestCase;

use strict;
use base 'Test::Unit::TestCase';

use Data::Dumper;
use B 'svref_2object';
use Scalar::Util qw(weaken isweak);


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

=head2 Use Devel::Leak::Object, if loaded

Lots of stuff here.

=head2 Checking the libs were loaded from consistent places

This needs to be configured by the test case, else it does nothing.

If inconsistencies are detected, the C<set_up> will fail.

=cut

# $Id$

#----------------------------------------------------------------------------


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

    $self->DLO_reset(1) if $self->DLO_present;
    $self->libscheck_do();
}

sub tear_down {
    my $self = shift;

    $self->SUPER::tear_down;

    my $leaked = ($self->{DLO_before}
		  ? $self->DLO_check
		  : 0);
    $self->DLO_reset(0); # drop the lists - nobody needs them, they're untidy on the output

    # Display data structures from testcase, if we fail
    if ($self->{annotate} && scalar keys %{ $self->{annotate} }) {
	$self->fail("annotate value exists but is not hashref") if ref($self->{annotate}) ne 'HASH';
	$self->_dump_order_add($self->{annotate}); # ensure it will come out sorted, but don't mess with any existing order
	$self->annotate($self->datadump([ $self->{annotate} ], ['*annotate']));
    }

    my $t = $self->global_test_name;
    $test_times{$t}->[1] = time2();

    $self->fail("Leaked object count = $leaked\nXXX: this will mask other failures, may prevent later parts of tear_down") if $leaked;
}

sub DLO_present { Devel::Leak::Object->can('get_seen') }

sub DLO_reset {
    my ($self, $gather) = @_;
    delete $self->{DLO_before};
    delete $self->{leak_significant};
    delete $self->{weaklist};
    $self->{DLO_before} = Devel::Leak::Object::get_seen() if $gather;
}

sub DLO_ignore_class {
    return qw(B::Deparse);
}


=head2 DLO_leak_annotate()

Returns a boolean.  Override to 1 to dump the laggards.

=cut

sub DLO_leak_annotate { 0 }

=head2 DLO_GV_ignorecheck($obj)

Returns list of refs which should be excluded from the graph.  Is
passed the ref currently about to be added.

Method ignores parts of the DLO accounting, members of
C<DLO_GV_nofollow_class>
XXX: and some hardwired stuff for GT:PE's.

=cut

sub DLO_GV_ignorecheck {
    my ($self, $obj) = @_;

    return $obj if grep { UNIVERSAL::isa($obj, $_) } $self->DLO_GV_nofollow_class;

    if (UNIVERSAL::isa($obj, __PACKAGE__)) {
	return ($obj->{DLO_before});
    }
    if (UNIVERSAL::isa($obj, 'Class::Abstract::Container::Bean')) {
	return ($obj->{_KeyFunc})
    }

    return ();
}

sub _DLO_GV_seeig {
    my ($self, $obj, $seen_ignore) = @_;
    foreach my $ig ($self->DLO_GV_ignorecheck($obj)) {
	next unless ref($ig);
	$seen_ignore->{ (Devel::Leak::Object::obj_info($ig))[2] } = 1;
    }
}


=head2 DLO_GV_nofollow_class()

Returns a list of classes whose instances (including subclasses)
should be ignored by the default C<DLO_GV_ignorecheck>.

=cut

sub DLO_GV_nofollow_class {qw{
 Test::Unit::Listener B::Deparse
 DBI::dr DBI::db DBI::st DBI::var Errno
 Class::Property SqlEngine2
 }}


=head2 DLO_GV_getname($obj)

Called once per object, if the object hasn't already been given some
sort of name.  The default is to return C<undef> - an anonymous
object.

=cut

sub DLO_GV_getname {
    my ($self, $obj) = @_;
    if (grep { UNIVERSAL::isa($obj, $_) }
	qw(GT::Physical::Entity GT::Physical::Entity::Contents)) {
	return $obj->getId;
    }
    return undef;
}

sub DLO_check {
    my $self = shift;
    $_ = %_ = @_ = (); # just in case...  no evidence yet that it had been a problem

    # Subtract 'before' objects from 'after' ones, build data structure
    my $before = $self->{DLO_before};
    my $after = Devel::Leak::Object::get_seen();
    my %after; # key = address, value = [ before-index, after-index, after_class, after_refcnt, reftype ]
    for my $outer (0, 1) {
	my $list = ($before, $after)[$outer];
	for (my $i=0; $i<@$list; $i++) {
	    next unless defined $list->[$i];
	    # The 'before' list could have holes in it from where
	    # objects went Away during our run  XXX: could track these by keeping strong refs

	    my ($class,$type,$addr,$refcnt) = Devel::Leak::Object::obj_info($list->[$i]);
	    my $place = $after{$addr} ||= [];
	    $place->[$outer] = $i;
	    if ($outer) {
		$place->[2] = $class;
		$place->[3] = $refcnt;
		$place->[4] = $type;
	    }
	}
    }

    my %ignore_class = map {($_ => 1)} $self->DLO_ignore_class;
    my (@leak, @persist);
    foreach my $addr (keys %after) {
	my ($bef, $aft, $class, $refcnt, $reftype) = @{ $after{$addr} };
	# Could detect class change if we wanted to...  would need extra info
#	next if $ignore_class{$class};
	if (defined $bef) {
	    push @persist, $addr;
	} else {
	    push @leak, $addr;
	}
    }

#    $self->annotate("Objects persisting from before DLO_reset: @persist\n") if @persist;
# doesn't tell us much
    return 0 if !@leak;


##########
#
# Graphviz refcount dumper

    my %seen; # key = addr, value = [ total-refcnt, name, type, leakstate, {edgeset}, colour ]
    # type is ARRAY etc. or classname
    # leakstate in (notblessed, leak, sigleak, persist, darkmatter); name often undef
    # darkmatter: a blessed or unknown object which was not detected as leaked or persisting
    # edgeset: key = index, value = [ addr, isweak ]
    my @unseen; # objects pending a seeing
    my %refs_found; # key = addr, value = found-refcnt
    my %seen_ignore; # key = addr, value = true if we ignore the addr
    my $see = sub {
	my ($obj, $addr, $refcnt, $name, $class, $reftype, $leakstate, $clr) = @_;
	$self->_DLO_GV_seeig($obj, \%seen_ignore);
	return if $seen_ignore{$addr} || $seen{$addr};
	die "\$see called on undef object" unless defined $obj;
	$class = "" if !defined $class;
	$name = $self->DLO_GV_getname($obj) if !defined $name;
	my %edgeset;
	$seen{$addr} = [ $refcnt, $name, $class || $reftype, $leakstate, \%edgeset, $clr || 'black' ];
	# collect refs inside $obj
	my %scan;
	if ($reftype eq 'SCALAR') {
	    $scan{""} = [isweak($$obj), $$obj] if ref($$obj);
	} elsif ($reftype eq 'ARRAY') {
	    for (my $i=0; $i<@$obj; $i++) {
		next unless ref($obj->[$i]);
		$scan{$i} = [isweak($obj->[$i]), $obj->[$i]];
	    }
	} elsif ($reftype eq 'HASH') {
	    foreach my $key (keys %$obj) {
		next unless ref($obj->{$key});
		$scan{$key} = [isweak($obj->{$key}), $obj->{$key}];
	    }
	} elsif ($reftype eq 'CODE' ||
		 $reftype eq 'GLOB' ||
		 $reftype eq 'Regexp') {
	    # XXX: have to ignore code refs; may contain significant refs in the closure
	} else {
	    warn "Can't deal with reftype=$reftype class=$class, will ignore\n";
	}
	# make joins, collect unseen for later
	while (my ($k, $v) = each %scan) {
	    my ($isweak, $to) = @$v;
	    my (undef, undef, $to_addr, undef) = Devel::Leak::Object::obj_info($to);
	    $self->_DLO_GV_seeig($to, \%seen_ignore);
	    next if $seen_ignore{$to_addr};
	    $edgeset{$k} = [ $to_addr, $isweak ];
	    $refs_found{$to_addr} ++ unless $isweak;
	    if (!$seen{$to_addr}) { # don't re-see self
		push @unseen, $to;
		weaken $unseen[-1];
	    }
	}
    };
    my $getsee = sub {
	my (undef, $name, $leakstate) = @_;
	# $obj is $_[0], but avoid making another ref to it
	return if !defined $_[0];
	my ($class, $reftype, $addr, $refcnt) = Devel::Leak::Object::obj_info($_[0]);
	$leakstate ||= ($class ? 'darkmatter' : 'notblessed');
	$see->($_[0], $addr, $refcnt, $name, $class, $reftype, $leakstate);
    };
    # Prime with things we know about
    $getsee->($self->{annotate}, 'annotate');
    $getsee->($self, 'TESTCASE', 'persist');
    for (my $i=0; $i<@leak; $i++) {
	my $addr = $leak[$i];
	my ($bef, $aft, $class, $refcnt, $reftype) = @{ $after{$addr} };
	my $obj = $after->[$aft];
	$see->($obj, $addr, $refcnt, undef, $class, $reftype, 'leak');
    }
    for (my $i=0; $i<@persist; $i++) {
	my $addr = $persist[$i];
	my ($bef, $aft, $class, $refcnt, $reftype) = @{ $after{$addr} };
	my $obj = $after->[$aft];
	$see->($obj, $addr, $refcnt, undef, $class, $reftype, 'persist');
    }
    # Trundle through attached things
    while (@unseen) {
	$getsee->(shift @unseen);
    }
    # Invent darkmatter to account for missing refcnt; ignore some classes
    foreach my $addr (keys %seen) { # enumerate _before_ we start adding more
	my $type = $seen{$addr}->[2];
	my ($actual, $expect) = ($refs_found{$addr} || 0, $seen{$addr}->[0]);
	next if $expect == $actual;
	$seen{$addr}->[5] = ($actual > $expect ? 'green' : 'red');
	for (my $i=0; $i<($expect-$actual); $i++) {
	    my $invis_addr = "${addr}unk$i";
	    $see->( [], $invis_addr, 1, '??', '', 'ARRAY', 'darkmatter', 'blue');
	    $seen{$invis_addr}->[4]->{""} = [ $addr, 0 ]; # join from darkmatter to $addr
	}
    }


## dot -Tps -o $ENV{HOME}/tmp/leakdump.{ps,dotty} && ggv /tmp/leakdump.ps
    my $dotFn = "$ENV{HOME}/tmp/leakdump.dotty";
    open GV, ">$dotFn" or die "Can't write dotty file $dotFn: $!";
    # XXX: Hardcoded output path
    print GV <<HDR;
/* leakdump: @{[ $self->global_test_name, scalar localtime ]} */
digraph leakdump {
  rotate = 90;
  page = "8.26,11.68";     /* A4 landscape */
  size = "11.15,7.73";     /* multiply up for multi-page */
  margin = "0.25,0.25";
  ratio = "0.70719";       /* remove to allow non-filling, or for multi-paging */
HDR
    # XXX: Hardcoded classname abbreviation scheme, should be configurable
    my %classmap = ('PRJ::FoosPE::Expand' => 'wc',
		    'PRJ::FoosPE::Foo' => 'ms',       'PRJ::FoosPE::BoxedFoo' => 'pm',
		    'PRJ::FoosPE::GroupGroup' => 'lg', 'PRJ::FoosPE::JoinPair' => 'mp',
		    'GT::Physical::Entity' => 'PE',
		    'GT::Physical::Entity::Contents' => 'PE:C',
		    'GT::Physical::Entity::Contents::Container' => 'PE:Cc',
		    'GT::Physical::Entity::Contents::Attribute' => 'PE:A',
		    'GT::Physical::Entity::Contents::Info' => 'PE:I',
		    'GT::Physical::Entity::Contents::Info::Container' => 'PE:Ic',
		   );
    # XXX: Hardcoded shape (and colour, elsewhere) scheme. No "key" graph generation.
    my %shapemap = (SCALAR => 'diamond', ARRAY => 'parallelogram', HASH => 'trapezium',
		    CODE => 'triangle', GLOB => 'triangle', Regexp => 'invtriangle',
		    leak => 'house', sigleak => 'invhouse', persist => 'box',
		    darkmatter => 'hexagon');
    my @edges; # list of [ from_node, to_node, isweak, label ]
    foreach my $addr (sort keys %seen) {
	my ($refcnt, $name, $type, $leakstate, $edgeset, $clr) = @{ $seen{$addr} };
	my $shape = $shapemap{$leakstate} || $shapemap{$type};
	warn "No shape for leakstate=$leakstate, type=$type\n" unless defined $shape;
	my @lbl = ($leakstate eq 'notblessed' ? "" : $classmap{$type} || $type);
	# push @lbl, $addr;
	push @lbl, $name if $name;
	@lbl = ('??') if $name && $name eq '??'; # an unknown
	my $lbl = join "\\n", @lbl;
	(my $node = $addr) =~ s/^0x/x/;
	print GV qq{$node \[ label="$lbl", shape=$shape, color=$clr ]; /* refcnt=$refcnt */\n};
	while (my ($k, $v) = each %$edgeset) {
	    my ($to_addr, $isweak) = @$v;
	    next unless exists $seen{$to_addr}; # skip edges to ignored nodes
	    (my $to_node = $to_addr) =~ s/^0x/x/;
	    push @edges, [ $node, $to_node, $isweak, $k ];
	}
    }
    foreach my $e (@edges) {
	my ($from, $to, $isweak, $edge) = @$e;
	my $style = $isweak ? 'dotted' : 'solid';
	my @label = $edge;
	while (length($label[-1]) > 20) {
	    splice @label, -1, 0, substr($label[-1], 0, 15, "");
	}
	my $label = join "\\n", @label;
	print GV qq{$from -> $to \[ style=$style, label="$label" ];\n};
    }
    print GV "}\n";
    close GV;

# Graphviz refcount dumper
#
##########

    # Generate annotation text from leak addresses
    my @leak_descr;
    for (my $i=0; $i<@leak; $i++) {
	# Reformat the @leak list with detail
	my $addr = $leak[$i];
	my ($bef, $aft, $class, $refcnt, $reftype) = @{ $after{$addr} };
	next if $ignore_class{$class};
	my $obj = $after->[$aft];
	$self->dumpnote_weak("DLO_leak_$addr" => $obj) if $self->DLO_leak_annotate;
	push @leak_descr, sprintf("  %-16s %s refcnt=%d\n", $class, $addr, $refcnt);
    }
    $self->annotate(join "", " Leaks detected in DLO_check\n", sort @leak_descr);

    return scalar @leak_descr;
}

sub _weak_update_refcnt {
    my $self = shift;
    my $L = $self->{weaklist};
    return unless $L;
    foreach my $list (@$L) {
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
    my $weaks = $self->{weaklist} ||= [];
    while (my ($name, $value) = splice @pairs, 0, 2) {
	$self->_dump_order_add($h, $name) unless exists $h->{$name};
	my $list = $h->{$name} = [ undef, $value ];
	push @$weaks, $list;
	$leak->{$list} = $list;
	weaken $list->[1];
    }
    return $h;
}

sub dumpnote_weak {
    my ($self, @pairs) = @_;
    my $h = $self->{annotate} ||= {};
    my $weaks = $self->{weaklist} ||= [];
    while (my ($name, $value) = splice @pairs, 0, 2) {
	$self->_dump_order_add($h, $name) unless exists $h->{$name};
	my $list = $h->{$name} = [ undef, $value ];
	push @$weaks, $list;
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

    $self->_weak_update_refcnt;

#    $d->Seen($self->{_dumper_seen}) if $self->{_dumper_seen};
    my $dump = $d->Dump;

## doesn't work?
#    my %seen = $d->Seen;
#    $self->{_dumper_seen} = \%seen;
## because some items come back ($k, $v) and others come back ($k, $v, 1); all in a flat list

    return $dump;
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
	    push @timings, "$t\tdata missing (tear_down breakage || set_up died)\n";
	}
    }
    print( "-" x 70,
	   "\nSlow test info (threshold ",
	   sprintf("%3.1fs)\n", $time_lim),
	   @timings)
      if @timings;
}

sub libscheck_do {
    my $self = shift;
    my (@prob, @info);
    foreach my $group (sort $self->libscheck_get) {
	my %source;
	my $common;    # e.g.  /home/me/projdir/
	my %uncommon;  # e.g.  lib/ testlib/ [keys only, no values]
	foreach my $incmod (grep m{^$group(/|\.pm$)}, keys %INC) {
	    my $src = $INC{$incmod};
	    my $len = length($incmod);

	    # This allows two packages in one file, provided the extra
	    # package doesn't explicitly add itself to %INC .

	    # It doesn't allow packages declaring themselves to have
	    # been included from a file which isn't named
	    # appropriately.

	    if (substr($src, -$len, $len) ne $incmod) {
		push @prob, "Can't match sourcefile $src to module \$INC{$incmod}";
		$source{$incmod} = $src;
		next;
	    }
	    substr($src, -$len, $len) = "";
	    $source{$incmod} = $src;

	    # Detect common path prefixes in the group
	    if (defined $common) {
		my $tmp = $common;
		while (substr($src, 0, length($tmp)) ne $tmp) {
		    # prefixes differ, shrink the prefix by one char
		    substr($tmp, -1, 1) = "";
		}
		my $lost_common = substr($common, length($tmp));
		if ($lost_common ne "") {
		    # Update existing uncommon
		    my @uncommon = keys %uncommon;
		    %uncommon = ();
		    $uncommon{$lost_common.$_} = undef foreach @uncommon;
		}
		$common = $tmp;
	    } else {
		$common = $src;
	    }
	    $uncommon{substr($src, length($common))} = undef;
	}
	push @info, (" For package group $group\n".
		     join "", (map {sprintf("  %-40s %s\n", $_, $source{$_})}
			       sort keys %source));
	my @uncommon = keys %uncommon;
	push @info, "  > common prefix = $common\n  > uncommon suffixes = (@uncommon)\n\n";
	# Allow certain uncommon path elements
	delete @uncommon{qw{ lib/ testlib/ }};
	delete $uncommon{""};
	# Complain if anything remains
	push @prob, (join "\n  ",
		     "Cannot reconcile these paths within package group $group",
		     # original paths containing modules
		     sort map {$common.$_} @uncommon)
	  if scalar keys %uncommon;
    }

    # warn "\n\n@info\n";
    if (@prob) {
	my $msg = join "\n", @prob;
	$self->fail("libscheck_do failure as above")
	  unless $self->_libscheck_newfailp($msg);
	$self->annotate(join "", "libscheck_do info:\n", @info);
	$self->fail($msg);
    }
}

{
    # Idempotent setup via class method
    # Could be made specific to the calling test class, but isn't
    my %pkgroots; # key = regexp fragment for package group; no value
    sub libscheck_group { shift; @pkgroots{join "|", @_} = () } # XXX: regexp escape the class prefixes?
    sub libscheck_get { return keys %pkgroots }
    # Cut out duplicate long messages
    my %fails;
    sub _libscheck_newfailp {
	my ($self, $msg) = @_;
	my $new = !exists $fails{$msg};
	$fails{$msg} = 1;
	return $new;
    }
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

Includes tests of its new C<assert_*> methods, inline in the file but
in a separate package so they don't piggyback on C<McaTestCase>
subclasses.

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

sub McaTestCaseTest::test_assert_is_idnum {
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

Matthew Astley E<lt>mca@sanger.ac.ukE<gt> 2004-08ish, 2005-06 (D:L:O stuff)

=cut
