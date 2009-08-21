package CheckPragmas;

use strict;
use warnings;

use B::Deparse;

use base 'McaTestCase';


=head1 NAME

CheckPragmas - testcase finds code compiled without strict + warnings


=head1 DESCRIPTION

Trundles the symbol table for subroutines.  Deparses them.  Collects
lists of

=over 4

=item *

failures to C<use strict> and C<use warnings>.

=item *

use of C<goto> which is not the C<goto &sub> form.

=item *

use of the dreaded dollar-left-square-bracket variable.

=back


=head1 BUGS, CAVEATS, MISSING FEATURES

This is a proof of concept / first cut abandoned after proving itself
useful.  Things are hardwired.

Suffers L<B::Deparse>s limitations, particularly it can't see the
status of C<use strict 'vars', 'subs'>

The structure is untidy.  The code walker (and its various exceptions)
should be separated out for re-use by checkers of POD,
are-all-our-test-classes-loaded etc..

Multiple complaints stem from single offending subroutines (e.g. sub
compiled without warnings, then exported into another module).

Local definition of which code is "local".  The rest is ignored.

Local definition of which modules compile offending code and inject it
into unsuspecting modules.

Debug / diagnostic triggers linger, awaiting uncommenting.

Doesn't give a per-module percentage offense, though the information
is available.

Doesn't detect assignment (without the protection of C<local>) to $"
$/ and friends.


=head1 AUTHOR

 Copyright (c) 2009 Genome Research Ltd.
 Author: Matthew Astley <mca@sanger.ac.uk>

=cut


# XXX: Get some code compiled
{
    use Finishing::FinPrimers;

    ### Trick a module into doing its AUTOLOAD magic
    #
    # Make a cheap instance which can run a method without segfaulting
    # in the XS
    { package Junk; sub get_attribute { "foo" } }
    my $inst = bless {}, "Junk";
    #
    # Call it
    my $ret = TraceServer::Group::get_type_description($inst);
    #warn "$inst --> $ret\n";
}

# XXX: fodder for the goto detectorm belongs in selftest
sub junk1 {
  sinbin:
    goto sinbin;
}
sub junk2 {
    goto ("FOO", "BAR", "GLARCH")[shift];
}
sub junk3 {
    goto &junk2;
}
sub junk4 {
    my @foo = (1..4);
    no warnings;
    local $[ = 4;
    print $foo[3];
}


# We don't use the variable, but need its name as a string.  Obfuscate
# to avoid false alarm.
my $OFFSET_VAR = '$'."[";

sub set_up {
    my ($self) = @_;

    $$self{deparse} = B::Deparse->new("-sC");
    $$self{deparse}->ambient_pragmas(strict => 'none',
				     # warnings => [] # doesn't work? is the default
				     $OFFSET_VAR => 0);

    return $self->SUPER::set_up;
}


sub test_modules {
    my ($self) = @_;

    my @skip_code = $self->_skip_subs;

    # Enumerate loaded modules
    my %prob;
    while (my ($modpath, $fn) = each %INC) {
	my $mod = $modpath;
	$mod =~ s{/}{::}g;
	$mod =~ s/\.pm$//;

next if $fn =~ m{^/software/perl-}; # XXX: the local universal Perl install
next if $fn =~ m{/cpan-pfx/}; # XXX: my devel CPAN install

	# Enumerate subroutines in module
	my %code = $self->_getSubs($mod);
	while (my ($sub, $code) = each %code) {
	    next if grep { $_ == $code } @skip_code;
	    my ($where, $prob) = $self->_problem($mod, $sub, $code);
	    push @{ $prob{$where} }, "$sub \[$prob]" if $prob;
	}
    }
    if (keys %prob) {
	my @prob = map {"$_\n\t@{$prob{$_}}"} sort keys %prob;
	$self->fail(join "\n  ", "Pragma check failed in modules", @prob);
    }
}


# Return CODErefs of subroutines imported from standard places we know
# don't comply
sub _skip_subs {
    my ($self) = @_;

    my @try = split /\s+/, <<SYMS;
Readonly::Readonly
Scalar::Util::blessed
Scalar::Util::reftype
File::Temp::tempfile
SYMS

    my @out;
    foreach my $sym (@try) {
	my $ref = do {
	    no strict 'refs';
	    *{$sym}{CODE};
	};
#	warn "$sym skip: ".($ref || "NO")."\n";
	push @out, $ref if $ref;
    }

    return @out;
}


# Return true if $name "Module::sub" is compile-time generated (so
# CODEref may be unpredictable), and doesn't comply
sub _mask_sub {
    my ($self, $name) = @_;

    return ($name =~ m{^Carp(::Clan)?::(carp|cluck|confess|croak)$} ||
	    $name =~ m{^(Data::Dumper::Dumper|Exporter::import)$} ||
	    $name =~ m{^(overload|AutoLoader|DynaLoader|Time::localtime)::} ||
	    $name =~ m{^(DBIx::Class|Class::Accessor|Class::C3)::});
}


sub _getSubs {
    my ($self, $mod) = @_;
    my $scan = do {
	no strict 'refs';
	*{"$mod\::"};
    };
    my @sym = keys %$scan;

    my %code;
    foreach my $sym (@sym) {
	my $code = do {
	    no strict 'refs';
	    *{"$mod\::$sym"}{CODE};
	};
	$code{$sym} = $code if $code;
    }
    return %code;
}


sub _problem {
    my ($self, $mod, $sub, $code) = @_;

    my $src = $$self{deparse}->coderef2text($code);

    # Skip certain subs created & poked (instead of aliased) at
    # compile time
    my $LABEL = qr{(?:\w+:\s*)?};
    my ($pkg) = ($src =~ /^\s*${LABEL}package (\S+);$/m); # first only
    return () if $pkg && $self->_mask_sub("$pkg\::$sub");

    # Skip constants
    return () if $src =~ m{^\(\) \{ \{?[^{};]*\}? \}$};

    # Skip XS (?)
    return () if $src =~ m{^(\([^()]*\))?\s*;$};

    # Detect problems
    my @prob;
    push @prob, "W" unless $src =~ /^\s*use warnings\b/m;
    push @prob, "S" unless $src =~ /^\s*use strict\b/m;
    push @prob, "G!"    if $src =~ /\bgoto\s*(?!\s*\\?&)/; # fiddly!
#   push @prob, "G&"    if $src =~ /\bgoto\s*\\?&/; # goto sub "less stigma"
    push @prob, $OFFSET_VAR if $src =~ /\$\[/;

### uncommentables to stash the deparsing of some items
#
#    $$self{annotate}{$mod}{"&$sub"} = $src if
#      @prob ||
#      $mod eq 'CheckPragmas' ||
#	$sub =~ /^(junk.|AUTOLOAD|\(==)$/;
#      "@prob" =~ /W S|G/;

    if (@prob) {

	my $comp;
	if (!defined $pkg) {
	    $comp = "!pkg";
	} elsif ($pkg eq $mod) {
	    $comp = "";
	} else {
	    $comp = "pkg=$pkg";
	}

	my @out = $comp ? ("$mod/$comp", "@prob") : ($mod, "@prob");

#	push @prob, $comp if $comp ne "";
#	$$self{annotate}{$mod}{$sub} = "@prob";

	return @out;
    } else {
	return ();
    }
}


1;
