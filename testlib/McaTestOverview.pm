package McaTestOverview;
use strict;
use Pod::Checker 1.2; # 1.2 with Perl 5.6; 1.4 with Perl 5.8
use base 'Test::Unit::TestCase';

=head1 NAME

McaTestOverview - generic tests to run on a project

=head1 DESCRIPTION

Runs various tests on a set of files defined in C<set_up>.  Currently
these are "whatever is loaded but not in C</usr/>.  Clearly this will
need changing for many people.

=over 4

=cut

sub set_up {
    my $self = shift;
    $self->SUPER::set_up;
    $self->{sources} = [ get_loaded_files() ];
}



=item test_notes_to_self

Look for lines containing reminder comments in source files, while
being careful not to needlessly mention the strings involved in the
test source.

Search patterns defined include B<X>B<XX:> and the string B<T>B<ODO:>,
in various combinations of colon, word boundary and case sensitivity.

=cut

sub test_notes_to_self {
    my $self = shift;
    my @filenames = @{ $self->{sources} };

    my $marks = 0;
    my @pats = ( qr/\b[x]xx:/i,
		 qr/\b[T]ODO\b|(?i)\b[t]odo[-:]/,
	       );
    foreach my $filename (@filenames) {
	my @marklines = ( grep { my $line = $_;
				 grep { $line =~ $_ } @pats
			     }
			  get_source($filename)
			);
	foreach (@marklines) { s/^\s*#?\s*// } # strip leading dullness
	local $" = "  "; # lines have \n already
	if (@marklines) {
	    $self->annotate("In $filename:\n  @marklines\n");
	    $marks += @marklines;
	}
    }
#    $self->annotate(join "\n  ", "----\nFiles scanned:", @filenames);

    $self->fail("Found $marks note(s) to self") if $marks;
}


=item test_podchecker

Runs all files through a modified (silent) L<Pod::Checker> and reports
one failure for any/all warnings, POD absence or error.

The little lexical below (in the code) allows files with certain names
(currently testcases in my naming scheme) to contain no POD without
this being an error.

This could be extended or replaced by checking the file for package
names and seeing whether these are subclasses of
L<Test::Unit::TestCase>, but that's a bit fancy.

=cut

my $podless_ok_filename = qr{^testlib/[\w/]+[a-z]Test.pm$};

sub test_podchecker {
    my $self = shift;
    my @filenames = @{ $self->{sources} };

    my $tot_warns = 0;
    my $tot_errs = 0;
    foreach my $filename (@filenames) {
	# Make the checker.  Initialise with full filename because you
	# can't pass filenames with parse_from_filehandle
	my $checker = My::Test::Unit::PodChecker->new( -warnings => 2,
						       -testcase => $self,
						       _INFILE => $filename);

	# Line up the text and make a sink for the "no errors found" noise
	my $in_fh = My::GetlineSource->new(get_source($filename));
	#DNW: my $in_fh = My::PrintSink->open();
	#     print $in_fh get_source($filename);
	my $out_fh = My::PrintSink->open();
## fallback for unices:
#	open my $out_fh, ">", "/dev/null" or die "Can't open sink to /dev/null: $!";

	# Do the check, count the problems
	$checker->parse_from_filehandle($in_fh, $out_fh);
	my ($e, $w) = ($checker->num_errors, $checker->get_num_warnings);

## The sunk output is available but mostly not interesting
#	$self->annotate(map {"   : $_"} <$out_fh>);

	# Is it reasonable to allow POD-free test cases?  For now, I think yes.
	if ($e < 0) {
	    $e = 0 if $filename =~ $podless_ok_filename;
	}

	if ($e < 0) {
	    $self->annotate("E:$filename: No POD found\n\n");
	} else {
	    my @probs = $checker->takeprob;
	    local $" = "  "; # lines have \n already
	    $self->annotate("  @probs\n")
	      if $e || $w;
	}

	$tot_warns += $w;
	$tot_errs += abs($e); # -1 indicates no POD
    }
    $self->fail("Found $tot_errs error(s) and $tot_warns warning(s)") if $tot_errs || $tot_warns;
}


=back

=head2 Utility methods

=over 4

=item get_source($filename)

Loads, caches and returns a copy of the whole file.  Works in list and
scalar contexts.

This is mainly to avoid loading files multiple times for various
tests.

NB. Currently B<does no timestamp checks>, so you may get a stale
version if you call it after the file changed.

=cut

my %file_cache;
sub get_source {
    my $filename = shift;
    if (!exists $file_cache{$filename}) {
	open SOURCE, "<$filename" or die "Failed to read source $filename: $!";
	$file_cache{$filename} = [ <SOURCE> ];
	close SOURCE;
    }
    my $f = $file_cache{$filename};
    return wantarray ? @$f : join "", @$f;
}


=item get_loaded_files

As called from C<set_up>.  Not a very clever bit of code.

=cut

sub get_loaded_files {
    die unless wantarray;
    sort grep {!m{^/usr/}} values %INC
}

=back

=cut

1;


##############################################################################
#
# Hacking about to get a silent Pod::Checker that records the warning
# count

package My::Test::Unit::PodChecker;
use base 'Pod::Checker';

# Invoked as $self->poderror( @args ), or $self->poderror( {%opts}, @args )
#
#  poderr: opts=(-line, 271, -msg, node 'http://sourceforge.net/tracker/index.php?func=detail&aid=1014540&group_id=2653&atid=102653' contains non-escaped | or /, -severity, WARNING, -file, testlib/McaTestCase.pm), args=()
#  poderr: opts=(-line, 293, -msg, 2 unescaped <> in paragraph, -severity, WARNING, -file, testlib/McaTestCase.pm), args=()
sub poderror {
    my $self = shift;
    my %opts = (ref $_[0]) ? %{shift()} : ();

    die "Override code doesn't expect \@_ args" if @_; # would these be part of the -msg ?

    my $type = $opts{-severity} eq 'ERROR' ? 'ERROR' : 'WARNING';
    $self->{"_NUM_${type}S"}++;
    my $t = substr($type, 0, 1); # W | E

    my $file = $opts{-file} || "[unknown file]";
    my $line = $opts{-line} || "??";
    my $msg  = $opts{-msg} || "[message lost]";

    $self->addprob("$t:$file:$line: $msg\n");

#    warn " poderr: opts=(".(join ", ", %opts)."), args=(@_)\n";
}

# num_warnings accessor is in Pod:::Checker 1.4, but not in 1.2
sub get_num_warnings { $_[0]->{_NUM_WARNINGS} || 0 }

sub addprob {
    my $self = shift;
    push @{ $self->{problist} }, @_;
}

sub takeprob {
    my $self = shift;
    my $probs = delete $self->{problist} || [];
    return @$probs;
}

1;


# This is just an ARRAY we can use as a filehandle
# because the POD reader has provision for this method call
package My::GetlineSource;

sub new { bless [ @_ ], __PACKAGE__ }

sub getline { shift @{ $_[0] } }


# Handle which can do a limited set of I/O operations on whole lines
package My::PrintSink;
sub open {
    my $proto = shift;
    my $class = ref($proto) || $proto;

#    local *JUNK;
# puzzled about globs .. localising this seems to cause the locally
# tied handle to be lost and the restored glob contents to be used as
# an unopened file...  so I'm probably reusing the same sink several
# times
    our @JUNK;
    my $list = bless \@JUNK, $class;

    tie *JUNK, $class, $list;
    return \*JUNK;
}
sub TIEHANDLE {
    my ($class, $list) = @_;
    return $list;
}
sub PRINT {
    my $list = shift; # the blessed ARRAYref
    push @$list, @_;
}
sub PRINTF {
    my $list = shift;
    my $format = shift;
    my $txt = sprintf($format, @_); # first arg has scalar context!
    $list->PRINT($txt);
}
sub READLINE {
    my $list = shift;
    return wantarray ? splice @$list : shift @$list;
}
