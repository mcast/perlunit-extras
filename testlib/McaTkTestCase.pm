package McaTkTestCase;
# $Id$

use base 'McaTestCase'; # Test::Unit::TestCase with extra toys

=head1 NAME

McaTkTestCase - Test::Unit::TestCase with support for Tk::MainWindow-driven test

=cut

# Private accessor for our data stash
my $get_priv = sub {
    return $_[0]->{__PACKAGE__."tkstuff"} ||= {};
};

 ### not implemented .. sleeping with update not needed yet
 #
 # # Accessor (one arg get, two arg set) for time delays config
 # # Defaults to zero, convention suggests milliseconds
 # sub slowdown {
 #     my ($self, $what, $delay) = @_;
 #     my $S = $get_priv->($self)->{slowdown};
 #     $S->{$what} = $delay if defined $delay;
 #     return $S->{$what} || 0;
 # }

sub tear_down {
    my $self = shift;
    my $P = $get_priv->($self);

    # Ensure again that windows are gone
    my @zapped;
    foreach my $win ($self->tk_windows) {
	push @zapped, $win;
	$win->destroy;
    }
    $self->assert_str_equals("()", "(@zapped)");

    # Last bail-out check
    $self->bail_check("**LATE** Bail-out check during tear_down");

    $self->SUPER::tear_down;
}


# Cause test to fail later - for use in Tk callback, where you can't usefully call $self->fail
# Organise the early destruction of the window
#
# Caller's responsibility to abort the current callback in a Tk-friendly way.
# (Tk->break seems to propagate outside the callback..?)
sub bail {
    my ($self, $prob) = @_;
    my $P = $get_priv->($self);

    if (!$P->{bail}) {
	# First bail
	$P->{bail} = $prob;
	# Direct destruction here upsets the callback mechanism..?
	foreach my $win ($self->tk_windows) {
	    $win->afterIdle(sub { $win->destroy if Tk::Exists($win) })
	}
    } else {
	# More problems, just append
	$P->{bail} .= "\n".$prob;
    }

    $self->{annotate}->{bail} = $P->{bail} if $self->{annotate};
}

# Fail the test if we had previously called bail.
# Test should call this before finishing, but tear_down calls it too.
sub bail_check {
    my ($self, $descr) = @_;
    $descr ||= "Did bail";
    my $P = $get_priv->($self);

    my $probs = delete $P->{bail}; # clear for tear_down
    $self->fail("$descr: $probs") if defined $probs;
}


# Register new windows and/or return current existing windows
# No need to remove, they just become invalid (non-Exist()ent)
sub tk_windows {
    my ($self, @new_wins) = @_;
    my $P = $get_priv->($self);
    push @{ $P->{windows} }, @new_wins;
    return grep { $_->Exists } @{ $P->{windows} };
}



# Conversion wrapper on tk_pokekeysym
sub tk_pokestring {
    my ($self, $win, $str) = @_;
    my @keysym;
    foreach my $c (split //, $str) {
	if ($c eq "\n") {
	    push @keysym, "Return";
	} else {
	    warn sprintf("Not sure how to convert chr(0x%02X) to keysym", ord($c))
	      unless $c =~ /[ -z]/;
	    push @keysym, $c;
	}
    }
    return $self->tk_pokekeysym($win, @keysym);
}

# Put keystrokes on the tail of the event queue (Check focus first!)
# @keysym of the form qw(m a g i c Return)
sub tk_pokekeysym {
    my ($self, $win, @keysym) = @_;
#    my $delay = $self->slowdown('key');
    foreach my $key (@keysym) {
	foreach my $type (qw(<KeyPress> <KeyRelease>)) {
	    $win->eventGenerate($type, -keysym => $key, -when => 'tail');
	}
#	$self->delay($win, $delay);
#	$win->after($delay) if $delay;
    }
}


1;
