package McaTkTestCase;
# $Id$

use base 'McaTestCase'; # Test::Unit::TestCase with extra toys

sub tear_down {
    my $self = shift;
    my $P = $self->{__PACKAGE__."tkstuff"} ||= {};

    # Ensure again that windows are gone
    my @zapped;
    foreach my $win (@{ $P->{windows} }) {
	next unless Tk::Exists($win);
	push @zapped, $win;
	$win->destroy;
    }
    $self->assert_str_equals("()", "(@zapped)");

    # Last bail-out check
    $self->fail("Late bail-out check: $P->{bail}") if defined $P->{bail};

    $self->SUPER::tear_down;
}


# Cause test to fail later - for use in Tk callback.
# Organise the early destruction of the window
#
# Caller's responsibility to abort the current callback in a Tk-friendly way.
# (Tk->break seems to propagate outside the callback..?)
sub bail {
    my ($self, $prob) = @_;
    my $P = $self->{__PACKAGE__."tkstuff"} ||= {};

    if (!$P->{bail}) {
	# First bail
	$P->{bail} = $prob;
	# Direct destruction here upsets the callback mechanism..?
	foreach my $win (@{ $P->{windows} }) {
	    next unless Tk::Exists($win);
	    $win->afterIdle(sub { $win->destroy if Tk::Exists($win) })
	}
    } else {
	# More problems, just append
	$P->{bail} .= "\n".$prob;
    }

    $self->{annotate}->{bail} = $P->{bail} if $self->{annotate};
}


sub bail_check {
    my $self = shift;
    my $P = $self->{__PACKAGE__."tkstuff"} ||= {};

    my $probs = delete $P->{bail}; # clear for tear_down
    $self->fail("Did bail: $probs") if defined $probs;
}


# Register new windows and/or return current existing windows
# No need to remove, they just become invalid
sub tk_windows {
    my ($self, @new_wins) = @_;
    my $P = $self->{__PACKAGE__."tkstuff"} ||= {};
    push @{ $P->{windows} }, @new_wins;
    return @{ $P->{windows} };
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
    foreach my $key (@keysym) {
	foreach my $type (qw(<KeyPress> <KeyRelease>)) {
	    $win->eventGenerate($type, -keysym => $key, -when => 'tail');
	}
    }
}


1;
