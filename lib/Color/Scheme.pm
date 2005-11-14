package Color::Scheme;

use warnings;
use strict;

use Carp;
use List::Util qw(min max);
use POSIX qw(floor);

our $VERSION = '1.00';

=head1 NAME

Color::Scheme - Generate pleasing color schemes

=cut

my %SCHEMES = ( map { $_, 1 }
        qw( mono monochromatic contrast triade tetrade analogic ) );

my %PRESETS = (

    # name => [ ? ]
    default => [ -1,   -1,    1,   -0.7, 0.25, 1,   0.5,  1 ],
    pastel  => [ 0.5,  -0.9,  0.5, 0.5,  0.1,  0.9, 0.75, 0.75 ],
    soft    => [ 0.3,  -0.8,  0.3, 0.5,  0.1,  0.9, 0.5,  0.75 ],
    hard    => [ 1,    -1,    1,   -0.6, 0.1,  1,   0.6,  1 ],
    light   => [ 0.25, 1,     0.5, 0.75, 0.1,  1,   0.5,  1 ],
    pale    => [ 0.1,  -0.85, 0.1, 0.5,  0.1,  1,   0.1,  0.75 ],
);

my %COLOR_WHEEL = (

    # hue => [ red, green, blue, value ]
    0   => [ 255, 0,   0,   100 ],
    15  => [ 255, 51,  0,   100 ],
    30  => [ 255, 102, 0,   100 ],
    45  => [ 255, 128, 0,   100 ],
    60  => [ 255, 153, 0,   100 ],
    75  => [ 255, 178, 0,   100 ],
    90  => [ 255, 204, 0,   100 ],
    105 => [ 255, 229, 0,   100 ],
    120 => [ 255, 255, 0,   100 ],
    135 => [ 204, 255, 0,   100 ],
    150 => [ 153, 255, 0,   100 ],
    165 => [ 51,  255, 0,   100 ],
    180 => [ 0,   204, 0,   80 ],
    195 => [ 0,   178, 102, 70 ],
    210 => [ 0,   153, 153, 60 ],
    225 => [ 0,   102, 178, 70 ],
    240 => [ 0,   51,  204, 80 ],
    255 => [ 25,  25,  178, 70 ],
    270 => [ 51,  0,   153, 60 ],
    285 => [ 64,  0,   153, 60 ],
    300 => [ 102, 0,   153, 60 ],
    315 => [ 153, 0,   153, 60 ],
    330 => [ 204, 0,   153, 80 ],
    345 => [ 229, 0,   102, 90 ],
);

sub _round { floor( 0.5 + shift ) }

=head1 SYNOPSIS

    use Color::Scheme;

    my $scheme = Color::Scheme->new
        ->from_hex('ff0000') # or ->from_hue(0)
        ->scheme('analog')
        ->distance(0.3)
        ->add_complement(1)
        ->variation('pastel')
        ->web_safe(1)

    my @list = $scheme->colors();
    # @list = ( "999999","666699","ffffff","99cccc",
    #           "999999","666699","ffffff","9999cc",
    #           "669999","666699","ffffff","99cccc",
    #           "cccccc","996666","ffffff","cccc99" )

    my $set = $scheme->colorset();
    # $set  = [ [ "999999","666699","ffffff","99cccc", ],
    #           [ "999999","666699","ffffff","9999cc", ],
    #           [ "669999","666699","ffffff","99cccc", ],
    #           [ "cccccc","996666","ffffff","cccc99"  ] ]


=head1 DESCRIPTION

This module is a Perl implementation of Color Schemes
2 (L<http://wellstyled.com/tools/colorscheme2/>), a color scheme generator.
Start by visitng the Color Schemes 2 web site and playing with the colors.
When you want to generate those schemes on the fly, begin using this modoule.
The descriptions herein don't make too much sense without actually seeing the
colorful results.

Henceforth, paragraphs in quotes denote documentation copied from Color Schemes 2.

"B<Important note:> This tool B<doesn't use the standard HSV or HSB model> (the
same HSV/HSB values ie. in Photoshop describe different colors!). The color
wheel used here differs from the RGB spectre used on computer screens, it's
more in accordance with the classical color theory. This is also why some
colors (especially shades of blue) make less bright shades than the basic
colors of the RGB-model. In plus, the RGB-model uses red-green-blue as primary
colors, but the red-yellow-blue combination is used here. This deformation also
causes incompatibility in color conversions from RGB-values. Therefore, the RGB
input (eg. the HTML hex values like #F854A9) is not exact, the conversion is
rough and sometimes may produce slightly different color."

=head1 METHODS

=head2 new()

The C<new> method will return a new C<Color::Scheme> object.

=cut

sub new {
    my ( $class, @args ) = @_;
    carp __PACKAGE__ . "::new() doesn't take any arguments" if @args;

    my @colors;
    push @colors, Color::Scheme::mutablecolor->new(60) for 1 .. 4;

    return bless {
        col            => \@colors,
        scheme         => 'mono',
        distance       => 0.5,
        web_safe       => 0,
        add_complement => 0,
    }, $class;
}

=head2 colors()

Returns an array of 4, 8, 12 or 16 colors in C<RRGGBB> hexidecimal notation
(without a leading "#") depending on the color scheme and addComplement
parameter. For each set of four, the first is usually the most saturated color,
the second a darkened version, the third a pale version and fourth
a less-pale version. 

For example: With a contrast scheme, C<colors()> would return eight colors.
Indexes 1 and 5 could be background colors, 2 and 6 could be foreground colors.

Trust me, it's much better if you check out the Color Scheme web site, whose
URL is listed in in L<"DESCRIPTION">.

=cut

sub colors {
    my ($self)      = @_;
    my $used_colors = 1;
    my $h           = $self->{col}->[0]->get_hue;

    my %dispatch = (
        mono     => sub { },
        contrast => sub {
            $used_colors = 2;
            $self->{col}->[1]->set_hue($h);
            $self->{col}->[1]->rotate(180);
        },
        triade => sub {
            $used_colors = 3;
            my $dif = 60 * $self->{distance};
            $self->{col}->[1]->set_hue($h);
            $self->{col}->[1]->rotate( 180 - $dif );
            $self->{col}->[2]->set_hue($h);
            $self->{col}->[2]->rotate( 180 + $dif );
        },
        tetrade => sub {
            $used_colors = 4;
            my $dif = 90 * $self->{distance};
            $self->{col}->[1]->set_hue($h);
            $self->{col}->[1]->rotate(180);
            $self->{col}->[2]->set_hue($h);
            $self->{col}->[2]->rotate( 180 + $dif );
            $self->{col}->[3]->set_hue($h);
            $self->{col}->[3]->rotate($dif);
        },
        analogic => sub {
            $used_colors = $self->{add_complement} ? 4 : 3;
            my $dif = 60 * $self->{distance};
            $self->{col}->[1]->set_hue($h);
            $self->{col}->[1]->rotate($dif);
            $self->{col}->[2]->set_hue($h);
            $self->{col}->[2]->rotate( 360 - $dif );
            $self->{col}->[3]->set_hue($h);
            $self->{col}->[3]->rotate(180);
        },
    );
    $dispatch{monochromatic} = $dispatch{mono};

    if ( exists $dispatch{ $self->{scheme} } ) {
        $dispatch{ $self->{scheme} }->();
    }
    else {
        croak "unknown color scheme name: " . $self->{scheme};
    }

    my @output;
    for my $i ( 0 .. $used_colors - 1 ) {
        for my $j ( 0 .. 3 ) {
            $output[ $i * 4 + $j ]
                = $self->{col}->[$i]->get_hex( $self->{web_safe}, $j );
        }
    }
    return @output;
}

=head2 colorset()

Returns a list of lists of the colors in groups of four. This method simply
allows you to reference a color in the scheme by its group isntead of its
absolute index in the list of colors.  I am assuming that L<"colorset()">
will make it easier to use this module with the templating systems that are
out there.

For example, if you were to follow the synopsis, say you wanted to retrieve
the two darkest colors from the first two groups of the scheme, which is
typically the second color in the group. You could retrieve them with
L<"colors()">:

    my $first_background  = ($scheme->colors)[1];
    my $second_background = ($scheme->colors)[5];

Or, with this method,

    my $first_background  = $scheme->colorset->[0][1];
    my $second_background = $scheme->colorset->[1][1];

=cut

sub colorset {
    my ($self) = @_;
    my @flat_colors = $self->colors;
    my @grouped_colors;
    push @grouped_colors, [ splice @flat_colors, 0, 4 ] while @flat_colors;
    return \@grouped_colors;
}

=head2 add_complement( BOOLEAN )

If BOOLEAN is true, an extra set of colors will be produced using the
complement of the selected color. This is false by default.

This only works with the analogic color scheme.

=cut

sub add_complement {
    my ( $self, $b ) = @_;
    croak "add_complement needs an argument" unless defined $b;
    $self->{add_complement} = $b;
    return $self;
}

=head2 web_safe( BOOL )

Colors returned by L<"colors()"> or L<"colorset()"> will be web-safe. This is
fale by default.

=cut

sub web_safe {
    my ( $self, $b ) = @_;
    croak "web_safe needs an argument" unless defined $b;
    $self->{web_safe} = $b;
    return $self;
}

=head2 distance( FLOAT )

C<FLOAT> must be a value from 0 to 1. You might use
this with the L<"triade">, L<"tetrade"> or
L<"analogic"> color schemes.

=cut

sub distance {
    my ( $self, $d ) = @_;
    croak "distance needs an argument" unless defined $d;
    croak "distance($d) - argument must be >= 0" if $d < 0;
    croak "distance($d) - argument must be <= 1" if $d > 1;
    $self->{distance} = $d;
    return $self;
}

=head2 scheme( name )

C<name> must be a valid color scheme name. See L<"COLOR SCHEMES">.

=cut

sub scheme {
    my ( $self, $name ) = @_;
    croak "scheme needs an argument"          unless defined $name;
    croak "'$name' isn't a valid scheme name" unless exists $SCHEMES{$name};
    $self->{scheme} = $name;
    return $self;
}

=head2 variation( name )

C<name> must be a valid color variation name. See L<"COLOR VARIATIONS">.

=cut

sub variation {
    my ( $self, $v ) = @_;
    croak "variation needs an argument"       unless defined $v;
    croak "'$v' isn't a valid variation name" unless exists $PRESETS{$v};
    $self->_set_variant_preset( $PRESETS{$v} );
    return $self;
}

sub _set_variant_preset {
    my ( $self, $p ) = @_;
    $self->{col}->[$_]->set_variant_preset($p) for 0 .. 3;
}

=head2 from_hue( degrees )

Sets the base color hue, where C<degrees> is an integer from 0 to 359.

=cut

sub from_hue {
    my ( $self, $h ) = @_;
    croak "variation needs an argument" unless defined $h;
    croak "from_hue($h) - argument must be >= 0"   if $h < 0;
    croak "from_hue($h) - argument must be <= 359" if $h > 359;
    $self->{col}->[0]->set_hue($h);
    return $self;
}

=head2 from_hex( color )

Sets the base color to the given color, where C<color> is in the hexidecimal
form RRGGBB. C<color> should not be preceeded with a hash (#).

=cut

sub from_hex {
    my ( $self, $hex ) = @_;
    croak "from_hex needs an argument" unless defined $hex;
    croak "from_hex($hex) - argument must be in the form of RRGGBB"
        unless $hex =~ / ^ ( [0-9A-F]{2} ) {3} $ /ismx;

    $hex =~ m/(..)(..)(..)/;
    my ( $r, $g, $b ) = map {hex} ( $1, $2, $3 );

    my $rgb2hsv = sub {
        my ( $r, $g, $b ) = @_;

        my $min = min( $r, $g, $b );
        my $max = max( $r, $g, $b );
        my $d = $max - $min;
        my $v = $max;

        my $s =
              ( $d > 0 )
            ? ( $d / $max )
            : return ( 0, 0, $v );

        my $h =
              ( $r == $max ) ? ( ( $g - $b ) / $d )
            : ( $g == $max ) ? ( 2 + ( $b - $r ) / $d )
            : ( 4 + ( $r - $g ) / $d );
        $h *= 60;
        $h += 360 if $h < 0;

        return ( $h, $s, $v );
    };

    my @hsv = $rgb2hsv->( map { $_ / 255 } ( $r, $g, $b ) );
    my $h0  = $hsv[0];
    my $h1  = 0;
    my $h2  = 1000;
    my ( $i1, $i2, $h, $s, $v );

    foreach my $i ( sort keys %COLOR_WHEEL ) {
        my $c = $COLOR_WHEEL{$i};
        my @hsv1 = $rgb2hsv->( map { $_ / 255 } @$c[ 0 .. 2 ] );
        $h = $hsv1[0];
        if ( $h >= $h1 and $h <= $h0 ) {
            $h1 = $h;
            $i1 = $i;
        }
        if ( $h <= $h2 and $h >= $h0 ) {
            $h2 = $h;
            $i2 = $i;
        }
    }

    if ( $h2 == 0 or $h2 > 360 ) {
        $h2 = 360;
        $i2 = 360;
    }

    my $k = ( $h2 != $h1 ) ? ( $h0 - $h1 ) / ( $h2 - $h1 ) : 0;
    $h = _round( $i1 + $k * ( $i2 - $i1 ) );
    $h -= 360 if $h > 360;
    $h += 360 if $h < 0;
    $s = $hsv[1];
    $v = $hsv[2];

    $self->from_hue($h);
    $self->_set_variant_preset(
        [ $s, $v, $s, $v * 0.7, $s * 0.25, 1, $s * 0.5, 1 ] );

    return $self;
}

package Color::Scheme::mutablecolor;

use Carp;
use List::Util qw(min max);
use POSIX qw(floor);

sub _round { floor( 0.5 + shift ) }

sub new {
    my ( $class, $hue ) = @_;
    carp "no hue specified" unless defined $hue;
    my $self = bless {
        hue             => 0,
        saturation      => [],
        value           => [],
        base_red        => 0,
        base_green      => 0,
        base_blue       => 0,
        base_saturation => 0,
        base_value      => 0,
    }, $class;
    $self->set_hue($hue);
    $self->set_variant_preset( $PRESETS{default} );
    return $self;
}

sub rotate {
    my ( $self, $angle ) = @_;
    my $newhue = ( $self->{hue} + $angle ) % 360;
    $self->set_hue($newhue);
}

sub get_hue {
    my ($self) = @_;
    $self->{hue};
}

sub set_hue {
    my ( $self, $h ) = @_;

    my $avrg = sub {
        my ( $a, $b, $k ) = @_;
        return $a + _round( ( $b - $a ) * $k );
    };

    $self->{hue} = _round($h) % 360;
    my $d = $self->{hue} % 15 + ( $self->{hue} - floor( $self->{hue} ) );
    my $k = $d / 15;

    my $derivative1 = $self->{hue} - floor($d);
    my $derivative2 = ( $derivative1 + 15 ) % 360;
    my $colorset1   = $COLOR_WHEEL{$derivative1};
    my $colorset2   = $COLOR_WHEEL{$derivative2};

    my %enum = ( red => 0, green => 1, blue => 2, value => 3 );
    while ( my ( $color, $i ) = each %enum ) {
        $self->{"base_$color"}
            = $avrg->( $colorset1->[$i], $colorset2->[$i], $k );
    }
    $self->{base_saturation} = $avrg->( 100, 100, $k ) / 100;
    $self->{base_value} /= 100;
}

sub get_saturation {
    my ( $self, $variation ) = @_;
    my $x = $self->{saturation}->[$variation];
    my $s = $x < 0 ? -$x * $self->{base_saturation} : $x;
    $s = 1 if $s > 1;
    $s = 0 if $s < 0;
    return $s;
}

sub get_value {
    my ( $self, $variation ) = @_;
    my $x = $self->{value}->[$variation];
    my $v = $x < 0 ? -$x * $self->{base_value} : $x;
    $v = 1 if $v > 1;
    $v = 0 if $v < 0;
    return $v;
}

sub set_variant {
    my ( $self, $variation, $s, $v ) = @_;
    $self->{saturation}->[$variation] = $s;
    $self->{value}->[$variation]      = $v;
}

sub set_variant_preset {
    my ( $self, $p ) = @_;
    $self->set_variant( $_, $p->[ 2 * $_ ], $p->[ 2 * $_ + 1 ] ) for 0 .. 3;
}

sub get_hex {
    my ( $self, $web_safe, $variation ) = @_;

    my $max = max( map { $self->{"base_$_"} } qw( red green blue ) );
    my $min = min( map { $self->{"base_$_"} } qw( red green blue ) );

    my $v = (
        $variation < 0 ? $self->{base_value} : $self->get_value($variation) )
        * 255;
    my $s = (
          $variation < 0
        ? $self->{base_saturation}
        : $self->get_saturation($variation)
    );
    my $k = $max > 0 ? $v / $max : 0;

    my @rgb = map {
        min( 255, _round( $v - ( $v - $self->{"base_$_"} * $k ) * $s ) )
    } qw( red green blue );
    @rgb = map { _round( $_ / 51 ) * 51 } @rgb if $web_safe;

    return sprintf( '%02x' x @rgb, @rgb );
}

=head1 COLOR SCHEMES

The following documentation is adapated (and mostly copied verbatim) from the
Color Schemes 2 help.

=head2 monochromatic (or mono)

Monochormatic scheme is based on only one color tint, and uses only variations
made by changing its saturation and brightness. Black and white colors are
always added. The result is comfortable for eyes, even when using aggressive
color. However, it's harder to find accents and highlights.

The application makes only several monochromatic variants of each color. You'll
be able to make others - more or less saturated, lighter or darker.
Monochromatic variations are made for each color in other schemes, too. 

=head2 contrast

Base color is supplemented with its complement (color on the opposite side of
the wheel). One warm and one cold color is always created - we have to
consider, which one will be dominant, and if the result should look warm, or
cold. Suitable monochromatic variations of this two colors may be added to the
scheme. 

=head2 triade

Base color is supplemented with two colors, placed identically on both sides of
its complement. Unlike the "sharp" contrast, this scheme is often more
comfortable for the eyes, it's softer, and has more space for balancing warm
and cold colors.

You can use the L<"distance"> method to set the distance of these colors
from the base color complement. The less the value is, the closer the colors
are to the contrast color, and are more similar. The best value is between 0.25
and 0.5. Higher values aren't too suitable - except the shift by 60°, which
makes another color scheme, the triade:

The triade is made by three colors evenly distributed on the thirds of the
color wheel (by 120 degrees). The triade-schemes are vibrating, full of energy,
and have large space to make contrasts, accents and to balance warm and cold
colors. You can make the triade in the "soft contrast" scheme setting the
distance to the maximal value, 1.

=head2 tetrade

This scheme, also known as "double-contrast," is made by a pair of colors and
their complements. It's based on the tetrade - the foursome of colors evenly
distributed on the fourths of the color wheel (by 90 degreees). The tetrade is
very aggressive color scheme, requiring very good planning and very sensitive
approach to relations of these colors.

Less distance between two base colors causes less tension in the result.
However, this scheme is always more "nervous" and "action" than other schemes.
While working with it, we have to take care especially of relations between one
color and the complement of its adjacent color - in case of the tetrade
(maximum distance 1), good feeling and very sensitive approach are necessary.

=head2 analogic

This scheme is made by base color and its adjacent colors - two colors
identically on both sides. It always looks very elegantly and clear, the result
has less tension and it's uniformly warm, or cold. If a color on the warm-cold
border is chosen, the color with opposite "temperature" may be used for
accenting the other two colors.

You can set the distance of adjacent colors by using L<"setDistance">.  Values
between 0.25 and 0.5 (15-30 degrees on the wheel) are optimal. You can also add
the contrast color; the scheme is then supplemented with the complement of the
base color. It must be treated only as a complement - it adds tension to the
palette, and it's too aggressive when overused. However, used in details and as
accent of main colors, it can be very effective and elegant.

=head1 AUTHOR

Color Schemes 2 and documentation are copyright pixy L<http://www.pixy.cz/>

This JSAN module was created by Ian Langworth <ian.langworth@gmail.com>

=head1 COPYRIGHT

This product is distributed under the Creative Commons BY-NC-SA licence.
License for commercial use is not possible.

=cut

1;
