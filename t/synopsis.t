#!perl

use strict;
use warnings;

use Test::More tests => 8;
use Test::Differences;
use Color::Scheme;

my @names = qw( scheme1 scheme2 );

my $scheme1 = Color::Scheme->new->from_hex('ff0000')->scheme('analogic')
    ->distance(0.5)->add_complement(1)->variation('pastel')->web_safe(1);

my $scheme2
    = Color::Scheme->new->from_hue(0)->scheme('analogic')->distance(0.4)
    ->add_complement(1)->variation('pastel')->web_safe(1);

foreach my $scheme ( $scheme1, $scheme2 ) {
    my $name = shift @names;

    my @list = $scheme->colors();
    eq_or_diff(
        \@list,
        [   qw(
                ff6666 993333 ffcccc cc3333
                ff9966 996633 ffcccc cc6633
                cc6699 993366 ffcccc cc3399
                66cc66 339933 ccffcc 33cc33
                )
        ],
        "colors() for $name",
    );

    my $set = $scheme->colorset();
    eq_or_diff(
        $set,
        [   [qw( ff6666 993333 ffcccc cc3333)],
            [qw( ff9966 996633 ffcccc cc6633)],
            [qw( cc6699 993366 ffcccc cc3399)],
            [qw( 66cc66 339933 ccffcc 33cc33)],
        ],
        "colorset() for $name",
    );

    # the docs for colorset() shouldn't lie
    is( ( $scheme->colors )[1],
        $scheme->colorset->[0][1],
        "alternate usage one / $name"
    );
    is( ( $scheme->colors )[5],
        $scheme->colorset->[1][1],
        "alternate usage two / $name"
    );

}

