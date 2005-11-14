#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Color::Scheme' );
}

diag( "Testing Color::Scheme $Color::Scheme::VERSION, Perl $], $^X" );
