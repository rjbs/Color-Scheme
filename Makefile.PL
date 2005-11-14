use strict;
use warnings;
use ExtUtils::MakeMaker;

my %config = (
    NAME          => 'Color::Scheme',
    AUTHOR        => 'Ian Langworth <ian@cpan.org>',
    VERSION_FROM  => 'lib/Color/Scheme.pm',
    ABSTRACT_FROM => 'lib/Color/Scheme.pm',
    PREREQ_PM     => {
        'List::Util'        => '1.14',
        'POSIX'             => '1.08',
        'Test::More'        => '0.54',
        'Test::Differences' => '0.47',
    },
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES    => 'Color-Scheme-*' },
);

WriteMakefile(%config);

