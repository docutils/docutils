do {
    chomp (my $pwd = `pwd`);
    $DEFS{include1} = "include1.txt";
    $DEFS{include1rel} = relative_path($pwd, $DEFS{include1});
    $DEFS{include2} = "include2.txt";
    $DEFS{include3} = "include3.txt";
    $DEFS{include8} = "include8.txt";
    $DEFS{include10} = "include10.txt";
    $DEFS{include10rel} = relative_path($pwd, $DEFS{include10});
    $DEFS{include11} = "include 11.txt";
    $DEFS{include11rel} = relative_path($pwd, $DEFS{include11});
    $DEFS{utf_16_file} = "utf-16.csv";
    $DEFS{utf_16_file_rel} = relative_path($pwd, $DEFS{utf_16_file});
    $DEFS{nonexistent} = "nonexistent";
    $DEFS{nonexistent_rel} = relative_path($pwd, $DEFS{nonexistent});
};

sub relative_path {
    my($base, $file) = @_;
#    $file =~ s/^$base/./;
    return "$file";
    return "./$file";
}
1;
