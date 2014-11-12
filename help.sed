s/\\/\\\\/g
s/"/\"/g
1s/^/module Help ( text ) where\ntext = "/
1!s/^/\\/
$s/$/\\n"/
$!s/$/\\n\\/