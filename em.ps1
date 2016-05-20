param (
    [switch]$wait
)

if ($wait) {
    $nflag = ""
} else {
    $nflag = "-n"
}

emacsclient $nflag -c -a '""' $args
