#!/usr/bin/env bash

jq -RrC --unbuffered '. as $raw | try fromjson catch $raw' |
perl -pe '
  BEGIN { $| = 1 }
  if (/"(?:type|level|severity)"/) {
    s/\e\[0;32m"(error)"\e\[0m/\e[1;31m"$1"\e[0m/ig;
    s/\e\[0;32m"(warn(?:ing)?)"\e\[0m/\e[1;33m"$1"\e[0m/ig;
    s/\e\[0;32m"(info)"\e\[0m/\e[1;32m"$1"\e[0m/ig;
    s/\e\[0;32m"(debug)"\e\[0m/\e[36m"$1"\e[0m/ig;
    s/\e\[0;32m"(trace)"\e\[0m/\e[90m"$1"\e[0m/ig;
  }
'
