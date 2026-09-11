mvn spring-boot:run 2>&1 | jq -Rr --unbuffered '
  . as $raw |
  (try fromjson catch null) as $j |

  if ($j | type) == "object" then

    ($j.level // $j.type // $j.severity // "UNKNOWN"
      | tostring
      | ascii_upcase
    ) as $level |

    (
      if   $level == "ERROR" then "\u001b[1;31m"
      elif $level == "WARN"  then "\u001b[1;33m"
      elif $level == "WARNING" then "\u001b[1;33m"
      elif $level == "INFO"  then "\u001b[32m"
      elif $level == "DEBUG" then "\u001b[36m"
      elif $level == "TRACE" then "\u001b[90m"
      else "\u001b[37m"
      end
    ) as $color |

    "\u001b[2m\($j.time // $j.timestamp // "")\u001b[0m " +
    $color + "\($level)\u001b[0m " +
    "\u001b[34m\($j.logger // $j.logger_name // "")\u001b[0m " +
    "\($j.message // $j.msg // $j.message_template // "")"

  else
    $raw
  end
'