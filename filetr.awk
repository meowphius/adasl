BEGIN { FS = "-" }
/asgc-.*\.ads$/{
  newname = "ag" substr($2, 0, 2);
  for (i=3; i<=NF; i++) {
    newname = newname substr($i, 0, 1);
  }
  if (index($0, "_managed.ad") != 0) {
    newname = newname "m";
  }
  printf("%s.ads %s\n", newname, $0);
}
/asgc-.*\.adb$/{
  newname = "ag" substr($2, 0, 2);
  for (i=3; i<=NF; i++) {
    newname = newname substr($i, 0, 1);
  }
  if (index($0, "_managed.ad") != 0) {
    newname = newname "m";
  }
  printf("%s.adb %s\n", newname, $0);
}

/asoc-.*\.ads$/{
  newname = "ao" substr($2, 0, 2);
  for (i=3; i<=NF; i++) {
    newname = newname substr($i, 0, 1);
  }
  if (index($0, "_managed.ad") != 0) {
    newname = newname "m";
  }
  printf("%s.ads %s\n", newname, $0);
}

/asoc-.*\.adb$/{
  newname = "ao" substr($2, 0, 2);
  for (i=3; i<=NF; i++) {
    newname = newname substr($i, 0, 1);
  }
  if (index($0, "_managed.ad") != 0) {
    newname = newname "m";
  }
  printf("%s.adb %s\n", newname, $0);
}

{ }
