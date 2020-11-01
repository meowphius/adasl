{
  if (match($2, "s$") == 0) {
    type = "Body";
  } else {
    type = "Spec";
  }
  gsub("\.ad[sb]", "", $2);
  name = "";
  end = match($2, "[_\-]");
  while (end != 0) {
    bc = substr($2, 1, 1);
    ec = substr($2, 2, end-1);
    $2 = substr($2, end+1);
    name = name toupper(bc) ec
    end = match($2, "[_\-]");
  }
  bc = substr($2, 1, 1);
  ec = substr($2, 2);
  name = name toupper(bc) ec
  gsub("-", ".", name);

  printf("pragma Source_File_Name (%s,\n", name);
  printf("                         %s_File_Name => \"%s\");\n", type, $1)
}
