/^--$START EXPANDABLE/,/^--$END EXPANDABLE/d
/^--$START FIXED/,/^--$END FIXED/d
/^--$START DYNAMIC/d
/^--$END DYNAMIC/d
1,$s/CTYPE/Dynamic/g
1,$s/REF_VAL/Node_Ptr/g
1,$s/NULL_REF/null/g
1,$s/REF_BASE/Node_Base_Class/g
1,$s/REF(\([a-zA-z.0-9]*\), *\([a-zA-z.0-9]*\))/\2/g
1,$s/REF(\([a-zA-z.0-9]*\), *\([a-zA-z.0-9]*\))/\2/g
