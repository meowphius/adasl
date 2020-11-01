/^--$START EXPANDABLE/,/^--$END EXPANDABLE/d
/^--$START DYNAMIC/,/^--$END DYNAMIC/d
/^--$START FIXED/d
/^--$END FIXED/d
1,$s/CTYPE/Fixed/g
1,$s/REF_VAL/Node_Ref/g
1,$s/NULL_REF/Null_Node/g
1,$s/REF_BASE(\([a-zA-z.0-9]*\))/\1/g
1,$s/AREF(\([a-zA-z.0-9]*\))/\1/g
1,$s/REF(\([a-zA-z.0-9]*\), *\([a-zA-z.0-9]*\))/\1.Data(\2)/g
1,$s/REF(\([a-zA-z.0-9]*\), *\([a-zA-z.0-9()]*\))/\1.Data(\2)/g
