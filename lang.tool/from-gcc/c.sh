sed -e "/^@@ifobjc$/,/^@@end_ifobjc$/d" \
    -e "/^@@ifc$/d" -e "/^@@end_ifc$/d" \
    c-parse.in > c-parse.tmp
ed c-parse.tmp < c.ed
gosh c.scm < c-parse.tmp > c-parse.y
ed c-parse.y < cb.ed
gosh cm.scm < c-parse.y > c-parse.tmp
ed c-parse.tmp < cs.ed
gosh cr.scm < c-parse.tmp > c-parse.scm-for-edit
#ed c-parse.scm-for-edit < cz.ed
rm -f c-parse.tmp





