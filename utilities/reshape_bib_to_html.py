#!/usr/bin/python

from pybtex.database.input import bibtex
import sys
parser = bibtex.Parser()
bib_data = parser.parse_file(sys.argv[1])
st = '<tr> <th> <font color="#8bc6f0"><b><a href="{url}">[link]</a></b></font> {fullnames} ({year}).  <font color="#5789ad">{title}.</font> <i>{journal}</i>. {number}, {pages}.</th> </tr>'

def main():
    """
    USAGE: python reshape_bib_to_html.py test.bib
    reads the entry in the bib file and parse them as HTML enries
    """
    for bib_id in bib_data.entries:
        b = bib_data.entries[bib_id].fields
        au = ""
        if "number" not in b:
            b["number"] = "In press"
            b["pages"] = ""
        for author in bib_data.entries[bib_id].persons["author"]:
            au = au + author.last()[0] + "," + author.first()[0][0] + "., "
        # print(au)
        b["fullnames"] = au[:-3]
        print(st.format(**b))

if __name__ == '__main__': sys.exit(main())

