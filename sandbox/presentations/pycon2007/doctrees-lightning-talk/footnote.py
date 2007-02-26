def visit_footnote(self, node):

    # ... lots of complicated code snipped ...
    #         self.output.append(...)
 
    if self.settings.footnote_backlinks and backrefs:
        if len(backrefs) == 1:
            self.stack.append('')
            self.stack.append('</a>')
            self.stack.append('<a class="fn-backref" href="#%s">'
                                % backrefs[0])
        else:
            i = 1
            for backref in backrefs:
                backlinks.append('<a class="fn-backref" href="#%s">%s</a>'
                                 % (backref, i))
                i += 1
            self.stack.append('<em>(%s)</em> ' % ', '.join(backlinks))
            self.stack += ['', '']
    else:
        self.stack.append('')
        self.stack += ['', '']

    # ... lots of code snipped ...


def depart_footnote(self, node):
    self.output.append('</td></tr>\n</tbody>\n</table>\n')
    # No stack.pop here!







