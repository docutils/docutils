====================================
Docutils website menu + HTML5 writer
====================================

* ``docs/_template/``
  
  * ``web.css``: custom css for website
  * ``template.html``: HTML template for site
    
* Update documentation for new build command::

    ./buildhtml.py \
    --stylesheet-path=../docs/_template/web.css,../docutils/writers/html_base/html-base.css \
    --template=../docs/_template/template.html ..
* ``tools/buildhtml.py`` updated to use ``html_base`` and to use Günter
  Milde's HTML5 writer by default.
