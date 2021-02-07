EXAMPLES = \
  $(addsuffix .svg, $(wildcard examples/*.scm)) \
  $(addsuffix .svg, $(wildcard examples/*.w))

SLIDES=\
  00-title \
  map \
  pipeline1 \
  pipeline1a \
  pipeline1b \
  hpc \
  pipeline2 \
  reproducible-1 \
  11-environment \
  12-environment2 \
  reproducible-2 \
  reproducible-3 \
  13-environment-graph \
  14-containers \
  15-container-smoothie \
  giraffe \
  convergent \
  30-guix \
  31-guix2 \
  31-guix3 \
  31-guix4 \
  31-guix5 \
  31-guix6 \
  31-guix7 \
  32-gwl \
  33-gwl1 \
  34-gwl6 \
  pipe \
  $(EXAMPLES) \
  35-gwl-free-features \
  99-end

frames: $(addsuffix .png, ${SLIDES})
	identify $^

slides.pdf: $(addsuffix .pdf, ${SLIDES})
	gs -dSAFER -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=$@ $^

%.png: %.svg
	inkscape --export-dpi=300 --export-filename=$@ $<
%.scm.svg.png: %.scm.svg
	inkscape --export-dpi=300 --export-filename=$@ $<
%.w.svg.png: %.w.svg
	inkscape --export-dpi=300 --export-filename=$@ $<


%.pdf: %.svg
	inkscape --export-dpi=300 --export-filename=$@ $<

%.scm.svg: %.scm
	guile --no-auto-compile -L ../../ -e main -s syntax-highlight-me.scm $^
%.w.svg: %.w
	guile --no-auto-compile -L ../../ -e main -s syntax-highlight-me.scm $^
%.scm.svg.pdf: %.scm.svg
	inkscape --export-dpi=300 --export-filename=$@ $<
%.w.svg.pdf: %.w.svg
	inkscape --export-dpi=300 --export-filename=$@ $<

clean:
	rm -f examples/*.svg examples/*.pdf
