
manual_title = Guix Workflow Language
_header='<div id=header><div id=header-inner class=width-control><div class=logo><img  src=\"/static/images/logo.png\"></div><div class=menu><ul><li><a href=/>Home</a></li><li class=active>Manual</li></ul></div></div></div><div id=content class=width-control>'

gendocs_options_ = \
  -I $(abs_srcdir)/build-aux \
  --html "--no-number-sections --css-ref=/static/css/main.css -c TOP_NODE_UP_URL=/manual -c AFTER_BODY_OPEN=$(_header) -c PRE_BODY_CLOSE='</div>' -c TOC_LINKS=true  -c 'EXTRA_HEAD=<meta name=\"viewport\" \ content=\"width=device-width, initial-scale=1\" />'"

gpg_key_ID = BCA689B636553801C3C62150197A5888235FACAC
my-release-prep:
	$(AM_V_GEN)$(MAKE) --no-print-directory -s announcement \
	  > ~/announce-$(my_distdir)

gnulib_dir = /home/rekado/dev/gnulib
release-prep-hook = my-release-prep
news-check-lines-spec = 1,20
news-check-regexp = $(VERSION_REGEXP)
VC_LIST_ALWAYS_EXCLUDE_REGEX = ^build-aux|ttf
local-checks-to-skip = \
  sc_immutable_NEWS \
  sc_prohibit_empty_lines_at_EOF \
  sc_unmarked_diagnostics
