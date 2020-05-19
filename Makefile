
.PHONY: all
all: R/err-composite.R \
     R/err-member.R \
     R/err-single.R \
     R/err-specific.R \
     README.md \
     documentation

## Create README

README.md: README.rmd
	Rscript -e 'knitr::knit("README.Rmd")'


## Create 'err' files (note that there is no 'chk_tdy')

R/err-composite.R: make_err.awk \
                   R/chk-composite.R
	awk -f $< R/chk-composite.R > $@

R/err-member.R: make_err.awk \
                R/chk-member.R
	awk -f $< R/chk-member.R > $@

R/err-single.R: make_err.awk \
                R/chk-single.R
	awk -f $< R/chk-single.R > $@

R/err-specific.R: make_err.awk \
                  R/chk-specific.R
	awk -f $< R/chk-specific.R > $@


## Documentation

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"

.PHONY: clean
clean:
	rm -f R/err-composite.R
	rm -f R/err-member.R
	rm -f R/err-single.R
	rm -f R/err-specific.R
