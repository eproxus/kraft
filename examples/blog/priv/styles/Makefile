TEMPLATES = $(shell find ../web/templates -type f -name '*.html.mustache')
HTML = $(shell find ../web/static -type f -name '*.html' 2>/dev/null)
STATIC = Makefile tailwind.config.js .yarn/install-state.gz

CSS_ROOT = ../web/static/assets

CSS_IN = styles.css
CSS_OUT = $(CSS_ROOT)/$(CSS_IN)

.PHONY: all
all: $(CSS_OUT)
	@echo -n

.PHONY: watch
watch:
	@yarn tailwindcss -i $(CSS_IN) -o $(CSS_OUT) --minify --watch

$(CSS_OUT): $(CSS_IN) $(CSS_ROOT) $(TEMPLATES) $(HTML) $(STATIC)
	@yarn tailwindcss -i $< -o $@ --minify

$(CSS_ROOT):
	@mkdir -p $@

.PHONY: upgrade
upgrade:
	@yarn up tailwindcss@latest

.yarn/install-state.gz:
	@yarn install
