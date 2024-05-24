TEMPLATES = $(shell find ../web/templates -type f -name '*.html.mustache')
HTML = $(shell find ../web/static -type f -name '*.html' 2>/dev/null)
STATIC = Makefile tailwind.config.js bun.lockdb

CSS_ROOT = ../web/static/assets

CSS_IN = styles.css
CSS_OUT = $(CSS_ROOT)/$(CSS_IN)

all: help

.PHONY: css
css: $(CSS_OUT) ## Build CSS
	@echo -n

.PHONY: watch
watch: css ## Watch HTML and continuously re-build CSS
	@bun x tailwindcss -i $(CSS_IN) -o $(CSS_OUT) --minify --watch

$(CSS_OUT): $(CSS_IN) $(CSS_ROOT) $(TEMPLATES) $(HTML) $(STATIC)
	@bun x tailwindcss -i $< -o $@ --minify

$(CSS_ROOT):
	@mkdir -p $@

.PHONY: upgrade
upgrade: css ## Upgrade Tailwind
	@bun update

bun.lockdb: package.json
	@bun install

.PHONY: help
help: ## Show this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
