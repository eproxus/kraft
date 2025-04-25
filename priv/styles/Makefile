CSS_ROOT = ../web/static/assets

CSS_IN := $(firstword $(wildcard *.css))
CSS_OUT := $(patsubst %.css,$(CSS_ROOT)/%.css,$(CSS_IN))

TAILWINDCSS = tailwindcss

ifeq (, $(shell which $(TAILWINDCSS)))
$(error "No 'tailwindcss' in PATH")
endif

.PHONY: css
css: $(CSS_OUT) ## Build the main CSS file
	@echo -n

# Rule to build the main CSS file from its source
$(CSS_OUT): $(CSS_IN) $(CSS_ROOT)
	@$(TAILWINDCSS) -i $(CSS_IN) -o $(CSS_OUT) --minify

$(CSS_ROOT):
	@mkdir -p $@

.PHONY: watch
watch: $(CSS_ROOT) ## Watch sources and continuously re-build CSS using watchexec
	@$(TAILWINDCSS) -i $(CSS_IN) -o $(CSS_OUT) --minify --watch

.PHONY: help
help: ## Show this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
