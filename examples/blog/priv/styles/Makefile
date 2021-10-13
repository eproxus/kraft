TEMPLATES = $(shell find ../web/templates -type f -name '*.html.mustache')
HTML = $(shell find ../web/static -type f -name '*.html')

all: ../web/static/assets/styles.css
	@echo -n

../web/static/assets/styles/%.css: %.css $(TEMPLATES) $(HTML)
	@yarn install
	@npx tailwindcss build $< -o $@
