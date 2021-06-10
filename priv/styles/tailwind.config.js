module.exports = {
  purge: {
    enabled: true,
    content: [
      '../web/templates/**/*.mustache',
      '../web/static/**/*.html',
    ]
  },
  theme: {},
  variants: {},
  plugins: [
    require('@tailwindcss/typography'),
    require('@tailwindcss/forms'),
  ],
}
