/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    '../web/templates/**/*.html.mustache',
    '../web/static/**/*.html',
  ],
  theme: {
    container: {
      center: true,
    }
  },
  plugins: [
    require('@tailwindcss/typography'),
    require('daisyui'),
  ],
}
