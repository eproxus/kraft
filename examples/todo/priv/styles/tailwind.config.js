/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    '../web/templates/**/*.html.mustache',
    '../web/static/**/*.html',
  ],
  theme: {
    extend: {
      typography: ({ theme }) => ({
        system: {
          css: {
            '--tw-prose-body': 'CanvasText',
            '--tw-prose-headings': 'CanvasText',
            '--tw-prose-lead': 'CanvasText',
            '--tw-prose-links': 'AccentColor',
            '--tw-prose-bold': 'CanvasText',
            '--tw-prose-counters': 'AccentColor',
            '--tw-prose-bullets': 'AccentColor',
            '--tw-prose-hr': theme('colors.pink[300]'),
            '--tw-prose-quotes': 'CanvasText',
            '--tw-prose-quote-borders': theme('colors.pink[300]'),
            '--tw-prose-captions': theme('colors.pink[700]'),
            '--tw-prose-code': 'CanvasText',
            '--tw-prose-pre-code': theme('colors.pink[100]'),
            '--tw-prose-pre-bg': 'CanvasText',
            '--tw-prose-th-borders': theme('colors.pink[300]'),
            '--tw-prose-td-borders': theme('colors.pink[200]'),
            '--tw-prose-invert-body': 'CanvasText',
            '--tw-prose-invert-headings': 'CanvasText',
            '--tw-prose-invert-lead': 'CanvasText',
            '--tw-prose-invert-links': 'AccentColor',
            '--tw-prose-invert-bold': 'CanvasText',
            '--tw-prose-invert-counters': 'AccentColor',
            '--tw-prose-invert-bullets': 'AccentColor',
            '--tw-prose-invert-hr': theme('colors.pink[700]'),
            '--tw-prose-invert-quotes': 'CanvasText',
            '--tw-prose-invert-quote-borders': theme('colors.pink[700]'),
            '--tw-prose-invert-captions': theme('colors.pink[400]'),
            '--tw-prose-invert-code': theme('colors.white'),
            '--tw-prose-invert-pre-code': theme('colors.pink[300]'),
            '--tw-prose-invert-pre-bg': 'rgb(0 0 0 / 50%)',
            '--tw-prose-invert-th-borders': theme('colors.pink[600]'),
            '--tw-prose-invert-td-borders': theme('colors.pink[700]'),
          },
        },
      }),
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
    require('@tailwindcss/forms'),
  ],
}

