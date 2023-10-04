/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.purs"],
  theme: {
    extend: {
      boxShadow: {
        'custom': '1px 1px 0px 0px #808080',
        'custom-hover': '0.5px 0.5px 1.5px 1px #808080'
      },
    },
  },
  plugins: [],
}

