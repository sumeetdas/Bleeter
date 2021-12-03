module.exports = {
  purge: [],
  darkMode: false, // or 'media' or 'class'
  theme: {
    flexGrow: {
      "0": 0,
      "1": 1,
      "2": 2,
      default: 0
    },
    backgroundColor: theme => ({
      ...theme('colors'),
      "bleeter-blue": "#1a8fc5"
    }),
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
