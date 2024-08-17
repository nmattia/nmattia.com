// .prettierrc.mjs
// recommended astro config:
// https://github.com/withastro/prettier-plugin-astro/tree/1f2b7de2900df251cb9f8674405acbb80d8542b1?tab=readme-ov-file#recommended-configuration
/** @type {import("prettier").Config} */
export default {
  plugins: ['prettier-plugin-astro'],
  overrides: [
    {
      files: '*.astro',
      options: {
        parser: 'astro',
      },
    },
  ],
};
