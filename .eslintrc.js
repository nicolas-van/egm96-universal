module.exports = {
  env: {
    browser: true,
    es6: true,
    node: true,
    'jest/globals': true
  },
  extends: [
    'standard',
    'plugin:jsdoc/recommended'
  ],
  globals: {
    Atomics: 'readonly',
    SharedArrayBuffer: 'readonly'
  },
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: 'module'
  },
  plugins: [
    'jest',
    'jsdoc'
  ],
  rules: {
    'no-var': 'error',
    'require-jsdoc': ['error', {
      require: {
        ClassDeclaration: true,
        FunctionDeclaration: true,
        MethodDefinition: true
      }
    }]
  }
}
