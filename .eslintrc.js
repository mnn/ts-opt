module.exports = {
    root: true,
    parser: '@typescript-eslint/parser',
    plugins: [
        '@typescript-eslint',
    ],
    ignorePatterns:  ['simple-doc-gen/*.js'],
    extends: [
        'eslint:recommended',
        'plugin:@typescript-eslint/recommended',
    ],
    rules: {
        '@typescript-eslint/no-unused-vars': [
            'error',
            {
                'ignoreRestSiblings': true,
                'argsIgnorePattern': '^_',
            },
        ],
        '@typescript-eslint/no-explicit-any': 'off',
        '@typescript-eslint/array-type': 'error',
        '@typescript-eslint/no-empty-function': 'error',
        '@typescript-eslint/no-inferrable-types': 'error',
    },
};
