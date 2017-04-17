<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [How to run tests](#how-to-run-tests)
- [New-style tests](#new-style-tests)
    - [Types of tests](#types-of-tests)
        - [Tests for internals](#tests-for-internals)
        - [Tests for interactive commands](#tests-for-interactive-commands)
        - [Tests for language features](#tests-for-language-features)
    - [Helpers](#helpers)
- [Old-style tests](#old-style-tests)
- [Deprecated tests](#deprecated-tests)

<!-- markdown-toc end -->

As the smartparens project evolved, we've tried several testing strategies, from the initial "freestyle" tests through some formalization back to freestyle back to formalization (*sigh*).  I will describe the current practices and then add a few remarks about the "old style" suits which are still found (and run) in the repository (on the CI), but are discouraged to be added to.

# How to run tests

We use [ert-runner](https://github.com/rejeep/ert-runner.el) to run all test suites (old and new).

To run all tests, run

```
$ cask exec ert-runner
```

This ensures all dependencies are installed and tests are run in proper environment.

If you have all the dependencies already installed (for example by calling `cask install` or from previous test runs), you can invoke `ert-runner` directly by calling

```
$ cask exec ert-runner
```

You can use the `-p` switch to only run specific subset of the tests (matched as regular expression), so

```
$ cask exec ert-runner -p python
```

will only run tests related to python.

# New-style tests

Tests are organized in the `test` directory.  Tests are added to an "appropriate" thematic test file.  When picking the right file, use these simple heuristics:

* each broader feature has its own file: parser, insertion, wrapping, commands...
* tests for specific language features go into their own file (`smartparens-<language>-test.el`)

Each test file must end with the suffix `-test.el`. They are regular elisp files.  Tests are defined using `ert-deftest`.  All `ert` tests start with prefix `sp-test`.  All language-specific tests add the language name afterwards, for example `sp-test-python`.  It is useful to follow this convention so that we can run only language specific tests when we change language specific feature.

We try to write tests with as little magic as possible, only abstracting the most annoying repetitions.  So instead of adding endless layers of helpers, try to write the tests as plainly as possible to make it easy to throw them away and update or replace them as the conditions change.

## Types of tests

There are two philosophical classes of tests: tests testing specific *behaviour* and *data-driven* tests.  The former is usually employed to test features for specific language support, the latter for the internals, such as the parser, navigation commands and so on.

Behavioral tests should test one specific behaviour, for examples see `smartparens-python-test.el`.  The name of the test should reflect what is being tested.

Data-driven tests usually look very similar: you feed an input to a function and compare its output with an expected value.  Repeat for many input/output data sets to verify broad range of possible executions (boundary cases/regular cases/failure modes etc.).  This is more advantageous for testing pure functions (no side effects) where you can't test "behaviour" directly as there is seemingly none.

### Tests for internals

Internals tests are usually data-driven.  These include tests for the various parsers, for pair insertion and wrapping helpers and similar.

### Tests for interactive commands

Most commands are tested using the `sp-test-command` macro in `smartparens-commands-test.el` file.  This macro wraps the common pattern of:

* insert string,
* execute a command,
* compare with result string.

These tests are data-driven---we test each command by trying lots of input-output scenarios to make sure it works as expected.

Language setup and overriding of prefix arguments is also supported.  It is best to read the existing examples to see how it works.

### Tests for language features

Finer support for specific languages usually gets one test per feature.  If we change how `'` pair behaves in python, we add a test for each added/changed behaviour. Each test has a representative name (within reason) to help us located the error.

## Helpers

All tests are usually best written using the `sp-test-with-temp-buffer` helper method.  It takes two required arguments, `initial` which is a string to be inserted in a new temporary buffer, and `initform` which is a form executed before the text is inserted.  Then follow arbitrary forms.

There is special syntax available for `initial` string:
* the point is set to the first occurrence of `|`, this character is then removed
* the mark is set to the first occurrence of `M`, this character is then removed

Tests are always run with `case-fold-search` set to `nil`.  Before `initform` is executed, input method is turned off.  After `initform` is executed, `smartparens-mode` is turned on, the `initial` text is inserted and point and mark are set.  Then the rest of the code is executed.

Inside the body you can execute any code you wish with at least one assertion (see `should` from `ert`).

A special version `sp-test-with-temp-elisp-buffer` is provided which automatically sets up `emacs-lisp-mode`.

You can add some simple wrappers for data-driven tests where it loops through the input/output pairs and calls some function, or just call the function manually from the `ert-deftest` as many times as you need.  If the helpers aren't absolutely needed, we recommend to avoid them.

Here is an example `ert` test template (designed for `clojure-mode`):

``` emacs-lisp
(ert-deftest sp-test-somethingsomething-clojure ()
  (sp-test-with-temp-buffer "initial buffer content, | is the initial point"
      (clojure-mode)  ;; initform, could be a `progn'
    (your-code-in-a-new-temp-clojure-buffer)
    (optionally (insert "|") so you can test the resulting point)
    (should (equal (buffer-string) "output | buffer"))))
```

# Old-style tests

There are some remnants of the old system which can be characterized by lots of magic and abstractions.  With time it became clear that such an approach is not ideal for tests: it restricts too much and helps too little.  If you find any test which you don't understand right away, chances are you are looking at the "old-style" suites.

Before you start writing your test read the previous section on "new-style" tests.  If things still aren't clear, ask someone before you start implementing your tests to save both your and our time.  We love questions, so don't worry!  This project has a lot of cruft in it and you can't blame yourself for "not getting it" :P

# Deprecated tests

We used `ecukes` for testing way back when smartparens was simpler, but writing the support code turned out to be more problems than the framework solved.  As the tests became unmaintainable we dropped them completely.  The suits are still in the repository as we still want to migrate all the tests present there to new test suites.  You are very welcome to help!

We do not accept new ecukes test features anymore.
