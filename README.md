Description
===========

The vty-hangman-helper tool is designed to help
you solve Hanging With Friends puzzles and to explore
how difficult words you choose will be.

Installation
============

Install the Haskell Platform and use the cabal-install
tool to install this program.

$ cabal install

Usage
=====

Use the -s flag to automatically hide all letters
except the last vowel.

Use the -f flag to select your own word list.

When specifying the starting mask, use '.' for unknown
letters.

vty-hangman-helper \[-s] [-f FILENAME] STARTING\_WORD

Examples
========

$ vty-hangman-helper .a...a.

$ vty-hangman-helper -s hangman
