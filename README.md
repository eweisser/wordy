# wordy
A game of "word hide and seek" where the computer tries to figure out the five-letter English word the user is thinking of. The user can also determine the computer's own secret five-letter word.

Both the user and the computer find out information about each other's secret word by guessing additional five-letter words. The number of letters the secret word and a guessed word have in common is reported after each guess. For example, if the computer's secret word is 'CLOUD', and the user guesses 'DRONE', the computer will report that the user's guess had two correct letters, but not which letters were correct (in this case, D and O).

Because this is the only data available for determining the secret words, the words the user or computer guesses must be chosen carefully to gain new and useful information. Although algorithms for choosing a word to guess, and the speed with which they tend to find a secret word, can be compared objectively, there is no single obvious algorithm for the problem.

Guessed words and secret words must be present in a hard-coded lexicon. I have removed some particularly infrequently-used words from it, using frequency data from Google Ngram Viewer, but this process is not complete.

Right now, this project is written in Python, but I plan to write a similar program in Java, with an improved user interface—and hopefully, improved algorithms for guessing the user's word. 
