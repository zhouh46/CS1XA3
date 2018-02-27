# Brief Instruction of ProjectAnalyze.sh

- The file was made for the first assignment and done by Kevin Zhou


 

# Features

  >- The first part of the code checks if local repo is up to date and prints the result. The git command and some basic syntax was quoted on stack overflow
  >- The second part of the code just basically puts all uncommitted changes into a file, which named "change.log"
  >- The third part of the function puts each line from every file of your project with the tag #TODO into a file "todo.log"
  >- The fourth part of the code checks all haskell files for syntax errors and puts the results into error.log
  >- The last part just gives users a option if they want to commit and push all the uncommitted files
  >- The script will run through all these parts automatically until the last part, which requires users to choose if needed.

# References
  >- The first function was inspired and modified from a post by *Neil Mayhew* on Stack Overflow, and here is the [link](https://stackoverflow.com/questions/3258243/check-if-pull-needed-in-git)
  
  >- The rest part of the code are basically made by basic git command and hints from my other friends.
