# Brief Instruction of ProjectAnalyze.sh

- The file was made for the first assignment and done by Kevin Zhou


 

# Features

  >- The first part of the code checks if local repo is up to date and prints the result. The git command and some basic syntax was quoted on stack overflow
  >- The second part of the code just basically puts all uncommitted changes into a file, which named "change.log"
  >- The third part of the function puts each line from every file of your project with the tag #TODO into a file "todo.log"
  >- The fourth part of the code checks all haskell files for syntax errors and puts the results into error.log
  >- The last part just gives users a option if they want to commit and push all the uncommitted files
  >- The script will run through all these parts automatically until the last part, which requires users to choose if needed.

# References and Notices
  >- The first function was inspired and modified from a post by *Neil Mayhew* on Stack Overflow, and here is the [link](https://stackoverflow.com/questions/3258243/check-if-pull-needed-in-git). I was trying to use git status to check if local is up to date, but I haven't yet figured out the correct syntax, so I just uses another method, which is comparing SHA1, but it doesn't always work well.
  
  >- The rest part of the code are basically made by basic git or bash command and hints from my other friends.
  >- I made some minor changes of the file directly on the github webpage and it might have "syntax" issues while running this script if directly downloading this file, and I have no idea about that. If this happens, just simply copy the code and save to a new .sh file, then it should work.
