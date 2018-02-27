#!/bin/bash
# 1. Check if local is up to date and print the result #
$(git fetch origin)
localG = $(git rev-parse HEAD)
remoteG = $(git rev-parse @{u})
if [$localG == $remoteG]
then
	echo 'Up to date'
else
	echo 'not up to date'
fi

# End #

# 2. Puts all uncommitted changes in a file changes.log #

git diff >> changes.log 
echo "All uncommitted changes have been put into changes.log"
# End #

# 3. Puts each line from every file of your project with the tag #TODO into a file todo.log #

grep -r --exclude=todo.log "#TODO" . >> todo.log
echo "Lines with #TODO have been put into todo.log"
# End #

# 4. Checks all haskell files for syntax errors and puts the results into error.log #
shopt -s nullglob
find. -type f -name "*.hs" -exec ghc -fno-code {} \; &> error.log
echo "All syntax errors have been put into error.log"
# End #

# Extra: Ask if a git pull is needed #

read -p  "Would you like to add, commit and push all of the files? (yes/no) " input
   if [ "$input" = "yes" ]
   then
      git add .
      git commit -a -m "Initial Commit"
      git push
      echo "Local repo updated"
