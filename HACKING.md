Git pre-commit hook
===================

The `Makefile` has the rule `git_pre_commit` which can be used in the
pre-commit hook by adding something like

````
echo "Starting pre-commit"
make git_pre_commit
if [ $? -ne 0 ]; then
  exit 1
fi
echo "Ending pre-commit"
````

into the `.git/hooks/pre-commit` file.
