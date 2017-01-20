HACKING
=======

#### Git pre-commit hook

The `Makefile` has the rule `git_pre_commit` which can be used in the
pre-commit hook by adding something like

````Bash
echo "Starting pre-commit"
make git_pre_commit
if [ $? -ne 0 ]; then
  exit 1
fi
echo "Ending pre-commit"
````

into the `.git/hooks/pre-commit` file.

#### Before make a pull request

Assert everything is all right running these commands.

* `$ make install-bin`
* `$ make tests`

#### About the code style

In the following list, we keep some advices that improve the quality of
the code from our point of view. We don't include an item about the layout of
source code, but we encourage you to check it out in source files.

* All files must end by a *newline*
* Comment all the exported functions in the modules
* Delete *trailing spaces*
* Each line cannot exceed *80 characters*
* Run always `$ make hlint`
* The *arrows* must be *aligned* in a **case of** structure
* The imports must be *sorted* alphabetically
* Try to avoid redundant *parenthesis* when it is possible

**Examples**

  * For instance, redundant parenthesis in a *case of* structure
  for pattern matching.

  ```Haskell
  case maybeValue of
      (Just x)→ somethingCool x
      (Nothing) → failure
  ```

  It looks nicer presented like

  ```Haskell
  case maybeValue of
    Just x  → somethingCool x
    Nothing → failure
  ```

  Notice in the above example, that the arrows are aligned, the indentation
  follows the two spaces rule and we removed the parenthesis.

  * Don't miss the type in a *let* definition.
  Instead of doing this

  ```Haskell
  let dotApia = homeDir </> ".apia"
  ```
  we think is better this

  ```Haskell
  let dotApia ∷ FilePath
      dotApia = homeDir </> ".apia"
  ```

  Or the same thing about the scope of a *where*.

  ```Haskell
  getDefaults ∷ IO Options
  getDefaults = return somethingNice
    where
      somethingNice = defaultsOpts
  ```
  Better if we do this

  ```Haskell
  getDefaults ∷ IO Options
  getDefaults = return somethingNice
    where
      somethingNice ∷ Options
      somethingNice = defaultsOpts
  ```

  We believe that these little details in the code will help for the maintenance,
  the readability and self documenting of the source code.

  * Commenting functions

  ```Haskell
  -- | Get the default values for the command-line 'Options'.
  getDefaults ∷ IO Options
  getDefaults = somethingNice
  ```
