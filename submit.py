#!/usr/bin/python3

# This script is used to create and push tags for a local GitHub repository.
# The tags created by this script signal that a particular commit is ready to
# be graded.

import argparse
import enum
import os
import re
import subprocess
import sys
import yaml

from typing import List, Optional, Tuple, Union

DATA_DIR=".submit"

# Error code when a user request (e.g. CLI args) is invalid
ERR_BAD_REQUEST = 1
# Error code when execution environment is invalid in a user-approachable way.
# User needs to correct e.g. Git repository status but can proceed.
ERR_BAD_ENV     = 2
# Error code when the script is used in a way that cannot be successful, such
# as running from somewhere not the root of a Git repository.  This will never
# work and the script must be run differently.
ERR_MISUSE      = 3
# Error code when the script's environmental invariants have been invalidated in
# a way that the user is unlikely to be able to correct without assistance.
ERR_BAD_STATE   = 4
# Error code when the external commands used by this script behave in a way that
# this script can't diagnose or correct.
ERR_SUBPROCESS  = 5

VERBOSE=False
def log(msg):
    if VERBOSE: print("[LOG] " + str(msg))

def run(cmd: List[str], capture=True) -> Tuple[int, str, str]:
    log("Running command: {}".format(" ".join(cmd)))
    stdout = subprocess.PIPE if capture else None
    stderr = subprocess.STDOUT if capture else None
    result : subprocess.CompletedProcess = \
        subprocess.run(
            cmd, input="", stdout=stdout, stderr=stderr, text=True, check=True)
    return result.stdout

def check_repository_clean(
        skip_unstaged_check=False,
        skip_uncommitted_check=False,
        skip_upstream_check=False):
    """
    Determines if the local Git repository is "clean".  In this context, this
    means that it has (1) no unstaged files, (2) no uncommitted files, and (3)
    is up-to-date with its upstream.  This function will return a list of
    textual descriptions of problems; an empty list implies a clean repository.
    """
    log("Checking local repository to determine if it is clean.")
    status_text = run(["git","status","--branch","--porcelain=v2"])
    unstaged_files = False
    uncommitted_files = False
    upstream_difference = False
    for line in status_text.split("\n"):
        if len(line) > 0 and line[0] == '?':
            log("Repository contains unstaged file {}".format(line[2:]))
            unstaged_files = True
        elif len(line) > 0 and line[0] == 'A':
            log("Repository contains uncommitted file {}".format(line[2:]))
            uncommitted_files = True
        elif line.startswith("# branch.ab") and line != "# branch.ab +0 -0":
            log("Repository branch.ab status is {}".format(line[11:]))
            upstream_difference = True
    result = []
    if unstaged_files and not skip_unstaged_check:
        result.append(
            "You have unstaged files. (Some files have not been 'git add'ed.).")
    if uncommitted_files and not skip_uncommitted_check:
        result.append(
            "You have uncommitted files. (Some files have been 'git add'ed but "
            "you have not yet committed them.")
    if upstream_difference and not skip_upstream_check:
        result.append(
            "Your local repository differs from the upstream repository. You "
            "may need to 'git push' or 'git pull'.")
    return result

def parse_arguments():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--skip-unstaged-check", default=False, action='store_true',
        help='Disables the check for unstaged files.  Unstaged files will be '
             'ignored and will not be part of your submission.  Use with '
             'care.')
    parser.add_argument(
        "--skip-uncommitted-check", default=False, action='store_true',
        help='Disables the check for uncommitted files.  Uncommitted files '
             'will be ignored and will not be part of your submission.  Use '
             'with care.')
    parser.add_argument(
        "--skip-upstream-check", default=False, action='store_true',
        help='Disables the check for upstream differences.  If the upstream is '
             'different from your local repository, this script may fail to '
             'push the tag it creates and your work may not be submitted.  Use '
             'with care.')
    parser.add_argument(
        "--skip-duplicate-tag-check", default=False, action='store_true',
        help='Disables the check for duplicate tag submissions.  This makes it '
             'possible for you to submit the same source code twice for the '
             'same assignment.  You should not need to do this.')
    parser.add_argument(
        "--assignment", "-a", default=None, metavar='ASSIGNMENT_NAME',
        help='Specifies the name of the assignment you are submitting.')
    parser.add_argument(
        "--verbose", "-v", default=False, action='store_true',
        help='Prints additional information about what the script is doing as '
             'it runs.')
    parser.add_argument(
        "--dry-run", "-D", default=False, action='store_true',
        help='Does not perform any modifications.  This can be used to '
             'estimate what this script *would* do without actually doing it.')

    return parser.parse_args()

class ConfigurationError(Exception):
    def __init__(self, msg, config_file_text):
        super().__init__(msg)
        self.msg = msg
        self.config_file_text = config_file_text

class Configuration:
    valid_assignments : List[str]
    default_assignment: Optional[str]

def read_configuration():
    with open(os.path.join(DATA_DIR, "config.yaml")) as f:
        config_file_text = f.read()
    structure = yaml.safe_load(config_file_text)
    for key in "valid_assignments", "default_assignment":
        if key not in structure:
            raise ConfigurationError(
                "Missing key {} in configuration.".format(key),
                config_file_text)
    configuration = Configuration()
    configuration.valid_assignments = \
        list(map(str.lower, structure["valid_assignments"]))
    if structure["default_assignment"] is None:
        configuration.default_assignment = None
    else:
        configuration.default_assignment = \
            str(structure["default_assignment"]).lower()
    return configuration

@enum.unique
class CreateResult(enum.Enum):
    SUCCESS = 0
    DRY_RUN = 1
    DUPLICATE = 2

def create_and_push_new_tag(
        assignment_name: str, skip_duplicate_tag_check: bool = False,
        dry_run: bool = False
        ) -> Tuple[CreateResult,str]:
    """
    Attempts to create and push a tag for the current repository's HEAD commit.
    The meaning of this function's returned value depends upon what happened.
    * If this function successfully created and pushed a tag, then it returns
      CreateResult.SUCCESS.  The string is the name of the created tag.
    * If this function would've created and pushed a tag but did not due to
      dry running, then it returns CreateResult.DRY_RUN.  The string is the name
      of the tag that would've been created.
    * If this function did not create and push a tag because another tag already
      exists for the current HEAD commit, then this function returns
      CreateResult.DUPLICATE.  The string is the name of the duplicate tag.
    """
    # Get all the tags from the repository
    tags = run(["git","tag","--list"]).split("\n")
    tags = list(map(str.strip, tags))
    # Get the commit hash for the current head
    head_hash = run(["git","rev-parse", "HEAD"]).strip()
    # Identifies existing tags for this assignment.  Checks them to determine if
    # any refer to the current HEAD commit.  Also determines the maximum index
    # for this assignment's existing tags.
    tag_name_format = "submission_{assignment_name:s}_{index:02d}"
    tag_pattern = re.compile("^submission_([^_]+)_([0-9]+)$")
    maximum_index = 0
    for tag_name in tags:
        # Is this a tag for the current assignment?
        m = tag_pattern.match(tag_name)
        if m:
            log("Recognized tag for assignment {} with index {}".format(
                m.group(1), m.group(2)))
        else:
            log("Found tag which does not match submission format: {}".format(
                tag_name))
        if m and m.group(1) == assignment_name:
            log("Recognized tag for assignment {} with index {}".format(
                m.group(1), m.group(2)))
            # Incorporate this tag index into our aggregate.
            maximum_index = max(maximum_index, int(m.group(2)))
            # Determine if this tag points to the current HEAD commit.
            if not skip_duplicate_tag_check:
                # We have to use rev-list here (and not rev-parse) because tag
                # objects (as opposed to lightweight tags) have their own
                # hashes.  We want to dereference the tag object to see which
                # commit it's tagging.
                tag_hash = run(["git", "rev-list", "-n", "1", tag_name]).strip()
                log("Tag {} has hash {} while HEAD has hash {}.".format(
                    tag_name, tag_hash, head_hash))
                if tag_hash == head_hash:
                    # This submission is a duplicate.  Complain.
                    return (CreateResult.DUPLICATE, tag_name)
    # If we haven't bailed out by now, we must not have encountered a duplicate
    # tag and we know the highest index of a tag for this assignment.  Increase
    # by one to create our new tag name.
    tag_name = tag_name_format.format(
        assignment_name=assignment_name, index=maximum_index+1)
    log("Selecting tag name \"{}\".".format(tag_name))
    if dry_run:
        return (CreateResult.DRY_RUN, tag_name)
    else:
        run(["git","tag","-a",tag_name,"-m",
             "submit.py created this tag to submit "+assignment_name],
             capture=False)
        run(["git","push","--tags"], capture=False)
        return (CreateResult.SUCCESS, tag_name)

def ensure_tag_pushed(tag_name: str) -> bool:
    """
    Ensures that the specified tag, which should exist in the local repository,
    has been pushed to the remote.  This is useful in cases where the tag push
    was unsuccessful in a previous run.  Returns True if the tag was not pushed
    but now is; returns False if the tag had already been pushed.
    """
    output = run(["git","ls-remote","--tags"], capture=True)
    found = False
    for line in output.split("\n"):
        if line.endswith("refs/tags/" + tag_name):
            found = True
            break
    if found:
        return False
    else:
        run(["git","push","--tags"], capture=False)
        return True

def main():
    args = parse_arguments()
    # Update global configuration
    global VERBOSE
    if args.verbose: VERBOSE=True
    # Verify that we're in the right directory.
    if not os.path.exists(".git"):
        print("This script must be run from the root of your Git repository.")
        print("Your current directory is not a Git repository root.")
        return ERR_MISUSE
    # Validate local repository status.
    errors = check_repository_clean(
        args.skip_unstaged_check, args.skip_uncommitted_check,
        args.skip_upstream_check)
    if len(errors) > 0:
        print("In order to submit your work, your repository must be "
              "\"clean\": everything should be committed and pushed and you "
              "should be up to date with your remote repository.  This script "
              "cannot submit your work for the following reason(s):")
        for error in errors:
            print("  * " + error)
        print("To submit your work, please resolve the above and try again.")
        print("Please ask for help if you are unsure how to proceed.")
        return ERR_BAD_ENV
    # Read our configuration
    try:
        configuration : Configuration = read_configuration()
    except FileNotFoundError:
        print("No configuration file found.  Are you in the right Git "
              "repository?")
        return ERR_MISUSE
    except ConfigurationError as e:
        print("Error reading configuration file: {}".format(e.msg))
        print("Please contact your instructor.  Be sure to send a copy of "
              "this message.  Your configuration file contains the following "
              "text:")
        print(e.config_file)
        return ERR_MISUSE
    # Ascertain which assignment we're submitting.
    assignment : str
    if args.assignment is None:
        if configuration.default_assignment is None:
            print("You must specify which assignment you are submitting using "
                  "the -a or --assignment command-line flag.")
            return ERR_BAD_REQUEST
        else:
            assignment = configuration.default_assignment
    else:
        assignment_selected : str = args.assignment.lower()
        if assignment_selected not in configuration.valid_assignments:
            print("{} is not a valid assignment name.".format(
                assignment_selected))
            print("Valid assignment names are: {}".format(
                ", ".join(configuration.valid_assignments)))
            return ERR_BAD_REQUEST
        else:
            assignment = assignment_selected
    # Create and push a tag for this assignment
    (create_result, tag_name) = create_and_push_new_tag(
        assignment, skip_duplicate_tag_check=args.skip_duplicate_tag_check,
        dry_run=args.dry_run)
    if create_result == CreateResult.SUCCESS:
        print("Created and pushed new tag: {}".format(tag_name))
        print("Your work has been submitted for grading.")
    elif create_result == CreateResult.DRY_RUN:
        print("This script would create and push the tag {}.".format(tag_name))
        print("However, this is a dry run, so we will do nothing.")
        print("Your work has NOT been submitted for grading.")
    elif create_result == CreateResult.DUPLICATE:
        if ensure_tag_pushed(tag_name):
            print("Pushed existing tag: {}".format(tag_name))
            print("Your work has been submitted for grading.")
        else:
            print(("Your current commit has already been submitted as tag "
                   "{}.").format(tag_name))
            print("This is probably not what you meant to do.  Please contact "
                  "your instructor if you need help.")
            return ERR_BAD_REQUEST
    else:
        print("Internal error: unrecognized CreateResult {}".format(
            str(create_result)))
        return ERR_BAD_STATE
        
    # All good!
    return 0

if __name__ == "__main__":
    try:
        ec = main()
        sys.exit(ec if ec is not None else 0)
    except subprocess.CalledProcessError as e:
        print("Exit code {:d} when running command: {}".format(
            e.returncode, " ".join(e.cmd)))
        print("To submit your work, solve this error and try again.")
        print("Please ask for help if you are unsure how to proceed.")
        sys.exit(ERR_SUBPROCESS)
