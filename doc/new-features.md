# Workflow for UChicago Library Developers

This section will describe our workflow for coming up with a new
feature, a developer working on that feature, and the rest of us
merging that feature in.

### Ordinary Workflow

Here is how we envision the life cycle of a new Attachment Converter
feature.

- we come up with an idea while talking about the project
- a member of the dev team writes up a new GitHub issue describing
  that feature
- the title of the issue should be a short one-line prose description
  of the new feature
- GitHub will automatically generate a number for the issue when it
  gets created; that will be the primary identifier for the issue
  within GitHub
- at first, it's just there for reference—no one gets assigned to the
  issue until we decide it's time for it to be added to the project
- once it's time, the author of the issue should then assign it to
  whoever agreed to work on the new feature
- the person assigned to the issue should pull down the branch they
  want to fork the new feature branch from, and create a new branch
  starting with the number of the issue, followed by the 'slugified'
  prose name of the issue
- 'slugified' means all lowercase and spaces are hyphens
- for example, if issue number 12 is called "Delete All Emails For All
  Time", then the new branch should be called
  `12-delete-all-emails-for-all-time`
- while working on the feature, the person assigned to it should feel
  free to commit as often as they want, pushing to GitHub as they go
- we do not impose any requirements on squashing feature branch
  commits
- when the assignee thinks it's ready for prime time, they should
  create a new pull request merging the feature branch into `main` ,
  associate it with the relevant issue (GitHub should do that
  automatically), and assign everyone else in the group as a reviewer
- if you are assigned as a reviewer to a PR, take an afternoon or
  morning at your earliest convenience to pull the code down, compile
  it, test it, and make any style suggestions you have using GitHub's
  cool interface for showing your the diffs in the branch, pinning
  comments to specific line numbers, etc.
- if the comments are minor, you can go ahead and approve the feature
  branch for changes
- if you have a substantial suggested change, like 'fix this runtime
  bug I discovered', you should probably request changes before
  approving the pull request
- the person in charge of the PR will then make the relevant changes,
  push them up to the feature branch, and notify the reviewer on Slack
  when the latest round of changes are in
- if the reviwer noticed a significant problem, like a bug that might
  take several days or a week to fix, the team will discuss whether we
  want to fix it now or make fixing it into a new GitHub issue
- if the reviewer is not able to finish code review in one week, the
  developer in charge of the new feature will merge the new changes in
  unreviewed
- this ensures that no new changes are held up by the code review
  process
