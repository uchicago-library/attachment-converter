# Workflow for UChicago Library Developers

This section will describe our workflow for coming up with a new
feature, a developer working on that feature, and the rest of us
merging that feature in.

# Current Developers

Our development team, funded by the [Email Archives: Building Capacity
and Community](https://emailarchivesgrant.library.illinois.edu/)
Mellon project out of [UIUC](https://www.library.illinois.edu/),
currently includes:

- [Nathan Mull](https://github.com/nmmull)
- [Cormac Duhamel](https://github.com/cormacd9818)

Previous developers include Owen Price Skelly, who wrote much of the
initial exploratory code from this project in 2020 as part of a
research practicum, then returned during the 2021-2022 academic year
to build some of the core functionality out.

[Matt Teichman](https://elucidations.vercel.app/) and
[Keith Waclena](https://www2.lib.uchicago.edu/keith/) will be
contributing to and maintaining the project on an ongoing basis, after
the end of the grant period.  Matt is currently in charge of managing
the project.

### Ordinary Workflow

Here is how we envision the life cycle of a new Attachment Converter
feature.

- we come up with an idea while talking about the project
- one of us---probably typically Matt---writes up a new GitHub issue
  describing that feature
- the title of the issue should be a short one-line prose description
  of the new feature
- GitHub will automatically gensym up a number for the issue when it
  gets created; that will be the primary identifier for the issue
  within GitHub
- at first, it's just there for reference---no one gets assigned to
  the issue until we decide it's time for it to be added to the
  project
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
  free to commit all the time, keep pushing the branch up to GitHub,
  pulling it down, etc.
- when the assignee thinks it's ready for prime time, they should
  create a new pull request merging the feature branch into `main` ,
  associate it with the relevant issue (GitHub should do that
  automatically), and assign everyone else in the group as a reviewer
- the group, presently, is Matt, Keith, Cormac, Owen, and Nathan
- Keith won't necessarily have time to code review every issue, but he
  will have time to do some of them, and Matt will take care of
  un-assigning him and others, where applicable
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
- the person in charge of the PR can then make the relevant changes,
  push them up to the feature branch, and notify us on Slack when the
  latest round of changes are in
- if the person doing code review noticed a significant problem, like
  a bug that might take several days or a week to fix, we'll discuss
  on Slack whether we want to fix it now or make fixing it into a new
  GitHub issue
- once every reviewer has either approved the PR or been unassigned
  from it, Matt will take care of merging the feature branch into
  `main` and re-assign developers to new issues


### Fast-Track Workflow

Occasionally we'll be in a situation where we wanna fast track some
feature branch for merging into `main`.  If that happens, we'll follow
the same workflow described above, with the difference that only one
or two people will be assigned to the PR as reviewers, and the bar for
merging the feature branch in will be more like: unless there's a
really bad bug, approve it.  Any suggested changes left over after
that can be written up into a new GitHub issue.
