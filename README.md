
Feedback
========

Accumulate students points from different groups and through various
tutors.

Students work on weekly exercises in groups of (mostly) 2.  Student
groups may be reorganised during the semester: Some students drop out,
others form new groups, change their partner, or move to a different
tutorial time slot (and tutor).

The groups' solutions are stored in per-group directories.  Each tutor
reviews and rates the submissions of (about) 10 groups, so six
tutorials are enough to accommodate 120 students.

All data is stored in simple plain text files.

Given requirements to pass (e.g., a minimum of passed exercises),
`feedback` calculates an overview listing the *individual* situation
for each student, and also generates feedback for all groups.
`feedback` also performs consistency checks, e.g., making sure that no
student wrongly gains points from two different groups for the same
assignment.



Building
--------

    $ make


Documentation
-------------

Read `help.txt` or run the compiled binary without arguments.


Demo
----

To set up the demo environment, run

    $ demo/prepare

then the setup is as follows:

  * `demo/groups` defines which students form which groups.

  * The directory `demo/group` contains the directories used by the
    individual student groups to commit their solutions (for the demo,
    this is created by `prepare` from the groups definitions above).
    In each group directory we'll create a file summarising the
    group's achievements.

  * `demo/max_punkte` defines how many (bonus) points are available in
    each exercise.

  * Two tutors maintain `tutor-a.points` and `tutor-b.points`
    respectively, noting how many points a group gained for each
    assignment.

  * The shell script `demo/feedback` defines criteria required to pass
    (minimum percentage of overall points, minimum required points per
    exercise, etc.), and input/output files and directories.

Running

    $ demo/feedback

should perform consistency checks, and then give you a listing of
points gained by each student.  Each group's `punkte` file is updated.
