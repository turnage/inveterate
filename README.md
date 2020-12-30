# inveterate

`inveterate` watches a directory recursively for changes to files. When a file
is changed (modified, deleted, created), it runs a user-provided command. It
builds on inotify. It adds recursive watching and monitors the contents of newly
added or created directories as well.

`inveterate` allows users to express a substitution string, so their command can
employ the path of the changed file as a parameter. It also allows users to
express regex filters on these paths to ignore or only select certain events to
listen to.

### Examples

To re-run a Rust crate whenever a rust source file changes:

````inveterate --select .rs$ <crate-dir> "cargo run"````

To convert pngs to gifs whenever they appear or change in only the top level of
a watched directory:

````inveterate --select ^[^/].png$ --substitute %i% <dir> "test -e %i% && convert %i% $(basename %i%.gif)"````

To kill and re-launch a web server whenever a source file changes, while you're
editing with `Vim`:

````inveterate --ignore "(4013|~$|.swp$)" <dir> serve-my-website````

(Vim creates a temporary file called '4013' basically any time it hears you
breathe.)

### Limitations

`inveterate` only executes on file events, not directory events. Moving a
directory into a watched directory, or creating one there will start watching
those new directories for file events, but will not execute the user command
when they appear.

