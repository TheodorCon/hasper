# Hasper

Hasper is a basic TODO CLI made in Haskell.

## Usage

Upon first use, Hasper will create a _.hasper_ in the user's home directory, where it stores its CSV file, used to store task information.

Hasper currently supports the following operations:

- `hasper -n "<task text>"` - task creation
- `hasper -c <task ID>` - task completion
- `hasper -a` - listing all tasks
- `hasper -u` - listing tasks in progress
- `hasper -d` - listing done tasks

## Future development

Further development will come down the line
