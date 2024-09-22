# Examples

---
<!-- toc -->
---

## Config

You can add a `hooks` field to your config file like this:

```yaml
hooks:
  directory: /Users/adrian/Dropbox/TaskLite/hooks
  launch:
    pre:
      - interpreter: v
        body: |
          fn main() {
            println('{
              "message": "📜 Pre launch message",
              "warning": "⚠️ Pre launch warning",
              "error": "❌ Pre launch error"
            }')
          }
    post: []
  add:
    pre: []
    post: []
  modify:
    pre: []
    post: []
  exit:
    pre: []
```


## V Lang Scripts

Save the examples in the hooks directory as `pre-launch.v`, etc.


### Pre Launch

```v
fn main() {
  println('{
    "message": "📜 Pre launch message",
    "warning": "⚠️ Pre launch warning",
    "error": "❌ Pre launch error"
  }')
}
```


### Post Launch

```v
import os
import json

struct PostLaunchInput {
mut:
  arguments []string
}

struct PostLaunchOutput {
  message string
  warning string
  error   string
}

fn main() {
  stdin := os.get_raw_lines_joined()
  mut input := json.decode(PostLaunchInput, stdin)!

  println(json.encode(PostLaunchOutput{
    message: '📜 Post launch message\n' + //
     'Provided arguments: ${input.arguments}\n'
    warning: '⚠️ Post-launch warning'
    error: '❌ Post-launch error'
  }))
}
```


### Pre Add

```v
import os
import json

struct Task {
mut:
  body string
  tags []string
}

struct PreAddInput {
mut:
  task_to_add Task     @[json: 'taskToAdd']
  arguments   []string
}

struct PreAddOutput {
mut:
  task    Task
  message string
  warning string
  error   string
}

fn main() {
  stdin := os.get_raw_lines_joined()
  input_data := json.decode(PreAddInput, stdin)!

  mut task := input_data.task_to_add
  task.tags << 'additional-tag'

  pre_add_output := PreAddOutput{
    task: task
    message: '📜 Pre-add message\n' + //
     'Provided arguments: ${input_data.arguments.str()}\n' + //
     'Provided task: ${input_data.task_to_add.str()}\n' + //
     'Updated task: ${task.str()}\n'
    warning: '⚠️ Pre-add warning'
    error: '❌ Pre-add error'
  }

  println(json.encode(pre_add_output))
}
```


### Post Add

```v
import os
import json

struct Task {
mut:
  body string
  tags []string
}

struct PostAddInput {
mut:
  arguments  []string
  task_added Task     @[json: 'taskAdded']
}

struct PostAddOutput {
mut:
  message string
  warning string
  error   string
}

fn main() {
  stdin := os.get_raw_lines_joined()
  mut input_data := json.decode(PostAddInput, stdin)!

  post_add_output := PostAddOutput{
    message: '📜 Post-add message\n' + //
     'Provided arguments: ${input_data.arguments.str()}\n' + //
     'Added task: ${input_data.task_added.str()}\n'
    warning: '⚠️ Post-add warning'
    error: '❌ Post-add error'
  }

  println(json.encode(post_add_output))
}
```


### Pre Modify

```v
import os
import json

struct Task {
mut:
  body string
  tags []string
}

struct PreModifyInput {
mut:
  arguments      []string
  task_to_modify Task     @[json: 'taskToModify']
}

struct PreModifyOutput {
mut:
  task    Task
  message string
  warning string
  error   string
}

fn main() {
  stdin := os.get_raw_lines_joined()
  input_data := json.decode(PreModifyInput, stdin)!

  mut task := input_data.task_to_modify
  task.tags << 'additional-tag'

  pre_modify_output := PreModifyOutput{
    task: task // TODO: This should be parsed by TaskLite and used downstream
    message: '📜 Pre-modify message\n' + //
     'Provided arguments: ${input_data.arguments.str()}\n' + //
     'Provided task: ${input_data.task_to_modify.str()}\n' + //
     'Updated task: ${task.str()}\n'
    warning: '⚠️ Pre-modify warning'
    error: '❌ Pre-modify error'
  }

  println(json.encode(pre_modify_output))
}
```


### Post Modify

```v
import os
import json

struct Task {
mut:
  body string
  tags []string
}

struct PostModifyInput {
mut:
  arguments     []string
  task_modified Task     @[json: 'taskModified']
}

struct PostModifyOutput {
mut:
  message string
  warning string
  error   string
}

fn main() {
  stdin := os.get_raw_lines_joined()
  mut input_data := json.decode(PostModifyInput, stdin)!

  post_modify_output := PostModifyOutput{
    message: '📜 Post-modify message\n' + //
     'Provided arguments: ${input_data.arguments.str()}\n' + //
     'Modified task: ${input_data.task_modified.str()}\n'
    warning: '⚠️ Post-modify warning'
    error: '❌ Post-modify error'
  }

  println(json.encode(post_modify_output))
}
```


### Pre Exit

```v
fn main() {
  println('{
    "message": "📜 Pre exit message",
    "warning": "⚠️ Pre exit warning",
    "error": "❌ Pre exit error"
  }')
}
```


## Shell Scripts

Save the examples in the hooks directory as `pre-launch.sh`, etc.

### Pre Launch

```sh
stdin=$(cat)

>&2 echo "File > pre-launch: Input via stdin:"
>&2 echo "$stdin"
echo "{}"
```


### Post Launch

```sh
stdin=$(cat)

>&2 echo "File > post-launch: Input via stdin:"
>&2 echo "$stdin"
echo "{}"
```


### Pre Add

```sh
stdin=$(cat)

>&2 echo "File > pre-add: Input via stdin:"
>&2 echo "$stdin"
echo "{}"
```
