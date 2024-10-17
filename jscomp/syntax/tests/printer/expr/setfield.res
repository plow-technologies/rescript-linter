user.name = "steve"
config.shouldSkip = !filePath.includes(allMlSuffixesCategory) &&
  !filePath.endsWith(allScriptDirectoriesCategory)

let () =
  @attr user.name = "steve"

let () =
  @attr user.name = "steve" |> @attr user.name = "steve"

user.name = steve["name"]
user.address = addresses[2]

(currentNode.parent->castNotOption).color = Black
(isLeft ? node.left : node.right).color = Black

// interpret strings correct
dict["\n"] = dict2["\""]
