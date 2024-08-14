import type { RehypePlugin } from "@astrojs/markdown-remark";
import type { Visitor } from "unist-util-visit";
import type { ElementContent } from "hast";
import { classnames } from "hast-util-classnames";
import { isElement } from "hast-util-is-element";
import { visit } from "unist-util-visit";

/* This is a rehype plugin adding support for shiki "commands". */

export const popCommand = (nodes: ElementContent[]): string | undefined => {
  const reg = /\[(?<sh>sh_|sh)!\s*(?<command>\w+)]/;

  let i = nodes.length;
  while (i--) {
    const node = nodes[i];
    if (
      node.type !== "element" ||
      !node.children ||
      node.children.length < 1 ||
      node.children[0].type !== "text"
    ) {
      continue;
    }
    const match = reg.exec(node.children[0].value);

    if (match === null) {
      continue;
    }

    const command = match.groups?.command;
    const sh = match.groups?.sh;

    if (sh === "sh") {
      // If the annotation is "sh", then remove the whole (most likely "comment") block
      nodes.splice(i, 1);
    } else if (sh === "sh_") {
      // If the annotation is "sh_", then only remove the match and leave the rest of the block intact
      const value = node.children[0].value;
      node.children[0].value =
        value.slice(0, match.index) +
        value.slice(match.index + match[0].length);
    }

    // NOTE: returns at most one command
    if (command !== undefined) {
      return command;
    }
  }

  return undefined;
};

export const transformLineNodes = (lines: ElementContent[]) => {
  lines.forEach((line) => {
    if (!isElement(line)) {
      return;
    }

    const command = popCommand(line.children);
    if (command === "highlight") {
      classnames(line, "highlight");
    }
  });
};

export const rehypeShikiCommands: RehypePlugin = () => {
  const visitor: Visitor = (node, _index, _parent) => {
    if (!isElement(node, "code")) {
      return;
    }

    transformLineNodes(node.children);
  };

  return (tree, _file) => {
    visit(tree, visitor);
  };
};
