import type { RehypePlugin } from "@astrojs/markdown-remark";
import type { Visitor } from "unist-util-visit";
import { isElement } from "hast-util-is-element";
import { visit } from "unist-util-visit";

/* workaround for incorrect hast properties in MDX: https://github.com/withastro/astro/issues/13070 */

export const fixMdx: RehypePlugin = () => {
  const visitor: Visitor = (node, _index, _parent) => {
    if (!isElement(node)) {
      return;
    }

    if (node.properties.class && typeof node.properties.class === "string") {
      node.properties.className = node.properties.class.split(" ");
      delete node.properties.class;
    }
  };

  return (tree, _file) => {
    visit(tree, visitor);
  };
};
