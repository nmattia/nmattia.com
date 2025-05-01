import type { RehypePlugin } from "@astrojs/markdown-remark";
import type { Visitor } from "unist-util-visit";
import { classnames } from "hast-util-classnames";
import { isElement } from "hast-util-is-element";
import { visit } from "unist-util-visit";

/* This is a rehype plugin that marks paragraphs containing only math as "equations" so that
 * they can be handled differently. This is done by adding a class to the parent of the mathjax code
 * block. */
export const rehypeMathjaxEquation: RehypePlugin = () => {
  const visitor: Visitor = (node, _index, _parent) => {
    if (!isElement(node, "p")) {
      return;
    }

    if (node.children?.length !== 1) {
      return;
    }

    const child = node.children[0];
    if (!isElement(child)) {
      return;
    }

    const props = child.properties;
    if (
      !props ||
      !Array.isArray(props.className) ||
      !props.className.includes("math-inline")
    ) {
      return;
    }

    classnames(node, "math-equation-paragraph");
  };

  return (tree, _file) => {
    visit(tree, visitor);
  };
};
