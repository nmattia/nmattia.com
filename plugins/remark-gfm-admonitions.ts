/* Plugin that adds support for GitHub-style admonitions in markdown:
 * https://github.com/orgs/community/discussions/16925
 */

import type { RemarkPlugin } from "@astrojs/markdown-remark";

import type * as Mdast from "mdast";

/* "Note" icon from GitHub
 * https://github.com/orgs/community/discussions/16925
 * modifications:
 * - removed classes
 * - added file="currentColor"
 */
const icon: string = `
<svg viewBox="0 0 16 16" version="1.1" width="16" height="16" fill="currentColor" aria-hidden="true"><path d="M0 8a8 8 0 1 1 16 0A8 8 0 0 1 0 8Zm8-6.5a6.5 6.5 0 1 0 0 13 6.5 6.5 0 0 0 0-13ZM6.5 7.75A.75.75 0 0 1 7.25 7h1a.75.75 0 0 1 .75.75v2.75h.25a.75.75 0 0 1 0 1.5h-2a.75.75 0 0 1 0-1.5h.25v-2h-.25a.75.75 0 0 1-.75-.75ZM8 6a1 1 0 1 1 0-2 1 1 0 0 1 0 2Z"></path></svg>
`;

/*
 * Try to read an admonition from the first child of a blockquote. The first child
 * must be a paragraph with a single text child, "[!<admonition>]". If an admonition
 * is found, the text child is removed.
 * Example: the following returns "DISCLAIMER":
{
  type: 'blockquote',
  children: [
    { type: 'paragraph', children: [ { type: 'text', value: '[!DISCLAIMER]' } ] },
    ...
  ],
  ...
}
*/
const popAdmonition = (element: Mdast.Blockquote): string | undefined => {
  // Go through the element's first child, and if paragraph, the paragraph's first child.

  if (!element.children || element.children.length <= 1) {
    return;
  }

  const firstChild = element.children[0];
  if (
    firstChild.type !== "paragraph" ||
    !firstChild.children ||
    firstChild.children.length !== 1
  ) {
    return;
  }

  const firstText = firstChild.children[0];
  if (firstText.type !== "text") {
    return;
  }

  const txt = firstText.value;

  // Try to parse as an "admonition"
  if (!txt.startsWith("[!") || !txt.endsWith("]")) {
    return;
  }

  const admonition = txt.slice(2, -1);
  element.children.shift(); // remove element

  return admonition;
};

/* Read the markdown nodes, and modify the HTML properties of blockquotes with admonitions.
 * For more information on HTML properties (hProperties) see:
 *   https://github.com/syntax-tree/mdast-util-to-hast?tab=readme-ov-file#fields-on-nodes
 *
 * For the markdown AST, refer to:
 *   https://github.com/syntax-tree/mdast?tab=readme-ov-file#nodes-abstract
 */
export const remarkGfmAdmonitions: RemarkPlugin = () => {
  /* some constants that might be configurable some day */
  const ADMONITION_CLASSNAME = "admonition"; // Set on the blockquote
  const ADMONITION_HEADER_CLASSNAME = "admonition-header"; // Set on the first paragraph of the (resulting) blockquote

  return (tree, _file) => {
    if (!tree.children) {
      // nothing to do
      return;
    }

    for (const content of tree.children) {
      if (content.type !== "blockquote") {
        continue;
      }

      let txt = popAdmonition(content);

      if (!txt) {
        continue;
      }

      // Capitalize the text correctly
      // NOTE -> Note
      txt = txt.charAt(0) + txt.toLowerCase().slice(1);

      content.data = content.data ?? {};
      content.data.hProperties = content.data.hProperties ?? {};

      content.data.hProperties.className = ADMONITION_CLASSNAME;

      const newChild: Mdast.Html = {
        type: "html",
        value: `<span class="${ADMONITION_HEADER_CLASSNAME}">${icon} ${txt}</span>`,
      };

      content.children.unshift(newChild);
    }
  };
};
