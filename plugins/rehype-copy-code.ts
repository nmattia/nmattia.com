import type { RehypePlugin } from "@astrojs/markdown-remark";
import type { Visitor } from "unist-util-visit";
import type * as Hast from "hast";
import { fromHtml } from "hast-util-from-html";
import { isElement } from "hast-util-is-element";
import { SKIP, visit } from "unist-util-visit";
import type { AstroIntegration } from "astro";

/* Astro integration that adds a "copy-to-clipboard" button to code snippets. */

// The clipboard icon (as a Hast element)
const clipboardIcon: Hast.Element = fromHtml(
  `<svg aria-hidden="true" height="16" viewBox="0 0 16 16" version="1.1" width="16" > <path fill-rule="evenodd" d="M0 6.75C0 5.784.784 5 1.75 5h1.5a.75.75 0 010 1.5h-1.5a.25.25 0 00-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 00.25-.25v-1.5a.75.75 0 011.5 0v1.5A1.75 1.75 0 019.25 16h-7.5A1.75 1.75 0 010 14.25v-7.5z"></path><path fill-rule="evenodd" d="M5 1.75C5 .784 5.784 0 6.75 0h7.5C15.216 0 16 .784 16 1.75v7.5A1.75 1.75 0 0114.25 11h-7.5A1.75 1.75 0 015 9.25v-7.5zm1.75-.25a.25.25 0 00-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 00.25-.25v-7.5a.25.25 0 00-.25-.25h-7.5z"></path> </svg>`,
).children[0] as Hast.Element;

// Return true iff the hast element has the specified class
const hasClass = (elem: Hast.Element, klass: string): boolean => {
  const props = elem.properties;
  if (!props) {
    return false;
  }

  const className = props.className;
  if (!className) {
    return false;
  }

  if (!Array.isArray(className) || !className.includes(klass)) {
    return false;
  }

  return true;
};

// A rehype plugin that wraps <pre><code> blocks with a .code-wrapper (helps with scrolling)
// and adds a "copy to clipboard" button to the top-right corner.
export const rehypeCopyCode: RehypePlugin = () => {
  let ix = 0;

  const visitor: Visitor = (node, _index, _parent) => {
    if (
      !isElement(node) ||
      node.tagName !== "pre" ||
      !hasClass(node, "astro-code")
    ) {
      return;
    }

    const children = node.children;
    const codeNodeId = ix++;

    // Wrap the '<code>' tag with a div that contains the (potentially overflowing) <code>.
    // By wrapping the code in another element we ensure that the wrapper's sibling (the copy button)
    // does not scroll along with the code but stays static.
    const codeWrapper: Hast.Element = {
      type: "element",
      tagName: "div",
      properties: {
        className: ["code-wrapper"],
        // XXX: technically the code node is a child of this element, but for using .innerText
        // having it on the parent is fine too (and it's easier).
        dataCodeNodeId: codeNodeId,
      },
      children,
    };

    const copyButton: Hast.Element = {
      type: "element",
      tagName: "button",
      properties: {
        className: ["clipboard-copy"],
        dataAction: "copy",
        dataCopyTarget: codeNodeId,
      },
      children: [clipboardIcon],
    };

    node.children = [codeWrapper, copyButton];

    // Avoid processing the new children recursively
    return SKIP;
  };

  return (tree, _file) => {
    visit(tree, visitor);
  };
};

// Add the plugin to the config and inject a hydration script
export const rehypeCopyCodeIntegration = (): AstroIntegration => {
  return {
    name: "rehype-copy-code",
    hooks: {
      "astro:config:setup": ({ updateConfig, injectScript }) => {
        updateConfig({
          markdown: {
            rehypePlugins: [rehypeCopyCode],
          },
        });

        // Function added to every page that looks for all elements with action "copy", and
        // add a click listener to them that reads the innerText of their copyTarget (through
        // data-code-node-id).
        const f = () =>
          window.addEventListener("load", () => {
            const copyButtons = document.querySelectorAll(
              '[data-action="copy"]',
            );
            copyButtons.forEach((button) => {
              if (!(button instanceof HTMLElement)) {
                return;
              }

              button.addEventListener("click", async () => {
                const targetId = button.dataset.copyTarget;
                if (!targetId) {
                  return;
                }

                const target = document.querySelector(
                  `[data-code-node-id="${targetId}"]`,
                );
                if (!target || !(target instanceof HTMLElement)) {
                  return;
                }
                await navigator.clipboard.writeText(target.innerText);
              });
            });
          });

        injectScript("page", `(${f.toString()})()`);
      },
    },
  };
};
