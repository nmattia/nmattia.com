import type { AstroIntegration } from "astro";

// Component that, if a #remark42 element exists on the page, registers an intersection observer to load
// remark42 when #remark42 is in the viewport.
//
// Adapted from: https://remark42.com/docs/configuration/frontend/

declare global {
  interface Window {
    remark_config: { host: string; site_id: string; no_footer?: boolean };
  }
}

// Add the plugin to the config and inject a hydration script
export const remark42Integration = (): AstroIntegration => {
  return {
    name: "remark42",
    hooks: {
      "astro:config:setup": ({ injectScript }) => {
        // Function added to every page that checks for a #remark42 element, and if found,
        // loads remark42.
        const f = () => {
          // Needed by the "embed" remark42 component
          window.remark_config = {
            host: "https://comments.nmattia.com",
            site_id: "remark" /* default */,
            no_footer:
              true /* the remark42 footer clashes with existing footer */,
          };
          // Event dispatched by the ClientRouter on page load:
          // https://docs.astro.build/en/guides/view-transitions/#astropage-load
          document.addEventListener("astro:page-load", () => {
            const remark42Elem = document.querySelector("#remark42");
            if (!remark42Elem) {
              return;
            }

            const loadRemark = () => {
              // Copied from https://remark42.com/docs/configuration/frontend/ and prettified + simplified
              // (only loads the "embed" component)
              const r = document.createElement("script");
              let ext = ".js";
              if ("noModule" in r) {
                r.type = "module";
                ext = ".mjs";
              }
              const d = document.head || document.body;

              r.async = true;
              r.defer = true;

              r.src = window.remark_config.host + "/web/embed" + ext;

              d.appendChild(r);
            };

            if (window.location.hash) {
              // If a hash is set, this might be a link to a comment, so load straight away and go to hash
              loadRemark();
              const h = window.location.hash;
              window.location.hash = "";
              window.location.hash = h;
            } else {
              // Otherwise delay until user reaches the remark42 div
              const observer = new IntersectionObserver(
                (entries) => {
                  const entry = entries[0];
                  if (!entry || !entry.isIntersecting) {
                    return;
                  }

                  loadRemark();

                  // Once we've loaded remark42, we can stop observing
                  observer.disconnect();
                },
                {
                  /* Watch the viewport and trigger as soon as the remark42 div enters */
                  root: null,
                  rootMargin: "0px",
                  threshold: 0,
                },
              );
              observer.observe(remark42Elem);
            }
          });
        };

        injectScript("page", `(${f.toString()})();`);
      },
    },
  };
};
