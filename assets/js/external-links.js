/* ==========================================================================
   Make external links in the LHS author profile open in a new tab
   ========================================================================== */

document.addEventListener("DOMContentLoaded", function () {
  // author profile links on the left
  var links = document.querySelectorAll(".author__urls a");

  links.forEach(function (link) {
    link.setAttribute("target", "_blank");
    link.setAttribute("rel", "noopener noreferrer");
  });
});