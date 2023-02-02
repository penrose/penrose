let uniqueIdCounter = 1;

export const makeIdsUnique = (
  svgElem: Element,
  onlyReferenced: boolean
): boolean => {
  const ID_SUFFIX = "--inject-";

  // Map of IRI referenceable tag names to properties that can reference them. This is defined in
  // https://www.w3.org/TR/SVG11/linking.html#processingIRI
  const IRI_TAG_PROPERTIES_MAP = {
    clipPath: ["clip-path"],
    "color-profile": null,
    cursor: null,
    filter: null,
    linearGradient: ["fill", "stroke"],
    marker: ["marker", "marker-end", "marker-mid", "marker-start"],
    mask: null,
    pattern: ["fill", "stroke"],
    radialGradient: ["fill", "stroke"],
  };

  const idSuffix = ID_SUFFIX + uniqueIdCounter++;
  // Regular expression for functional notations of an IRI references. This will find occurences in the form
  // url(#anyId) or url("#anyId") (for Internet Explorer) and capture the referenced ID
  const funcIriRegex = /url\("?#([a-zA-Z][\w:.-]*)"?\)/g;
  // Get all elements with an ID. The SVG spec recommends to put referenced elements inside <defs> elements, but
  // this is not a requirement, therefore we have to search for IDs in the whole SVG.
  const idElements = svgElem.querySelectorAll("[id]");
  let idElem;
  // An object containing referenced IDs  as keys is used if only referenced IDs should be uniquified.
  // If this object does not exist, all IDs will be uniquified.
  const referencedIds: number[] | null = onlyReferenced ? [] : null;
  let tagName;
  const iriTagNames = {};
  const iriProperties: string[] = [];
  let changed = false;
  let i, j;

  if (idElements.length) {
    // Make all IDs unique by adding the ID suffix and collect all encountered tag names
    // that are IRI referenceable from properities.
    for (i = 0; i < idElements.length; i++) {
      tagName = idElements[i].localName; // Use non-namespaced tag name
      // Make ID unique if tag name is IRI referenceable
      if (tagName in IRI_TAG_PROPERTIES_MAP) {
        iriTagNames[tagName] = 1;
      }
    }
    // Get all properties that are mapped to the found IRI referenceable tags
    for (tagName in iriTagNames) {
      (IRI_TAG_PROPERTIES_MAP[tagName] || [tagName]).forEach(function (
        mappedProperty: string
      ) {
        // Add mapped properties to array of iri referencing properties.
        // Use linear search here because the number of possible entries is very small (maximum 11)
        if (iriProperties.indexOf(mappedProperty) < 0) {
          iriProperties.push(mappedProperty);
        }
      });
    }
    if (iriProperties.length) {
      // Add "style" to properties, because it may contain references in the form 'style="fill:url(#myFill)"'
      iriProperties.push("style");
    }
    // Run through all elements of the SVG and replace IDs in references.
    // To get all descending elements, getElementsByTagName('*') seems to perform faster than querySelectorAll('*').
    // Since svgElem.getElementsByTagName('*') getElementsByTagName not return the svg element itself, we have to handle it separately.
    const descElements = svgElem.getElementsByTagName("*");
    let element: Element | null = svgElem;
    let propertyName;
    let value: string | null;
    let newValue;
    for (i = -1; element !== null; ) {
      if (element.localName === "style") {
        // If element is a style element, replace IDs in all occurences of "url(#anyId)" in text content
        value = element.textContent;
        newValue =
          value &&
          value.replace(funcIriRegex, function (_: string, id: string) {
            if (referencedIds) {
              referencedIds[id] = 1;
            }
            return "url(#" + id + idSuffix + ")";
          });
        if (newValue !== value) {
          element.textContent = newValue;
        }
      } else if (element.hasAttributes()) {
        // Run through all property names for which IDs were found
        for (j = 0; j < iriProperties.length; j++) {
          propertyName = iriProperties[j];
          value = element.getAttribute(propertyName);
          newValue =
            value &&
            value.replace(funcIriRegex, function (_: string, id: number) {
              if (referencedIds) {
                referencedIds[id] = 1;
              }
              return "url(#" + id + idSuffix + ")";
            });
          if (newValue && newValue !== value) {
            element.setAttribute(propertyName, newValue);
          }
        }
        // Replace IDs in xlink:ref and href attributes
        for (const refAttrName of ["xlink:href", "href"]) {
          let iri = element.getAttribute(refAttrName);
          if (iri && /^\s*#/.test(iri)) {
            // Check if iri is non-null and internal reference
            iri = iri.trim();
            element.setAttribute(refAttrName, iri + idSuffix);
            if (referencedIds) {
              // Add ID to referenced IDs
              referencedIds[iri.substring(1)] = 1;
            }
          }
        }
      }
      element = descElements.item(++i);
    }
    for (i = 0; i < idElements.length; i++) {
      idElem = idElements[i];
      // If set of referenced IDs exists, make only referenced IDs unique,
      // otherwise make all IDs unique.
      if (!referencedIds || referencedIds[idElem.id]) {
        // Add suffix to element's ID
        idElem.id += idSuffix;
        changed = true;
      }
    }
  }
  // return true if SVG element has changed
  return changed;
};
