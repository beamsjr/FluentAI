// Helper function to create DOM elements
function createElement(tag, props, children) {
  const element = document.createElement(tag);
  
  // Apply properties
  if (props) {
    Object.entries(props).forEach(([key, value]) => {
      if (key === 'style' && typeof value === 'object') {
        Object.assign(element.style, value);
      } else if (key.startsWith('on') && typeof value === 'function') {
        const eventName = key.slice(2).toLowerCase();
        element.addEventListener(eventName, value);
      } else if (key === 'className') {
        element.className = value;
      } else if (key !== 'children') {
        element.setAttribute(key, value);
      }
    });
  }
  
  // Add children
  if (children) {
    children.forEach(child => {
      if (child != null) {
        if (typeof child === 'string' || typeof child === 'number') {
          element.appendChild(document.createTextNode(child));
        } else if (child instanceof Node) {
          element.appendChild(child);
        }
      }
    });
  }
  
  return element;
}