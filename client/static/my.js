
bubble = function(elementId) {
  elementToBubble = document.getElementById(elementId);
  parent = elementToBubble.parentElement;
  children = parent.children;
  for (var i = 0; i < children.length; i++) {
    if (children[i].id == elementId) {
      children[i].style.zIndex = 1;
    }
    else {
      children[i].style.zIndex = 0;      
    }
  }
}