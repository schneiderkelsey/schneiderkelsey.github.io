// A function that hides or shows a selected element
function hideOrShow() {
	
  // Select the element with id "theDIV"
  var x = document.getElementById("theDIV");
  
  // If selected element is hidden
  if (x.style.display === "none") {
  
    // Show the hidden element
    x.style.display = "block";
    
    // Else if the selected element is shown
  } else {
  
    // Hide the element
    x.style.display = "none";
  }
}


