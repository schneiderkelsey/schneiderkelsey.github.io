var navClasses = document.getElementById('navBar').classList;

function downAction() {
    navClasses.remove('open');
    navClasses.add('collapse');
  }
  
  function upAction() {
    navClasses.remove('collapse');
    navClasses.add('open');
  }
  var scrollTop = function() {
	return window.scrollY;
};
var scrollState = 0;
  window.addEventListener("scroll", function() {
    scrollDetect(homeAction, downAction, upAction);
  });