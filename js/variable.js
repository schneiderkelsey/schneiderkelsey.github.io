const buttonA = document.querySelector('#button_A');
const headingA = document.querySelector('#heading_A');

buttonA.onclick = function() {
  let name = prompt('What is your name?');
  alert('Hello ' + name + ', welcome to my Wildflower garden!');
  headingA.textContent = 'Welcome ' + name;
}
