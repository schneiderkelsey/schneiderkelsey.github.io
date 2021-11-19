const buttonA = document.querySelector('#button_A');
const headingA = document.querySelector('#heading_A');

buttonA.onclick = function() {
  let name = prompt('Who sings the song, "Wildflowers"?');
  alert('You, ' + name + ', belong somewhere you feel free!');
  headingA.textContent = '&hearts; ' + name;
}
