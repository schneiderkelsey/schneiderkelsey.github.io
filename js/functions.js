let myVariable = document.querySelector('h1');
alert('You belong somewhere you feel free');

function multiply(num1,num2) {
    let result = num1 * num2;
    return result;
  }

let myButton = document.querySelector('button');
let myHeading = document.querySelector('h1');

function setUserName() {
    let myName = prompt('Please enter your name.');
    localStorage.setItem('name', myName);
    myHeading.textContent = 'You belong somewhere you feel free, ' + myName;
  }
  
  if(!localStorage.getItem('name')) {
    setUserName();
  } else {
    let storedName = localStorage.getItem('name');
    myHeading.textContent = 'You belong somewhere you feel free, ' + storedName;
  }



  function setUserName() {
    let myName = prompt('Please enter your name.');
    if(!myName) {
      setUserName();
    } else {
      localStorage.setItem('name', myName);
      myHeading.textContent = 'You belong somewhere you feel free, ' + myName;
    }
  }
  