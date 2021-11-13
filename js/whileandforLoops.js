let numSeeds = 4; 
let monthNumber = 1;
let monthsToPrint = 12; 

while (monthNumber <= 
    monthsToPrint) {
        numSeeds *= 4;
        console.log("Plant "
        + numSeeds + " seeds in " + monthNumber + " month(s)!"); monthNumber++; 
    }