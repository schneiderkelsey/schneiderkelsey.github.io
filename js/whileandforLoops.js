let numSeeds = 2; 
let dayNumber = 1;
let daysToPrint = 12; 

while (dayNumber <= 
    daysToPrint) {
        numSeeds *= 4;
        console.log("Plant "
        + numSeeds + " seeds in " + dayNumber + " day(s)!"); dayNumber++; 
    }