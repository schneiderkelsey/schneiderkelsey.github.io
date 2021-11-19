function Flights() {
    function calculateNumberOfFlights(numPassengers, capacity) {
        //1500.7
        if (numPassengers < 0 || numPassengers % 1 !== 0) {
            throw new Error(
                "The number of passengers must be a positive integer value"
            ); 
        }

        if (capacity < 0 || capacity % 1 !== 0) {
            throw new Error(
                "The capacity of the flight must be a positive integer value"
                );
        } 
        
        return Math.ceil(numPassengers / capacity);
    }

    function checkAircraftRevision(distanceLimit, distances) {
        let totalDistance = 0; 

        for (let i = 0; i < distances.length; i++) {
            totalDistance += distances[i];
        }

        //If the total distance is less or equal than half of the distance limit, you return a string "The revision needs to be done within the next 3 months".
        if (totalDistance <= distanceLimit / 2) {
            return "The revision needs to be done within the next 3 months";
        }

        //If the total distance is more than half but less or equal than 3 quarters of the distance limit, you return a string "The revision needs to be done within the next 2 months".
        if (
            totalDistance > distanceLimit / 2 && 
            totalDistance <= distanceLimit * 0.75 
        ) {
            return "The revision needs to be done within the next 2 months";
        }

        //If the total distance is more than 3 quarters of the distance limit but less or equal than the distance limit, you return a string "The revision needs to be done within the next month".
        if (
            totalDistance > distanceLimit * 0.75 && 
            totalDistance <= distanceLimit
         ) {
            return "The revision needs to be done within the next month"; 
        }

        //If the total distance is more than the distance limit, you throw an error.
        if (totalDistance > distanceLimit) {
            throw new Error("Total distance is more than the distance limit");
        }
    }

    return { calculateNumberOfFlights, checkAircraftRevision };
}

module.exports = Flights();
