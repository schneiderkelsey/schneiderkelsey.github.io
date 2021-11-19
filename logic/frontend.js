"use strict"

var util = require('../logic/util');
var passengers = require('../logic/passengers');

function onCalculateNumberOfFlights() {
    let passengers = document.getElementById('passengers').value;
    let capacity = document.getElementById('capacity').value;
    try {
       let flights = Flights().calculateNumberOfFlights(passengers, capacity);
       document.getElementById('flights').innerHTML = "You will need " + flights +
          " flight(s) to carry " + passengers + " passengers";
    } catch (error) {
        document.getElementById('flights').innerHTML = error; 
    } finally {
        console.log (passengers, capacity);
    }
}

function onCalculateTotalFinalPrice(object) {
    switch(object.id) {
        case 'regulareconomy':
            Util().checkInput(document.getElementById('regularPassengersEconomyFlightBasePrice').value);
            Util().checkInput(document.getElementById('regularPassengersEconomyFlight').value);
            document.getElementById('regularPassengersEconomyFlightTotalPrice').innerHTML = 
                  Prices().calculateTotalFinalPrice(document.getElementById('regularPassengersEconomyFlight').value,
                          'regular', 'economy', 
                           document.getElementById('regularPassengersEconomyFlightBasePrice').value);
        break;
        case 'vipeconomy':
            Util().checkInput(document.getElementById('vipPassengersEconomyFlightBasePrice').value);
            Util().checkInput(document.getElementById('vipPassengersEconomyFlight').value);
            document.getElementById('vipPassengersEconomyFlightTotalPrice').innerHTML = 
                  Prices().calculateTotalFinalPrice(document.getElementById('vipPassengersEconomyFlight').value,
                          'vip', 'economy', 
                          document.getElementById('vipPassengersEconomyFlightBasePrice').value);
        break;
        case 'regularbusiness':
            Util().checkInput(document.getElementById('regularPassengersBusinessFlightBasePrice').value);
            Util().checkInput(document.getElementById('regularPassengersBusinessFlight').value);
            document.getElementById('regularPassengersBusinessFlightTotalPrice').innerHTML = 
                  Prices().calculateTotalFinalPrice(document.getElementById('regularPassengersBusinessFlight').value,
                           'regular', 'business', 
                           document.getElementById('regularPassengersBusinessFlightBasePrice').value);
        break;
        case 'vipbusiness':
            Util().checkInput(document.getElementById('vipPassengersBusinessFlightBasePrice').value);
            Util().checkInput(document.getElementById('vipPassengersBusinessFlight').value);
            document.getElementById('vipPassengersBusinessFlightTotalPrice').innerHTML = 
                  Prices().calculateTotalFinalPrice(document.getElementById('vipPassengersBusinessFlight').value,
                          'vip', 'business', 
                           document.getElementById('vipPassengersBusinessFlightBasePrice').value);
        break;
    }
}

function onCalculateFinalPrice() {
        let basePrice = document.getElementById("basePrice").value;
        let variationPassengerType = document.getElementById("variationPassengerType").value;
        let variationFlightType = document.getElementById("variationFlightType").value;
        Util().checkInput(basePrice);
        Util().checkInput(variationPassengerType);
        Util().checkInput(variationFlightType);

        let finalPrice = Prices().calculateFinalPrice(basePrice, variationPassengerType, variationFlightType);

        document.getElementById("calculatedFinalPrice").innerHTML = finalPrice;
}

function onDistributePassengers() {
    let vipPassengers = parseInt(document.getElementById("vipPassengers").value);
    let regularPassengers = parseInt(document.getElementById("regularPassengers").value);
    let nrOfFlights = parseInt(document.getElementById("nrOfFlights").value);
    let businessSeatsPerFlight = parseInt(document.getElementById("businessSeatsPerFlight").value);
    let economySeatsPerFlight = parseInt(document.getElementById("economySeatsPerFlight").value);

    let distributedPassengers = Passengers().distributeAllSeatsToAllPassengers(vipPassengers, regularPassengers, 
                                           nrOfFlights, businessSeatsPerFlight, economySeatsPerFlight);

    let totalDistributedPassengers = Util().calculateTotalDistributedPassengers(distributedPassengers);                                       

    document.getElementById("distributedPassengers").innerHTML = 
         "Total number of distributed passengers: " + totalDistributedPassengers + "<br>" +
         "VIP passengers distributed to business seats: " + distributedPassengers.vipPassengersWithBusinessSeats + "<br>" + 
         "VIP passengers distributed to economy seats: " + distributedPassengers.vipPassengersWithEconomySeats + "<br>" +
         "Regular passengers distributed to business seats: " + distributedPassengers.regularPassengersWithBusinessSeats + "<br>" +
         "Regular passengers distributed to economy seats: " + distributedPassengers.regularPassengersWithEconomySeats + "<br>";

}

function onCalculatePassengers() {
    let passengers1 = parseInt(document.getElementById("passengers1").value);
    let passengers2 = parseInt(document.getElementById("passengers2").value);
    let passengers3 = parseInt(document.getElementById("passengers3").value);
    let passengers4 = parseInt(document.getElementById("passengers4").value);
    let passengers5 = parseInt(document.getElementById("passengers5").value);

    let passengersArray = [passengers1, passengers2, passengers3, passengers4, passengers5];
    calculatedPassengers = Util().calculateTotalNumberOfPassengers(passengersArray);

    document.getElementById("calculatedPassengers").innerHTML = "Total number of passengers: " + calculatedPassengers;

}

function onCalculateTotalDistance() {
    let distance1 = parseInt(document.getElementById("distance1").value);
    let distance2 = parseInt(document.getElementById("distance2").value);
    let distance3 = parseInt(document.getElementById("distance3").value);
    let distance4 = parseInt(document.getElementById("distance4").value);
    let distance5 = parseInt(document.getElementById("distance5").value);

    let distancesArray = [distance1, distance2, distance3, distance4, distance5];
    calculatedTotalDistance = Util().calculateTotalDistance(distancesArray);

    document.getElementById("calculatedTotalDistance").innerHTML = "Total distance: " + calculatedTotalDistance;

}

function onCalculateBonusPoints() {

    let businessDistance1 = parseInt(document.getElementById("businessDistance1").value);
    let businessDistance2 = parseInt(document.getElementById("businessDistance2").value);
    let businessDistance3 = parseInt(document.getElementById("businessDistance3").value);
    let businessDistance4 = parseInt(document.getElementById("businessDistance4").value);
    let businessDistance5 = parseInt(document.getElementById("businessDistance5").value);
    let businessDistance6 = parseInt(document.getElementById("businessDistance6").value);

    let businessDistancesArray = [businessDistance1, businessDistance2, businessDistance3, businessDistance4, businessDistance5, businessDistance6];

    let economyDistance1 = parseInt(document.getElementById("economyDistance1").value);
    let economyDistance2 = parseInt(document.getElementById("economyDistance2").value);
    let economyDistance3 = parseInt(document.getElementById("economyDistance3").value);
    let economyDistance4 = parseInt(document.getElementById("economyDistance4").value);
    let economyDistance5 = parseInt(document.getElementById("economyDistance5").value);
    let economyDistance6 = parseInt(document.getElementById("economyDistance6").value);

    let economyDistancesArray = [economyDistance1, economyDistance2, economyDistance3, economyDistance4, economyDistance5, economyDistance6];

    let businessBonus = parseInt(document.getElementById("businessBonus").value);
    let economyBonus = parseInt(document.getElementById("economyBonus").value);

    calculatedBonusPoints = Util().calculateBonusPoints(businessDistancesArray, economyDistancesArray, 
                                                              businessBonus, economyBonus);

    document.getElementById("calculatedBonusPoints").innerHTML = "Bonus points: " + calculatedBonusPoints;

}

function onCheckFlightCapacity() {
    let flightCapacity = parseInt(document.getElementById("flightCapacity").value);

    let passengersFromFlight1 = parseInt(document.getElementById("passengersFromFlight1").value);
    let passengersFromFlight2 = parseInt(document.getElementById("passengersFromFlight2").value);
    let passengersFromFlight3 = parseInt(document.getElementById("passengersFromFlight3").value);
    let passengersFromFlight4 = parseInt(document.getElementById("passengersFromFlight4").value);
    let passengersFromFlight5 = parseInt(document.getElementById("passengersFromFlight5").value);

    let passengersArray = [passengersFromFlight1, passengersFromFlight2, passengersFromFlight3, passengersFromFlight4, passengersFromFlight5];
    try { 
        let passengersNumber = Passengers().checkFlightCapacity(flightCapacity, passengersArray);
        document.getElementById("checkedFlightCapacity").innerHTML = "Passengers number: " + passengersNumber;
    } catch (error) {
        document.getElementById('checkedFlightCapacity').innerHTML = error; 
    }

}

function onCheckAircraftRevision() {
    let distanceLimit = parseInt(document.getElementById("distanceLimit").value);

    let coveredDistance1 = parseInt(document.getElementById("coveredDistance1").value);
    let coveredDistance2 = parseInt(document.getElementById("coveredDistance2").value);
    let coveredDistance3 = parseInt(document.getElementById("coveredDistance3").value);
    let coveredDistance4 = parseInt(document.getElementById("coveredDistance4").value);
    let coveredDistance5 = parseInt(document.getElementById("coveredDistance5").value);

    let distancesArray = [coveredDistance1, coveredDistance2, coveredDistance3, coveredDistance4, coveredDistance5];
    try { 
        let answer = Flights().checkAircraftRevision(distanceLimit, distancesArray);
        document.getElementById("checkedAircraftRevision").innerHTML = answer;
    } catch (error) {
        document.getElementById('checkedAircraftRevision').innerHTML = error; 
    }

}