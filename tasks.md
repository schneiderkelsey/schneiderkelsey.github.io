#JavaScript Project Managing Flights Using JavaScript Syntax and Operators

## Short Description

This is a sample project to learn how JavaScript Syntax and Operators work. For this project, you'll be working with Visual Studio Code and implement a series of tasks to manage the flights and passengers of a flights management company. 


#Module 2
Manage flights and passengers

#Task 1
Calculate the number of flights with a given capacity that are needed to transport a given number of passengers. 

In flights.js add a function called calculateNumberOfFlights that has as parameters the number of passengers and the flight capacity.
If the number of passengers is multiple of the capacity, you will need a number of flights equal to the number of passengers divided by the capacity. Otherwise, you will need a number of flights equal to the number of passengers divided by the capacity plus 1.
For example, to transport 1000 passengers with flights having capacities of 100, you will need 1000/100 = 10 flights.
To transport 1001 passengers with flights having capacities of 100, you will need 1001/100 + 1 = 11 flights.

After the definition of the calculateNumberOfFlights function, add this line of code that will export the function:
return {calculateNumberOfFlights}; 
At the end of the flights.js file, add this line of code to export the module:
module.exports = Flights();

Objective:
Utilize Control Flow and Error Handling
Test:
@calculate_flights

#Task 2
Restrict the passengers number and the capacity of a flight to be positive integer numbers. No matter if the input is correct or not, log in the console the values for the number of passengers and the capacity that have been introduced.

In flights.js, modify the calculateNumberOfFlights function to:
- check if the passengers number is a positive integer. If it is not a positive integer, you should throw an error saying "The number of passengers must be a positive integer value"
- check if the capacity is a positive integer. If it is not a positive integer, you should throw an error saying "The capacity of the flight must be a positive integer value"

In frontend.js update the onCalculateNumberOfFlights function so that, after the try/catch sequence, to add a finally block to log in the console the values for the number of passengers and the capacity that have been introduced. 

Check to see that the functionality "Calculate the number of needed flights to carry a given number of passengers" from the front end is running fine.

Objectives:
Utilize Control Flow and Error Handling
Throw exceptions using throw
Construct exception handlers with try...catch...finally
Test:
@check_exceptions

#Task 3
Check the aicrafts revision according to the distance they have flown. 

In flights.js, add a function checkAircraftRevision having as parameters the distance limit and a distances array.
Calculate the total distance as the sum of the distances in the array.
If the total distance is less than half of the distance limit, you return a string warning that the revision needs to be done within the next 3 months. If the total distance is between half and 3 quarters of the distance limit, you return a string warning that the revision needs to be done within the next 2 months. If the total distance is between 3 quarters of the distance limit and the distance limit, you return a string warning that the revision needs to be done within 1 month. If the distance limit is exceeded, you throw an error.
After the definition of the checkAircraftRevision function, a line of code will export the functions from the module:
return {calculateNumberOfFlights, checkAircraftRevision}; 

Check to see that the functionality "Check the aircraft revision" from the front end is running fine.

Objectives:
Utilize Control Flow and Error Handling
Use for...of to loop through iterable objects
Throw exceptions using throw
Construct exception handlers with try...catch...finally
Test:
@check_flights_revision

#Task 4
Check that the capacity of the flight is not exceeded by the number of passengers.

In passengers.js, write a checkFlightCapacity function that gets as parameters the flight capacity and and array of passengers numbers. Calculate the total number of passengers by adding each number of passengers from the array. If the total number of passengers is less that the capacity of the flight, return it. If there are more passengers than the flight capacity, you should throw an error.

Export the checkFlightCapacity function and the Passengers module itself.

Check to see that the functionality "Check the flight capacity" from the front end is running fine.

Objectives:
Utilize Control Flow and Error Handling
Use for...of to loop through iterable objects
Throw exceptions using throw
Construct exception handlers with try...catch...finally
Test:
@check_flight_capacity

#Task 5
Distribute a given number of VIP and regular passengers to a given number of flights, each of these flights having the same given number of business seats and the same given number of economy seats.

In passengers.js, write a distributeAllSeatsToAllPassengers function that receives as parameters the number of VIP passengers, the number of  regular passengers, the number of flights, the number of business seats per flight, and the number of economy seats per flight. It returns an object containing the following fields: VIP passengers with business seats; VIP passengers with economy seats; regular passengers with business seats; regular passengers with economy seats.
The distribution rules need to be followed in this order:
1) First, assign VIP passengers to business seats, until either the VIP passengers or the business seats are consumed.
2) Then, if there are still VIP passengers, assign them to economy seats, until either the VIP passengers or the economy seats are consumed.
3) Then, if there are still business seats, assign regular passengers to business seats, until either the regular passengers or the business seats are consumed.
4) Then, if there are still economy seats, assign regular passengers to economy seats, until either the regular passengers or the economy seats are consumed.
You have to assign as many passengers as possible. There may be either passengers or seats that are left. 

After the definition of the distributeAllSeatsToAllPassengers function, a line of code will export the functions from the module:
return {calculateNumberOfFlights, checkAircraftRevision}; 

Objectives:
Utilize Control Flow and Error Handling
Control looping with break, continue and labeled statements
Test:
@check_seats_distribution

#Task 6
Check the total number of distributed passengers to be the sum of the VIP passengers distributed to business seats, the VIP passengers distributed to economy seats, the regular passengers distributed to business seats, and the regular passengers distributed to economy seats.

In util.js, write a function calculateTotalDistributedPassengers that will receive as input the object returned by the distributeAllSeatsToAllPassengers function from passengers.js. It will add all these passengers and get the total number of passengers.

Export the calculateTotalDistributedPassengers function and the Util module itself.

Check to see that the functionality "Distribute the seats by passenger type and seat type" from the front end is running fine.

Objectives:
Utilize Control Flow and Error Handling
Use for...in to loop through enumerable properties
Test:
@check_total_seats_distribution

#Task 7
Calculate the total number of passengers from a set of flights by adding the number of the passengers of each flight. 

In util.js, write a function calculateTotalNumberOfPassengers having as argument a passengers array and that will return the total number of passengers.

Export the calculateTotalNumberOfPassengers function.

Check to see that the functionality "Calculate the total number of passengers" from the front end is running fine.

Objectives:
Utilize Control Flow and Error Handling
Use for...of to loop through iterable objects
Test:
@check_total_passengers


#Module 3
Manage prices and distances

#Task 8
Calculate the final price of a ticket starting from a base price and applying variations in percents. 

In price.js, write a calculateFinalPrice function that receives as parameters the base price and the variations depending on the passenger type and on the flight type and that calculates the final price.
Examples:
The base price is 100, there is a reduction depending on the passenger type pf 5%, there is a reduction depending on the flight type of 3 percent.
The final price is 100 - 5% * 100 - 3% * 100 = 92.15
The base price is 100, there is an increase depending on the passenger type pf 5%, there is a reduction depending on the flight type of 3 percent.
The final price is 100 + 5% * 100 - 3% * 100 = 101.85

Export the calculateFinalPrice function and the Prices module itself.

Objective:
Utilize Control Flow and Error Handling
Test:
@calculate_ticket_final_price

#Task 9
Check the input for calculating the final price from task 8 to be non empty and to be a number. 

In util.js, write a checkInput function that receives the input as an argument. If the input is empty or it is not a number, throw an exception. Rely on the fact that an empty input has the falsy value.

Export the checkInput function.

Check to see that the functionality "Calculate the final price of a ticket starting from a base price and applying variations in percents" from the front end is running fine.

Objectives:
Throw exceptions using throw
Distinguish between truthy and falsy values
Test:
@check_exceptions_wrong_input

#Task 10
Calculate the default final prices for possible combinations passenger type/flight type.

In price.js, write a calculateDefaultFinalPrice function that receives as input the base price, the passenger type and the flight type.

There are two types of passengers, regular and VIP. There are two types of flights, economy and business. There are default values for the increases or reductions depending on the passenger and flight type, as following:
- Regular passenger: -5%
- VIP passeger: +5%
- Economy flight: -3%
- Business flight: +10%
Calculate the default final prices for all 4 possible combinations:
1) Regular passenger, economy flight
2) Regular passenger, business flight
3) VIP passenger, economy flight
4) VIP passenger, business flight
Example:
For a base price of 100, a regular passenger and a business flight, the default final price will be:
100 - 5% * 100 + 10% * 100 = 104.50

Export the calculateDefaultFinalPrice function.

Objective:
Construct switch statements
Test:
@calculate_ticket_default_final_price

#Task 11
Calculate the final prices for a given number of seats, considering the default reductions/increases for the passenger and flight type.

In price.js, write a calculateTotalFinalPrice function that receives as input the number of seats, the passenger type, the flight type and the base price. It will multiply the number of seats with the result of the calculateDefaultFinalPrice function from task 10.

Example:
For 5 seats, a base price of 100, regular passengers and an economy flight, the total final price will be:
5 * (100 - 5% * 100 - 3% * 100) = 460.75

Export the calculateTotalFinalPrice function.

Check to see that the functionality "Calculate the total final price of tickets by flight types and passenger types" from the front end is running fine.

Objectives:
Utilize Control Flow and Error Handling
Test:
@calculate_tickets_total_final_price

#Task 12
Calculate the total distance covered by a passenger or by an aircraft. 

In util.js, write a calculateTotalDistance function that receives as argument an array of distances and will calculate the total. Each negative distace will be ignored, being considered a wrong value.

Export the calculateTotalDistance function.

Check to see that the functionality "Calculate the total distance" from the front end is running fine.

Objectives:
Utilize Control Flow and Error Handling
Use for...of to loop through iterable objects
Control looping with break, continue and labeled statements
Test:
@check_total_distance

#Task 13
Calculate the bonus awarded to a passenger depending on the distance covered in business seats and in economy seats. 

In util.js, write a calculateBonusPoints function that will receive as arguments an array of distances covered in business seats,  an array of distances covered in economy seats, a business bonus percent and an economy bonus percent and will calculate the awarded bonus points.

Each total distance is calculated with the function calculateTotalDistance from task 12. The business bonus percent will be multiplied with the total distance covered in business seats to give the business points. The economy bonus percent will be multiplied with the total distance covered in economy seats to give the economy points. The total points are the sum of the business points and the economy points.

Export the calculateBonusPoints function.

Check to see that the functionality "Calculate the total distance" from the front end is running fine.

Objectives:
Utilize Control Flow and Error Handling
Test:
@check_bonus

#Task 14
Add "use strict" to all JavaScript files modified for the project logic (flights.js, passengers.js, price.js, util.js). With strict mode you will not be able, for example, to use  variables that are undeclared. Check that the code is correctly working in "strict mode". Fix the possible errors that may be generated. 
Objectives:
Understand "use strict" usage and errors it generates
Test:
All previously defined tests