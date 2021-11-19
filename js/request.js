'use strict';

// Import logic
import { getQuote } from "./requestFunctions.mjs";

document.getElementById('order').onsubmit = function(event) {
    event.preventDefault(); // Prevent default behavior
    const formData = new FormData(document.getElementById('order'));
    let quoteValue = getQuote(formData.get("numPlayers"), formData.get("duration"));
    let orderObj = {
        name: formData.get("name"),
        email: formData.get("email"),
        comments: formData.get("comments"),
        genre: formData.get("genre"),
        numPlayers: formData.get("numPlayers"),
        duration: formData.get("duration"),
        quote: quoteValue
    }
    console.log(orderObj);
    axios
        .post('/request', orderObj)
        .then((response) => {
            console.log(response);
            document.body.innerHTML = "Request saved!";
        })
        .catch((error) => {
            console.log(error);
        });
};