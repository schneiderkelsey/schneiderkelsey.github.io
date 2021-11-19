const express = require('express');
const app = express();
const port = 3000;
const path = require('path');
const bodyParser = require('body-parser');
const { builtinModules } = require('module');
const indexLogic = require('./indexLogic');

// Sqlite3 initialization
var sqlite3 = require('sqlite3').verbose();
var db = new sqlite3.Database('db//orders.db');
const dbFields = ['name', 'email', 'genre', 'numPlayers', 'duration', 'quote'];
indexLogic.initDB(db, dbFields);

// Handle JSON requests
app.use(express.json());

app.use(express.urlencoded({ extended: true }));

// Automatically return static files
app.use(express.static("public"));

// Routes
app.get('/', (req, res) => {
    res.status(200).sendFile(path.join(__dirname, "index.html"));
});
app.post('/', (req, res) => {
    res.send("Received POST request");
});

app.get('/request', (req, res) => {
    res.status(200).sendFile(path.join(__dirname, "request.html"));
});
app.post('/request', (req, res) => {
    const order = req.body;
    db.run(`INSERT INTO orders(name, email, genre, numPlayers, duration, quote)
            VALUES ("${order.name}", "${order.email}", "${order.genre}", "${order.numPlayers}", "${order.duration}", "${order.quote}")`);
    res.send(`Added new order`);
});

app.get('/request/orderData', (req, res) => {
    // Read db file
    let orders = [];
    let currentOrder = {};
    db.all(`SELECT * FROM orders`, (err, rows) => {
        if (rows) {
            // Parse each row
            rows.forEach((row) => {
                currentOrder = {};
                currentOrder.name = row.name;
                currentOrder.email = row.email;
                currentOrder.genre = row.genre;
                currentOrder.numPlayers = row.numPlayers;
                currentOrder.duration = row.duration;
                currentOrder.quote = row.quote;
                orders.push(currentOrder);
            });
        }
        res.send(orders);
    });
});
// Delete all entries in order table
app.delete('/request/orderData', (req, res) =>  {
    db.run(`DELETE FROM orders`);
    res.status(204).send();
});

app.get('/about', (req, res) => {
    res.status(200).sendFile(path.join(__dirname, "about.html"));
});

app.get('/login', (req, res) => {
    res.status(200).sendFile(path.join(__dirname, "about.html"));
});

app.listen(port, () => {
    console.log(`Server listening at http://localhost:${port}`);
});