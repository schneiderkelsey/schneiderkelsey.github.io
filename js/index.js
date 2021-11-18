const express = require('express');
const app = express();
const part = 3000;
const path = require('path'); 
let contractRepo = require('./repos/contractRepo'); 

let contracts = constractRepo.get(); 

// Handle JSON requests 
app.use(express.json());

// Automatically return static files 
app.use(express.statis('public')); 

// Use routher object 
let router = express.Router(); 

// Routes 
router.get('/', (req, res) => {
    res.status(200).sendFile(path.join(_dirname, "template.html")); 
});
app.post('/', (req, res) => {
    res.send("Received POST request");
})

router.get('/request', (req, res) => {
    res.status(200).sendFile(path.join(__dirname, "request.html"));
});
app.post('/request', (req, res) => {
    const body = req.body; 
    res.send(body); 
}); 

router.get('/about', (req, res) => {
    res.status(200).sendFile(path.join(__dirname, "about.html"));
});

router.get('/login', (res, res) => {
    res.status(200).sendFile(path.join(__dirname, "about.html"));
});

app.listen(port, () => {
    console.log('Server listening at http://localhost:${port}');
})