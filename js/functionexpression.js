function grabcondition() {
    return Math.round(Math.
        random()) % 2 == 0;
}
let helloMessage; 
let condition = grabcondition(); 

if (condition) {
    helloMessage = function () {
        return 'hello from if';
    };
} else {
    helloMessage = function () {
        return 'hello from else'; 
    }; 
}

helloMessage(); 