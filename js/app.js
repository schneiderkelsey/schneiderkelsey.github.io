function range(start, end, step = 1) {
  
    const allNumbers = [start, end, step].every(Number.isFinite);
  
    if (!allNumbers) {
      throw new TypeError('range() expects only finite numbers as arguments.');
    }
    
    if (step <= 0) {
      throw new Error('step must be a number greater than 0.');
    }
    
    if (start > end) {
      step = -step;
    }
    
    const length = Math.floor(Math.abs((end - start) / step)) + 1;
    
    return Array.from(Array(length), (x, index) => start + index * step);
    
  }
  
  console.log(range(0, 5));
  console.log(range(0, 5, 0.5));
  console.log(range(1, 5, 0.75));
  console.log(range(10, 0, 2));
  console.log(range(15, 1, 2.25));