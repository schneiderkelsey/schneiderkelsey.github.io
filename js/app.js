function getData() {
    const from = '2021-01-01';
    const to = '2021-01-08';
    const block = 24;
    fetch(`https://api.carbonintensity.org.uk/intensity/stats/${from}/${to}/${block}`)
      .then(response => {
        return response.json();
      })
      .then(parsedData => {
        const carbonData = parsedData.data.map((dataPoint) => {
          return { x: dataPoint.from, y: dataPoint.intensity.average };
        })
        const data = {
          datasets: [{
            label: 'UK Carbon Intensity',
            backgroundColor: 'rgb(255, 99, 132)',
            borderColor: 'rgb(255, 99, 132)',
            data: carbonData,
          }]
        };
        const config = {
          type: 'line',
          data: data,
          options: {}
        };
        var myChart = new Chart(
          document.getElementById('myChart'),
          config
        );
      })
      .catch(error => {
          console.log(error)
      });
  }