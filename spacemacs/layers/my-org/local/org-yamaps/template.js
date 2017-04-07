ymaps.ready(init);

var yamaps = {};

String.prototype.hashCode = function() {
    var hash = 0, i, chr;
    if (this.length === 0) return hash;
    for (i = 0; i < this.length; i++) {
        chr   = this.charCodeAt(i);
        hash  = ((hash << 5) - hash) + chr;
        hash |= 0; // Convert to 32bit integer
    }
    return hash;
};

function init () {
    console.log(data);
    yamaps.myMap = new ymaps.Map("map", {
            center: [59.959525, 30.386539],
            zoom: 11
        }, {
            searchControlProvider: 'yandex#search'
        });

        for(var i = 0; i < data.length; i++) {
            addPoint(data[i]);
        }
}

function addPoint(point) {

    var colors = ["#ff0000", "#ff4000", "#ff8000", "#ffbf00", "#ffff00", "#bfff00",
                  "#80ff00", "#40ff00", "#00ff00", "#00ff40", "#00ff80", "#00ffbf",
                  "#00ffff", "#00bfff", "#0080ff", "#0040ff", "#0000ff", "#4000ff",
                  "#8000ff", "#bf00ff", "#ff00ff", "#ff00bf", "#ff0080", "#ff0040"];

    var color = colors[Math.abs(point.title.hashCode()) % colors.length];

    var myPlacemark = new ymaps.Placemark(point.latlng.split(",").map(parseFloat), {
        balloonContentHeader: point.title,
        balloonContentBody: point.body
        // hintContent: "Хинт метки"
    }, {
        preset: point.icon !== null ? point.icon : undefined,
        iconColor: color
    });

    yamaps.myMap.geoObjects.add(myPlacemark);
}
