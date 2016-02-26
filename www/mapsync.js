shinyjs.syncMap = function (mapId) {
    if (mapId == 'mapOrig') {
        if (typeof $("#mapDest").data('leaflet-map') !== 'undefined') {
            $("#mapOrig").data('leaflet-map').setView($("#mapDest").data('leaflet-map').getCenter(), $("#mapDest").data('leaflet-map').getZoom(), {
                animate: false,
                reset: true
            });
        }
    } else {

        $("#mapDest").data('leaflet-map').setView($("#mapOrig").data('leaflet-map').getCenter(), $("#mapOrig").data('leaflet-map').getZoom(), {
            animate: false,
            reset: true
        });

    }
};
