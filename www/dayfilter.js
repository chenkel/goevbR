shinyjs.filterDays = function (params) {
    // console.log("YIPEEE");
    var defaultParams = {
        dtype: 1,
        weekdayId: 'weekdayOrig'
    };
    params = shinyjs.getParams(params, defaultParams);

    var moT = $("input[value='1'][name=" + params.weekdayId + "]");
    var diT = $("input[value='2'][name=" + params.weekdayId + "]");
    var miT = $("input[value='3'][name=" + params.weekdayId + "]");
    var doT = $("input[value='4'][name=" + params.weekdayId + "]");
    var frT = $("input[value='5'][name=" + params.weekdayId + "]");
    var saT = $("input[value='6'][name=" + params.weekdayId + "]");
    var soT = $("input[value='7'][name=" + params.weekdayId + "]");

    // console.log(params.dtype, "<-- params.dtype");
    switch (params.dtype) {
        case '0':
            console.log("case 0");
            checkDay(moT, false);
            checkDay(diT, false);
            checkDay(miT, false);
            checkDay(doT, false);
            checkDay(frT, false);
            checkDay(saT, false);
            checkDay(soT, false);
            break;
        case '1':
            checkDay(moT, true);
            checkDay(diT, true);
            checkDay(miT, true);
            checkDay(doT, true);
            checkDay(frT, true);
            checkDay(saT, true);
            checkDay(soT, true);
            break;
        case '2':
            checkDay(moT, true);
            checkDay(diT, true);
            checkDay(miT, true);
            checkDay(doT, true);
            checkDay(frT, true);
            checkDay(saT, false);
            checkDay(soT, false);
            break;
        case '3':
            checkDay(moT, false);
            checkDay(diT, false);
            checkDay(miT, false);
            checkDay(doT, false);
            checkDay(frT, false);
            checkDay(saT, true);
            checkDay(soT, true);
            break;
    }
};
