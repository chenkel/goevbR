$(document).ready(function () {
    console.log("app.js loaded");
    var popupTemplate =
        '<div class="modal fade">' +
        '  <div class="modal-dialog">' +
        '    <div class="modal-content">' +
        '      <div class="modal-header">' +
        '        <button type="button" class="close" data-dismiss="modal">&times;</button>' +
        '        <h4 class="modal-title">Tipps</h4>' +
        '      </div>' +
        '      <div class="modal-body">' +
        '           Um die Daten nach einer gewünschten Zeit zu filtern, kann man einer der folgenden Methoden anwenden:' +
        '           <br/>' +
        '           <br/>' +
        '           <ol>' +
        '               <li>Zeitbereich markieren, z. B.: 6 Uhr - 9 Uhr <b>[drag]</b></li>' +
        '               <li>Stunde wählen, z. B.: 15 Uhr <b>[click]</b></li>' +
        '               <li>Filter zurücksetzen, 0 Uhr - 23 Uhr <b>[double click]</b></li>' +
        '           </ol>' +
        '      </div>' +
        '      <div class="modal-footer">' +
        '        <button type="button" class="btn btn-link" data-dismiss="modal">Okay</button>' +
        '      </div>' +
        '    </div>' +
        '  </div>' +
        '</div>';

    $('i.fa.fa-question-circle').click(function () {
        $(popupTemplate).modal();
    })
});

var checkDay = function (jqEl, shouldBeSelected) {
    if (jqEl.is(':checked') && (!shouldBeSelected)) {
        jqEl.click()
    }
    if (!jqEl.is(':checked') && (shouldBeSelected)) {
        jqEl.click()
    }
};
