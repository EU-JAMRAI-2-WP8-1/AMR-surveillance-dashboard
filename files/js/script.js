/**/

hideShow = function(buttonId, elementId) {
    if (document.getElementById(elementId).style.display == "none") {
        document.getElementById(elementId).style.display = "block";
        document.getElementById(buttonId).innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-chevron-down" viewBox="0 0 16 16"><path fill-rule="evenodd" d="M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708"/></svg>'
    } else {
        document.getElementById(elementId).style.display = "none";
        document.getElementById(buttonId).innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-chevron-right" viewBox="0 0 16 16"><path fill-rule="evenodd" d="M4.646 1.646a.5.5 0 0 1 .708 0l6 6a.5.5 0 0 1 0 .708l-6 6a.5.5 0 0 1-.708-.708L10.293 8 4.646 2.354a.5.5 0 0 1 0-.708"/></svg>'
    }
};

selectAllCountries = function() {
    var checkboxes = document.getElementsByName('countriesSelection');
    var allChecked = true;
    for (var i = 0; i < checkboxes.length; i++) {
        if (checkboxes[i].checked == false) {
            checkboxes[i].checked = true;
            allChecked = false;
        }
    }
    if (allChecked) {
        for (var i = 0; i < checkboxes.length; i++) {
            checkboxes[i].checked = false;
        }
    }
};

selectAllPathogens = function() {
    var checkboxes = document.getElementsByName('pathogensSelection');
    var allChecked = true;
    for (var i = 0; i < checkboxes.length; i++) {
        if (checkboxes[i].checked == false) {
            checkboxes[i].checked = true;
            allChecked = false;
        }
    }
    if (allChecked) {
        for (var i = 0; i < checkboxes.length; i++) {
            checkboxes[i].checked = false;
        }
    }
};

selectAllAntibiotics = function() {
    var checkboxes = document.getElementsByName('antibioticsSelection');
    var allChecked = true;
    for (var i = 0; i < checkboxes.length; i++) {
        if (checkboxes[i].checked == false) {
            checkboxes[i].checked = true;
            allChecked = false;
        }
    }
    if (allChecked) {
        for (var i = 0; i < checkboxes.length; i++) {
            checkboxes[i].checked = false;
        }
    }
};

selectAllSections = function() {
    var checkboxes = document.getElementsByName('sectionsSelection');
    var allChecked = true;
    for (var i = 0; i < checkboxes.length; i++) {
        if (checkboxes[i].checked == false) {
            checkboxes[i].checked = true;
            allChecked = false;
        }
    }
    if (allChecked) {
        for (var i = 0; i < checkboxes.length; i++) {
            checkboxes[i].checked = false;
        }
    }
};
