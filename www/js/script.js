// send size of main div to parent window (iframe)
new ResizeObserver(
    () => window.parent.postMessage(
        {
            type: 'setHeight',
            height: document.body.querySelector("div").clientHeight
        },
        '*'
    )).observe(
        document.body.querySelector("div")
    );

// Handle info button hover to show tooltip only on question mark
$(document).on('mouseenter', '.info-button-inline', function(e) {
    e.stopPropagation();
    $(this).closest('div').find('.info-sections').addClass('show-info');
});

$(document).on('mouseleave', '.info-button-inline', function(e) {
    e.stopPropagation();
    $(this).closest('div').find('.info-sections').removeClass('show-info');
});

// Update filter progress bars based on selection count
$(document).on('shiny:connected', function() {

    // Function to update a progress bar
    function updateProgressBar(inputId, progressBarId, totalCount) {
        const selectedCount = $('#' + inputId + ' input:checked').length;
        const percentage = totalCount > 0 ? (selectedCount / totalCount) * 100 : 0;
        // Account for 12px margins on each side (24px total)
        $('#' + progressBarId).css('width', 'calc(' + percentage + '% - 24px)');
    }

    // Function to get total count for a checkbox group
    function getTotalCount(inputId) {
        return $('#' + inputId + ' input[type="checkbox"]').length;
    }

    // Initialize and update on change for Countries
    $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'countriesSelection') {
            const total = getTotalCount('countriesSelection');
            updateProgressBar('countriesSelection', 'progress-countries', total);
        }
        if (event.name === 'cultureMaterialsSelection') {
            const total = getTotalCount('cultureMaterialsSelection');
            updateProgressBar('cultureMaterialsSelection', 'progress-cultureMaterials', total);
        }
        if (event.name === 'pathogensSelection') {
            const total = getTotalCount('pathogensSelection');
            updateProgressBar('pathogensSelection', 'progress-pathogens', total);
        }
        if (event.name === 'resistancesSelection') {
            const total = getTotalCount('resistancesSelection');
            updateProgressBar('resistancesSelection', 'progress-resistances', total);
        }
    });

    // Initial update after a short delay to ensure DOM is ready
    setTimeout(function() {
        updateProgressBar('countriesSelection', 'progress-countries', getTotalCount('countriesSelection'));
        updateProgressBar('cultureMaterialsSelection', 'progress-cultureMaterials', getTotalCount('cultureMaterialsSelection'));
        updateProgressBar('pathogensSelection', 'progress-pathogens', getTotalCount('pathogensSelection'));
        updateProgressBar('resistancesSelection', 'progress-resistances', getTotalCount('resistancesSelection'));
    }, 500);
});
