// Update body class based on section selection (for color theming)
$(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'filters-sectionsSelection') {
        $('body').removeClass('section-1 section-2 section-3');
        if (event.value === 'National surveillance') {
            $('body').addClass('section-1');
        } else if (event.value === 'National genomic surveillance') {
            $('body').addClass('section-2');
        } else if (event.value === 'National guidance') {
            $('body').addClass('section-3');
        }
    }
});

// Set initial body class on page load
$(document).ready(function() {
    $('body').addClass('section-1');
});

// send size of main div to parent window (iframe)
function getDocumentHeight() {
    return Math.max(
        document.body.scrollHeight,
        document.body.offsetHeight,
        document.documentElement.clientHeight,
        document.documentElement.scrollHeight,
        document.documentElement.offsetHeight
    );
}

function sendHeightToParent() {
    window.parent.postMessage(
        {
            type: 'setHeight',
            height: getDocumentHeight()
        },
        '*'
    );
}

// Use ResizeObserver on the body to detect any layout changes
new ResizeObserver(sendHeightToParent).observe(document.body);

// Also send height on load and after potential dynamic content changes
window.addEventListener('load', sendHeightToParent);
document.addEventListener('DOMContentLoaded', sendHeightToParent);

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
        $('#' + progressBarId).css('width', 'calc(' + percentage + '%)');
    }

    // Function to get total count for a checkbox group
    function getTotalCount(inputId) {
        return $('#' + inputId + ' input[type="checkbox"]').length;
    }

    // Initialize and update on change for Countries
    $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'filters-countriesSelection') {
            const total = getTotalCount('filters-countriesSelection');
            updateProgressBar('filters-countriesSelection', 'progress-countries', total);
        }
        if (event.name === 'filters-cultureMaterialsSelection') {
            const total = getTotalCount('filters-cultureMaterialsSelection');
            updateProgressBar('filters-cultureMaterialsSelection', 'progress-cultureMaterials', total);
        }
        if (event.name === 'filters-pathogensSelection') {
            const total = getTotalCount('filters-pathogensSelection');
            updateProgressBar('filters-pathogensSelection', 'progress-pathogens', total);
        }
        if (event.name === 'filters-resistancesSelection') {
            const total = getTotalCount('filters-resistancesSelection');
            updateProgressBar('filters-resistancesSelection', 'progress-resistances', total);
        }
        if (event.name === 'insight_filters-cultureMaterialsSelection') {
            const total = getTotalCount('insight_filters-cultureMaterialsSelection');
            updateProgressBar('insight_filters-cultureMaterialsSelection', 'progress-insight-cultureMaterials', total);
        }
        if (event.name === 'insight_filters-pathogensSelection') {
            const total = getTotalCount('insight_filters-pathogensSelection');
            updateProgressBar('insight_filters-pathogensSelection', 'progress-insight-pathogens', total);
        }
        if (event.name === 'insight_filters-resistancesSelection') {
            const total = getTotalCount('insight_filters-resistancesSelection');
            updateProgressBar('insight_filters-resistancesSelection', 'progress-insight-resistances', total);
        }
    });

    // Initial update after a short delay to ensure DOM is ready
    setTimeout(function() {
        updateProgressBar('filters-countriesSelection', 'progress-countries', getTotalCount('filters-countriesSelection'));
        updateProgressBar('filters-cultureMaterialsSelection', 'progress-cultureMaterials', getTotalCount('filters-cultureMaterialsSelection'));
        updateProgressBar('filters-pathogensSelection', 'progress-pathogens', getTotalCount('filters-pathogensSelection'));
        updateProgressBar('filters-resistancesSelection', 'progress-resistances', getTotalCount('filters-resistancesSelection'));
        updateProgressBar('insight_filters-cultureMaterialsSelection', 'progress-insight-cultureMaterials', getTotalCount('insight_filters-cultureMaterialsSelection'));
        updateProgressBar('insight_filters-pathogensSelection', 'progress-insight-pathogens', getTotalCount('insight_filters-pathogensSelection'));
        updateProgressBar('insight_filters-resistancesSelection', 'progress-insight-resistances', getTotalCount('insight_filters-resistancesSelection'));
    }, 500);
});

// Handle clicks on geo data disclaimer link in map annotations
$(document).on('click', '.geo-disclaimer-link', function(e) {
    e.preventDefault();
    Shiny.setInputValue('showGeoDataDisclaimer', Math.random(), {priority: 'event'});
});

// Main landing page popup (R/mod_main_landing.R). Screens and card selection are
// handled here; only the final choice is sent to the server, as input$mainLanding.
(function() {
    var choice = { view: null, section: null };

    function landing() { return document.getElementById('main-landing'); }

    function showScreen(name) {
        $('#main-landing .main-landing-screen').each(function() {
            this.classList.toggle('active', this.dataset.screen === name);
        });
        $('#main-landing .main-landing-body').scrollTop(0);
    }

    function closeLanding() {
        var el = landing();
        if (el) el.classList.add('hidden');
    }

    function openWith(value) {
        closeLanding();
        Shiny.setInputValue('mainLanding', value, {priority: 'event'});
    }

    // Welcome screen choices and Back buttons
    $(document).on('click', '#main-landing [data-landing-screen]', function() {
        showScreen(this.dataset.landingScreen);
    });

    // Dashboard cards: one view and one section, like radio buttons
    $(document).on('click', '#main-landing [data-landing-group]', function() {
        var group = this.dataset.landingGroup;
        $('#main-landing [data-landing-group="' + group + '"]').removeClass('selected');
        this.classList.add('selected');
        choice[group] = this.dataset.landingValue;

        var missing = [];
        if (!choice.view) missing.push('a view');
        if (!choice.section) missing.push('a section');
        $('#main-landing .main-landing-open-btn').prop('disabled', missing.length > 0);
        $('#main-landing .main-landing-open-label').text(
            choice.view === 'table' ? 'Open the Table' :
            choice.view === 'graphics' ? 'Open the Graphics' : 'Open the Dashboard');
        $('#main-landing .main-landing-open-hint').text(missing.length ?
            'Choose ' + missing.join(' and ') :
            $('#main-landing [data-landing-group="section"].selected .insight-landing-card-title').text());
    });

    $(document).on('click', '#main-landing .main-landing-open-btn', function() {
        if (!choice.view || !choice.section) return;
        openWith({ page: 'dashboard', view: choice.view, section: choice.section });
    });

    // Insight cards
    $(document).on('click', '#main-landing [data-landing-insight]', function() {
        openWith({ page: 'insight', tab: this.dataset.landingInsight });
    });

    // Close button and Escape just hide the popup
    $(document).on('click', '#main-landing .main-landing-close', closeLanding);
    document.addEventListener('keydown', function(e) {
        var el = landing();
        if (e.key === 'Escape' && el && !el.classList.contains('hidden')) closeLanding();
    });
})();

// Insight country filter: tapping a country pill cycles it through
// selected -> activated -> unselected -> selected -> ...
// (the server owns the state and re-renders the pills; this just reports the click)
$(document).on('click', '.country-pill', function() {
    Shiny.setInputValue('insight_filters-countryClicked', $(this).data('country'), {priority: 'event'});
});

// Progress bar for the Insight country filter - the server computes the percentage
// (it owns the actual state) and pushes it here directly, so there's no dependency
// on DOM render timing.
Shiny.addCustomMessageHandler('insightCountriesProgress', function(message) {
    $('#progress-insight-countries').css('width', 'calc(' + message.percentage + '%)');
});

// Insight tab 3's collapsible right-side figures (AST/WGT) start folded. The
// girafe plot inside sizes itself from its container's dimensions once, when
// it's first bound - if that happens while still hidden (display:none), it can
// end up sized 0x0 and never resize on its own afterward. Unlike Shiny's own
// tabsetPanel, a plain Bootstrap collapse isn't wired up to trigger a resize on
// show, so nudge one manually once a figure is actually unfolded.
$(document).on('shown.bs.collapse', '.insight-md-inline-figure .collapse', function() {
    $(window).trigger('resize');
});
