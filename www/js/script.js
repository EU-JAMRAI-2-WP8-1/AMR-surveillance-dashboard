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
