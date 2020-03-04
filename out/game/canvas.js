let graphics;

/** Starts the game */
$(function () {
    let canvas = $("<canvas>")
        .attr("width", screen.width)
        .attr("height", screen.height)
        .width(screen.width)
        .height(screen.height)
        .appendTo($("#game"));
    graphics = canvas[0].getContext("2d");

    // game startup
    init();
    // game logic
    updateLoop(update);
    // game rendering
    renderLoop(render);
});

/** Sets up update loop */
function updateLoop(code) {
    code();
    tick++;
    window.setTimeout(() => updateLoop(code), 1000 / tps)
}

/** Sets up rendering loop */
function renderLoop(code) {
    code();
    frame++;
    window.requestAnimationFrame(() => renderLoop(code));
}