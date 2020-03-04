let tps = 20;
let tick = 0;
let frame = 0;
let screen = {width: 500, height: 500};

/** Executed on game start */
function init() {

}

/** Executed each game tick */
function update() {

}

/** Executed each frame */
function render() {
    // clears canvas
    graphics.fillStyle = "#003300";
    graphics.fill();
    // rendering sample
    graphics.fillStyle = "#005599";
    graphics.fillRect(0, 0, frame % screen.width, frame % screen.height);
}