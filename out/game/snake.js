let tps = 10;
let tick = 0;
let frame = 0;
let cells = {x: 21, y: 21};
let cellSize = 30;
let screen = {width: cells.x * cellSize, height: cells.y * cellSize};

let snake = [];
let snakeStartSize = 3;
let direction = 0; // 0 - left, 1 - up, 2 - right, 3 - down
let nextDirection = 0;

let keyW = 87;
let keyA = 65;
let keyS = 83;
let keyD = 68;

Array.prototype.insert = function (index, item) {
    this.splice(index, 0, item);
};

/** Executed on game start */
function init() {
    let xs = Math.floor(cells.x / 2);
    let ys = Math.floor(cells.y / 2);
    for (let i = 0; i < snakeStartSize; i++) {
        snake.push({x: xs + i, y: ys});
    }
    console.log(snake);

    $("body").keydown(function (event) {
        if (event.which === keyW) nextDirection = 1;
        else if (event.which === keyA) nextDirection = 0;
        else if (event.which === keyS) nextDirection = 3;
        else if (event.which === keyD) nextDirection = 2;
    });
}

/** Executed each game tick */
function update() {
    // change direction
    if (direction === 0 && nextDirection === 2
        || direction === 1 && nextDirection === 3
        || direction === 2 && nextDirection === 0
        || direction === 3 && nextDirection === 1) {
        // ignore change
        nextDirection = direction;
    } else {
        direction = nextDirection;
    }

    // move snake
    console.log("update: " + snake[0]);
    let head = {x: snake[0].x, y: snake[0].y};
    if (direction === 0) head.x--;
    else if (direction === 1) head.y--;
    else if (direction === 2) head.x++;
    else if (direction === 3) head.y++;
    snake.insert(0, head);
    snake.pop();
}

/** Colors a single cell at x y */
function fillCell(x, y, color) {
    graphics.fillStyle = color;
    graphics.fillRect(x * cellSize + 1, y * cellSize + 1, cellSize - 2, cellSize - 2);
}

/** Executed each frame */
function render() {
    // clears canvas
    graphics.fillStyle = "#000000";
    graphics.fill();

    // draw background
    for (let x = 0; x < cells.x; x++) {
        for (let y = 0; y < cells.y; y++) {
            fillCell(x, y, "#333333");
        }
    }

    // draw snake
    for (let i = 0; i < snake.length; i++) {
        let part = snake[i];
        fillCell(part.x, part.y, "#cccccc");
    }
}