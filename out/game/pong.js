let leftArrow = 37;
let rightArrow = 39;
let keyA = 65;
let keyD = 68;
let screen = {width: 600, height: 800};

$(function () {
    let app = new PIXI.Application({
        width: screen.width,
        height: screen.height,
        antialias: true,
        transparent: false,
        resolution: 1,
        backgroundColor: 0x000000
    });
    $("#game").append($(app.view));

    let pad1 = createPad();
    let pad2 = createPad();
    let ball = createBall();
    pad2.y = screen.height - pad2.height;
    app.stage.addChild(pad1.graphics);
    app.stage.addChild(pad2.graphics);
    app.stage.addChild(ball.graphics);

    let body = $("body");
    body.keydown(function (event) {
        if (event.which === leftArrow) pad1.moveLeft = true;
        if (event.which === rightArrow) pad1.moveRight = true;
        if (event.which === keyA) pad2.moveLeft = true;
        if (event.which === keyD) pad2.moveRight = true;
    });
    body.keyup(function (event) {
        if (event.which === leftArrow) pad1.moveLeft = false;
        if (event.which === rightArrow) pad1.moveRight = false;
        if (event.which === keyA) pad2.moveLeft = false;
        if (event.which === keyD) pad2.moveRight = false;
    });

    loop(function () {
        pad1.update();
        pad2.update();
        ball.update();
        ball.collide(pad1);
        ball.collide(pad2);
    });
});

function createPad() {
    let pad = {
        x: 0,
        y: 0,
        speed: 0,
        acceleration: 1.5,
        friction: 0.12,
        moveLeft: false,
        moveRight: false,
        width: 120,
        height: 20
    };
    pad.x = screen.width / 2 - pad.width / 2;
    pad.graphics = new PIXI.Graphics();
    pad.graphics.beginFill(0xFFFFFF);
    pad.graphics.drawRect(0, 0, pad.width, pad.height);
    pad.graphics.endFill();
    pad.update = function () {
        if (pad.moveLeft) pad.speed = pad.speed - pad.acceleration;
        if (pad.moveRight) pad.speed = pad.speed + pad.acceleration;
        pad.x = pad.x + pad.speed;
        pad.speed = pad.speed * (1 - pad.friction);
        if (pad.x < 0) {
            pad.x = 0;
            pad.speed = -pad.speed;
        }
        if (pad.x > screen.width - pad.width) {
            pad.x = screen.width - pad.width;
            pad.speed = -pad.speed;
        }

        pad.graphics.position.x = pad.x;
        pad.graphics.position.y = pad.y;
    };
    return pad;
}

function createBall() {
    let ball = {
        x: screen.width / 2,
        y: screen.height / 2,
        angle: Math.random() < 0.5 ? 90 : 270,
        speed: 7,
        size: 20
    };
    ball.graphics = new PIXI.Graphics();
    ball.graphics.beginFill(0xFFFFFF);
    ball.graphics.drawRect(-ball.size * 0.5, -ball.size * 0.5, ball.size, ball.size);
    ball.graphics.endFill();
    ball.update = function () {
        ball.x = ball.x + Math.cos(ball.angle / 180 * Math.PI) * ball.speed;
        ball.y = ball.y + Math.sin(ball.angle / 180 * Math.PI) * ball.speed;

        if (ball.x < ball.size / 2) {
            ball.x = ball.size / 2;
            ball.angle = normalize(180 - ball.angle);
        }
        if (ball.x > screen.width - ball.size / 2) {
            ball.x = screen.width - ball.size / 2;
            ball.angle = normalize(180 - ball.angle);
        }

        ball.graphics.position.x = ball.x;
        ball.graphics.position.y = ball.y;
    };
    ball.collide = function (pad) {
        if (ball.x > pad.x - ball.size && ball.x < pad.x + pad.width + ball.size) {
            let top = ball.y - ball.size / 2;
            let bottom = ball.y + ball.size / 2;

            if (ball.angle > 180 && ball.angle < 360 && top > pad.y && top < pad.y + pad.height) {
                ball.y = pad.y + pad.height + ball.size / 2;
                ball.angle = normalize(-((ball.x - pad.x) / pad.width) * 90 + 135);
            }

            if (ball.angle > 0 && ball.angle < 180 && bottom > pad.y && bottom < pad.y + pad.height) {
                ball.y = pad.y - ball.size / 2;
                ball.angle = normalize(((ball.x - pad.x) / pad.width) * 90 - 135);
            }
        }
    };
    return ball;
}

function loop(update) {
    update();
    window.requestAnimationFrame(function () {
        loop(update)
    });
}

function normalize(angle) {
    if (angle >= 0 && angle < 360) return angle;
    if (angle < 0) return normalize(angle + 360);
    if (angle >= 360) return normalize(angle - 360);
}

function HSVtoRGB(h, s, v) {
    let r, g, b, i, f, p, q, t;
    if (arguments.length === 1) {
        s = h.s;
        v = h.v;
        h = h.h;
    }
    i = Math.floor(h * 6);
    f = h * 6 - i;
    p = v * (1 - s);
    q = v * (1 - f * s);
    t = v * (1 - (1 - f) * s);
    switch (i % 6) {
        case 0:
            r = v;
            g = t;
            b = p;
            break;
        case 1:
            r = q;
            g = v;
            b = p;
            break;
        case 2:
            r = p;
            g = v;
            b = t;
            break;
        case 3:
            r = p;
            g = q;
            b = v;
            break;
        case 4:
            r = t;
            g = p;
            b = v;
            break;
        case 5:
            r = v;
            g = p;
            b = q;
            break;
    }
    return {
        r: Math.round(r * 255),
        g: Math.round(g * 255),
        b: Math.round(b * 255)
    };
}