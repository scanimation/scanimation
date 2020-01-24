$(function () {
    $("#frames-clear").prop("disabled", true);
    $("#frames-show").prop("disabled", true);
    $("#settings-reset").prop("disabled", true);
    $("#scanimate").prop("disabled", true);
    $("#frames-list").hide();
    $("#results-section").hide();

    let template = $("#frames-list > .row").detach();
    // $("#frames-add").click(function () {
    //     $("#frames-dropzone").hide();
    //     $("#frames-list").show();
    //     $("#frames-clear").prop("disabled", false);
    //     $("#frames-show").prop("disabled", false);
    //     $("#scanimate").prop("disabled", false);
    //
    //     let index;
    //     for (index = 0; index < 6; index++) {
    //         let copy = template.clone();
    //         copy.find(".index").text(index + 1);
    //         copy.find(".name").text("frame-00" + (index + 1) + ".png");
    //
    //         copy.click(function () {
    //             $("#frames-list > .row").removeClass("selected");
    //             copy.addClass("selected");
    //         });
    //         copy.find(".remove").click(function () {
    //             copy.detach();
    //         });
    //
    //         $("#frames-list").append(copy);
    //     }
    // });

    $("#frames-clear").click(function () {
        $("#frames-clear-overlay").css("display", "flex");
    });
    $("#frames-clear-yes").click(function () {
        $("#frames-list > .row").detach();
        $("#frames-clear-overlay").hide();
    });

    $("#scanimate").click(function () {
        $("#scanimate").hide();
        $("#results-section").css("display", "flex");
    });

    // $(".overlay").hide();
    $(".overlay > div").click(function (event) {
        event.stopPropagation();
    });
    $(".overlay-close").click(function () {
        $(".overlay").hide();
    });
});