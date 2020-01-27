$(function () {
    $("#scanimate").prop("disabled", true);
    $("#results-section").hide();



    let template = $("#frames-list > .row").detach();
    // $("#frames-add").click(function () {
    //     $("#frames-dropzone").hide();
    //     $("#frames-list").show();
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

    $("#scanimate").click(function () {
        $("#scanimate").hide();
        $("#results-section").css("display", "flex");
    });

});