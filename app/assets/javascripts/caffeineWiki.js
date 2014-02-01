jQuery(document).ready(function () {
  var name = undefined;
  var lastMsgTime = 0;

  jQuery("#addTag").bind("enterKey",function(e){
    var id = Number(jQuery("#articleId").val());
    var tag = jQuery("#addTag").val();
    if (tag != "") {
      jQuery.ajax({
        url: "/tag",
        type: "POST",
        data: JSON.stringify({"id": id, "tag": tag}),
        contentType: "application/json; charset=UTF-8",
        success: addTag
      }).fail(function(x,t,e) {
        // Catch this?
        console.log(e);
      }).done(function(data) {
        jQuery("#addTag").val("");
      });
    } else {
      console.log("No tag...");
    }
  });

  jQuery(".lockArticle").click(function() {
    var id = Number(jQuery(this).data("article-id"));
    if (id != "") {
      jQuery.ajax({
        url: "/article/lock",
        type: "POST",
        data: JSON.stringify({"id": id}),
        contentType: "application/json; charset=UTF-8"
      }).fail(function(x,t,e) {
        // Catch this?
        console.log(e);
      }).done(function(data) {
        console.log(data);
        jQuery(".lockArticle[data-article-id='"+id+"']").attr("src", (data === "L") ? "/assets/images/locked.png" : "/assets/images/unlocked.png");
      })
    } else {
      console.log("No id...");
    }
  });

  var doDelete = function() {
    var id = Number(jQuery("#articleId").val());
    var tag = jQuery(this).parent().find("a").html();
    if (tag != "") {
      jQuery.ajax({
        url: "/tag/delete",
        type: "POST",
        data: JSON.stringify({"id": id, "tag": tag}),
        contentType: "application/json; charset=UTF-8"
      }).fail(function(x,t,e) {
        // Catch this?
        console.log(e);
      })
    } else {
      console.log("No tag...");
    }
    jQuery(this).parent().hide();
  };
  
  jQuery("#tagList .remove").click(doDelete)
  
  function addTag(data) {
    jQuery("#tagList").append('<span class="tag"><a href="/tag/' + data + '">' + data + '</a> <span class="remove">X</span></span>')
      .find(".tag").mouseover(showDelete).mouseout(hideDelete)
      .find(".remove").click(doDelete);
  }
  
  jQuery("#addTag").keyup(function(e){
    if(e.keyCode == 13)
    {
        jQuery(this).trigger("enterKey");
    }
  });

  var showDelete = function() {
    jQuery(this).find(".remove").show();
  }
  var hideDelete = function() {
    jQuery(this).find(".remove").hide();
  }
  
  jQuery(".tag").mouseover(showDelete);
  jQuery(".tag").mouseout(hideDelete);

});

function submitForm(action) {
  jQuery('#articleEditForm').attr("action", action);
  jQuery('#articleEditForm').submit();
}
