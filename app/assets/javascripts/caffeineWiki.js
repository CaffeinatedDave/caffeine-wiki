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
  
  function addTag(data) {
    jQuery("#tagList").append('<a href="/tag/' + data + '">' + data + '</a> ');
  }
  
  jQuery("#addTag").keyup(function(e){
    if(e.keyCode == 13)
    {
        jQuery(this).trigger("enterKey");
    }
  });
});
