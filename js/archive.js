$(document).ready(function() {
   $(".postShorten-wrap").map(function() {
       $(this).click(function() {
           redirect($(this).attr('id').substring(5));
       });
   });
});
