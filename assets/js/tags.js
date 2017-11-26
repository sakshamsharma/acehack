$('.mydata:gt(4)').hide().last().after($('<a />').attr('href','#').text('Show more').click(function(){
        var a = this;
        $('.mydata:not(:visible):lt(5)').fadeIn(function(){
         if ($('.mydata:not(:visible)').length == 0) $(a).remove();
        }); return false;
    })
);
