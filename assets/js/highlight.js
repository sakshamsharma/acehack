$(document).ready(function() {
  hljs.configure({ classPrefix: '', useBR: false });
  $('pre.code-highlight').each(function(i, block) {
    var code = "";
    hljs.highlightAuto(block.innerText).value.split(/\r\n|\r|\n/).forEach(function(line) {
      code += "<span class=\"line\">" + line + "</span><br>";
    });
    if (code.length > 0) {
      block.innerHTML = code;
    }
  });
  $('pre > code').each(function(i, block) {
    $(this).addClass('codeblock');
    hljs.highlightBlock(block);
  });
});
