function redirect(loc) {
    if (window) {
        if (window.location.href) {
            window.location.href = loc;
        } else {
            window.location = loc;
        }
    }
}

function moveToHttps() {
    var host = "sakshamsharma.com";
    if (window.location.host == host && window.location.protocol != "https:") {
        window.location.protocol = "https:";
    }
}
