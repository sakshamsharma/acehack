function redirect(loc) {
    if (window) {
        if (window.location.href) {
            window.location.href = loc;
        } else {
            window.location = loc;
        }
    }
}
