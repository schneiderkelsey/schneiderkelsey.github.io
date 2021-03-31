function scrollDetect(home, down, up) {
        var currentScroll = scrollTop();
        if (scrollTop() === 0) {
            home();
        } else if (currentScroll > scrollState) {
            down();
        } else {
            up();
        }
        scrollState = scrollTop();
    }
    