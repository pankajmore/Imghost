/*
 * jQuery Image Gallery Plugin JS Example 1.3.2
 * https://github.com/blueimp/jQuery-Image-Gallery
 *
 * Copyright 2011, Sebastian Tschan
 * https://blueimp.net
 *
 * Licensed under the MIT license:
 * http://creativecommons.org/licenses/MIT/
 */

/*jslint white: true, unparam: true, regexp: true */
/*global $ */

$(function () {
    'use strict';

    // Initialize the Image Gallery plugin:
    $('a[rel="gallery"]').imagegallery({
            open: function (event, ui) {/* called on dialogopen */},
            title: 'Image Gallery', // Sets the dialog title
            show: 'scale', // The effect to be used when the dialog is opened
            hide: 'explode', // The effect to be used when the dialog is closed
            offsetWidth: 50, // Offset of image width to viewport width
            offsetHeight: 50, // Offset of image height to viewport height
            slideshow: 5000, // Shows the next image after 5000 ms
            fullscreen: true, // Displays images fullscreen (overrides offsets)
            canvas: true, // Displays images as canvas elements
            namespace: 'myimagegallery' // event handler namespace
    });

});
