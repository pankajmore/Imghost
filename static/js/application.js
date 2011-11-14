/*
 * AQUANTUM Demo Application
 * http://aquantum-demo.appspot.com/file-upload
 *
 * Copyright 2010, Sebastian Tschan
 * https://blueimp.net
 *
 * Licensed under the MIT license:
 * http://creativecommons.org/licenses/MIT/
 */

/*jslint nomen: true, unparam: true, regexp: true */
/*global $ */

var Application = function (settings, locale) {
    'use strict';

    // Generic:
    // --------
    $('html:first').removeClass('no-js');
    // Initialize theme switcher:
    $('#theme-switcher select').change(function () {
        var theme = $('#theme');
        theme.prop(
            'href',
            theme.prop('href').replace(
                /[\w\-]+\/jquery-ui.css/,
                $(this).val() + '/jquery-ui.css'
            )
        );
        $.cookie('theme', $(this).val(), {path: '/'});
    }).val($.cookie('theme') || 'base').trigger('change');
    // Initialize imagegallery plugin:
    $('a[rel=gallery]').imagegallery();

    // File Upload:
    // ------------
    $('#tabs').tabs();
    $('#radio').buttonset();
    $('#fileupload').fileupload({
        maxFileSize: settings.max_file_size,
        authenticityTokenName: settings.authenticity_token &&
            settings.authenticity_token.name
    });
    $('#radio input').click(function (e) {
        $('#fileupload').fileupload(
            'option',
            'autoUpload',
            $(this).val() === 'auto'
        );
    });
    // Create jQuery UI buttons for existing files:
    $('#fileupload .files .delete button').button({
        text: false,
        icons: {primary: 'ui-icon-trash'}
    });
    // Enable drag-to-desktop for existing files:
    $('#fileupload .files .template-download a').each(
        $.blueimpUIX.fileupload.prototype._enableDragToDesktop
    );
    // Open download dialogs via iframes,
    // to prevent aborting current uploads:
    $('#fileupload .files').delegate(
        'a:not([rel^=gallery])',
        'click',
        function (e) {
            e.preventDefault();
            $('<iframe style="display:none;"></iframe>')
                .prop('src', this.href)
                .appendTo('body');
        }
    );

    // Home page:
    // ----------
    $('#home .demos a').button();
    
    // Login page:
    // -----------
    $('#login button.openid-provider').button({icons: {primary: 'icon-openid'}});
    $('#login #openid-provider-generic button, #logout-link').button();

};