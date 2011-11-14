/*
 * jQuery File Upload User Interface Extended Plugin
 * https://github.com/blueimp/jQuery-File-Upload
 *
 * Copyright 2010, Sebastian Tschan
 * https://blueimp.net
 *
 * Licensed under the MIT license:
 * http://creativecommons.org/licenses/MIT/
 */

/*jslint nomen: true, regexp: true */
/*global jQuery, location */

(function ($) {
    'use strict';

    $.widget('blueimpUIX.fileupload', $.blueimpUI.fileupload, {
        options: {
            autoUpload: true,
            add: function (e, data) {
                var that = this;
                $.getJSON('/upload', function (url) {
                    data.url = url.replace(/http(s)?:\/\/[^\/]+/, '');
                    $.blueimpUI.fileupload.prototype
                        .options.add.call(that, e, data);
                });
            },
            destroy: function (e, data) {
                var fu = $(this).data('fileupload');
                data.url = data.url &&
                    fu._addUrlParams(data.url, fu._getAuthenticityToken());
                $.blueimpUI.fileupload.prototype
                    .options.destroy.call(this, e, data);
            }
        },
        
        _addUrlParams: function (url, data) {
            return url + (/\?/.test(url) ? '&' : '?') + $.param(data);
        },
        
        _getAuthenticityToken: function () {
            var name = this.options.authenticityTokenName,
                parts = $.cookie(name).split('|'),
                obj = {};
            obj[name] = parts[0];
            return obj;
        },
        
        _downloadTemplateHelper: function (file) {
            $.blueimpUI.fileupload.prototype
                ._downloadTemplateHelper.call(this, file);
            if (file.info) {
                file.name = file.info.filename.replace(/^.*\\/, '');
                file.size = file.info.size;
                file.sizef = $.blueimpUI.fileupload.prototype
                    ._formatFileSize({size: file.size});
                file.url = '/file-upload/download/' + file.key +
                    '/' + encodeURIComponent(file.name);
                if (file.image_url) {
                    file.thumbnail_url = file.image_url.replace(
                        '/^http:/',
                        location.protocol
                    ) + '=s80';
                }
                file.delete_url = '/file-upload/files/' + file.key + '.json';
                file.delete_type = 'DELETE';
            }
            return file;
        }
        
    });
    
}(jQuery));
