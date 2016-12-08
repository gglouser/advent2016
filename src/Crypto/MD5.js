"use strict";

// Simple FFI to node's crypto MD5 hash.

const crypto = require('crypto');

exports.md5 = function (data) {
    return crypto.createHash('md5').update(data).digest('hex');
};
