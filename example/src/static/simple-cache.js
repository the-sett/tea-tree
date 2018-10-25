/**
 * Creates a new storage instance.
 * @constructor
 */
var Storage = function(options) {
	this.storage = {};
	this.timeouts = {};
	this.queued = {};
	this.queues = {};

	options = options || {};
	this.delay = options.delay || 60000;
};

/**
 * @function Storage.get(key)
 * @description Retrieves the value of a stored key.
 * @param key The key to look up.
 * @returns The value stored under this key, or undefined.
 */
Storage.prototype.get = function(key) {
	return this.exists(key)?this.storage[key]:undefined;
};

/**
 * @function Storage.set(key, value)
 * @description Sets a value under the supplied key, if it has not been set already.
 * @param key The key to look up.
 * @param value The value to set the key to.
 * @param delay (optional) How long to store the value in the cache, in seconds.
 * @returns {Storage} The storage instance.
 */
Storage.prototype.set = function(key, value, delay) {
	if (!this.storage[key]) {
		var me = this;
		this.storage[key] = value;
		this.timeouts[key] = setTimeout(function() {
			// console.log('clearing cache');
			this.master.clear(this.key);
		}, delay*1000 || this.delay);
		this.timeouts[key].master = this;
		this.timeouts[key].key = key;
	}
	return this;
};

/**
 * @function Storage.exists(key)
 * @description Checks whether or not a key has been stored in the cache.
 * @param key The key to check.
 * @returns {Boolean}
 */
Storage.prototype.exists = function(key) {
	return this.storage[key] !== undefined;
};

/**
 * @function Storage.clear(key)
 * @description Unsets the value under the selected key.
 * @param key The key to unset.
 * @returns {Storage} The storage instance.
 */
Storage.prototype.clear = function(key) {
	delete this.storage[key];
	clearTimeout(this.timeouts[key]);
	delete this.timeouts[key];
	return this;
};

/**
 * @function Storage.override(key, value, delay)
 * @description Overrides a currently stored value.
 * @param key The key to override.
 * @param value The new value to store in this key.
 * @param delay The new lifetime of the stored value.
 */
Storage.prototype.override = function(key, value, delay) {
	this.clear(key).set(key, value, delay);
};

/**
 * Intended for situations where storing a value to the cache may
 *  depend on asynchronous functions that could overlap.
 *
 * "get" will only be called once, and in the meantime all other
 *  requests will be added to a queue. Once the value is set, the
 * The "set" callback will be called on all pending requests.
 * "alwaysGet" decides whether or not to call get after setting the first
 * time. Defaults to true.
 * "perTick" how many `gets` to perform each tick. Set to 0 to perform them all at once.
 *
 * @function Storage.async(key, first, later)
 * @param key The key to store the value under.
 * @param options For callbacks `get` and `set`, among other parameters
 * @param delay The lifetime the value will have.
 */
Storage.prototype.async = function(key, options, delay) {
	var master = this;

	// Throw an error if there aren't any getters/setters
	if (!options || (!options.set && !options.get)) {
		throw new Error('No getters/setters provided');
	}

	// Default functionality if only get is provided
	if (typeof options.set !== 'function') {
		options.get(this.get(key));
		return;
	}

	// Default functionality if only set is provided
	if (typeof options.get !== 'function') {
		options.set(function(value) {
			master.set(key, value, delay || master.delay);
		});
		return;
	}

	// If already defined, just get it outright
	if (this.exists(key)) {
		options.get(this.get(key));
		return;
	}

	// If it hasn't been queued already, this is the first and should be `set`.
	if (!this.queued[key]) {
		this.queued[key] = true;

		process.nextTick(function() {
			options.set(function(value) {
				var i, l, interval;

				if (typeof options.alwaysGet === 'undefined') {
					options.alwaysGet = true;
				}

				master.set(key, value, delay || master.delay);
				if (options.alwaysGet) {
					options.get(value);
				}
				delete master.queued[key];

				if (master.queues[key]) {
					l = master.queues[key].length;
					i = 0;
					interval = setInterval(function() {
						var a = 0,
							b = options.perFrame > 0 ? options.perFrame : l;

						if (i < l) {
							for (a; a < b && i < l; a++) {
								master.queues[key][i](value);
								delete master.queues[key][i];
								i += 1; a += 1;
							}
						} else {
							clearInterval(interval);
							delete master.queues[key];
						}
					}, 1);
				}
			});
		});

		return;
	}

	// By now we know there's a getter and it should be directed
	// to the queue. So add it for later.
	this.queues[key] = this.queues[key] || [];
	this.queues[key].push(options.get);
};

module.exports.Storage = Storage;
