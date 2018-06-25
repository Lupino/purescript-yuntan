const services = require('yuntan-service');

exports.initService = function(name) {
  return function(service) {
    return function(options) {
      return function() {
        const srv = new services[service](options);
        srv.serviceName = name;
        return srv;
      };
    };
  };
};
