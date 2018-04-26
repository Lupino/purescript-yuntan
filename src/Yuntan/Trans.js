var services = require('yuntan-service');

exports.initService = function(name) {
  return function(service) {
    return function(options) {
      return function() {
        var srv = new services[service](options);
        srv.serviceName = name;
        return srv;
      }
    }
  }
}

exports.serviceName = function(service) {
  return service.serviceName;
}
