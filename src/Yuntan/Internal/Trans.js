exports.serviceName = function(service) {
  return service.serviceName;
};

exports.setServiceName = function(name) {
  return function(service) {
    service.serviceName = name;
    return service;
  };
};
