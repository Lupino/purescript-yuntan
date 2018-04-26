exports._importFn0 = function(service) {
  return function(func) {
    return function() {
      return service[func]();
    }
  }
}

exports._importFn1 = function(service) {
  return function(func) {
    return function(arg1) {
      return function() {
        return service[func](arg1);
      }
    }
  }
}

exports._importFn2 = function(service) {
  return function(func) {
    return function(arg1) {
      return function(arg2) {
        return function() {
          return service[func](arg1, arg2);
        }
      }
    }
  }
}

exports._importFn3 = function(service) {
  return function(func) {
    return function(arg1) {
      return function(arg2) {
        return function(arg3) {
          return function() {
            return service[func](arg1, arg2, arg3);
          }
        }
      }
    }
  }
}
