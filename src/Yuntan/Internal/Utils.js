exports._fromFn0 = function(func) {
  return function(service) {
    return function() {
      return service[func]();
    }
  }
}

exports._fromFn1 = function(func) {
  return function(arg1) {
    return function(service) {
      return function() {
        return service[func](arg1);
      }
    }
  }
}

exports._fromFn2 = function(func) {
  return function(arg1) {
    return function(arg2) {
      return function(service) {
        return function() {
          return service[func](arg1, arg2);
        }
      }
    }
  }
}

exports._fromFn3 = function(func) {
  return function(arg1) {
    return function(arg2) {
      return function(arg3) {
        return function (service) {
          return function() {
            return service[func](arg1, arg2, arg3);
          }
        }
      }
    }
  }
}

exports._fromAFn0 = function(func) {
  return function(client) {
    return function(onError, onSuccess) {
      client[func](function(err, ret) {
        if (err) {
          onError(err);
        } else {
          onSuccess(ret);
        }
      });

      // Return a canceler, which is just another Aff effect.
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelerSuccess(); // invoke the success callback for the canceler
      };
    }
  }
}

exports._fromAFn1 = function(func) {
  return function(arg1) {
    return function(client) {
      return function(onError, onSuccess) {
        client[func](arg1, function(err, ret) {
          if (err) {
            onError(err);
          } else {
            onSuccess(ret);
          }
        });

        // Return a canceler, which is just another Aff effect.
        return function (cancelError, cancelerError, cancelerSuccess) {
          cancelerSuccess(); // invoke the success callback for the canceler
        };
      }
    }
  }
}

exports._fromAFn2 = function(func) {
  return function(arg1) {
    return function(arg2) {
      return function(client) {
        return function(onError, onSuccess) {
          client[func](arg1, arg2, function(err, ret) {
            if (err) {
              onError(err);
            } else {
              onSuccess(ret);
            }
          });

          // Return a canceler, which is just another Aff effect.
          return function (cancelError, cancelerError, cancelerSuccess) {
            cancelerSuccess(); // invoke the success callback for the canceler
          };
        }
      }

    }
  }
}

exports._fromAFn3 = function(func) {
  return function(arg1) {
    return function(arg2) {
      return function(arg3) {
        return function(client) {
          return function(onError, onSuccess) {
            client[func](arg1, arg2, arg3, function(err, ret) {
              if (err) {
                onError(err);
              } else {
                onSuccess(ret);
              }
            });

            // Return a canceler, which is just another Aff effect.
            return function (cancelError, cancelerError, cancelerSuccess) {
              cancelerSuccess(); // invoke the success callback for the canceler
            };
          }
        }
      }
    }
  }
}
