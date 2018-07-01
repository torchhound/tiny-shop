
var putAdd = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/add'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'PUT'
    });
}

var getOneById = function(id, onSuccess, onError)
{
  $.ajax(
    { url: '/one/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var patchModify = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/modify'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'PATCH'
    });
}

var deleteRemoveById = function(id, onSuccess, onError)
{
  $.ajax(
    { url: '/remove/' + encodeURIComponent(id) + ''
    , success: onSuccess
    , error: onError
    , type: 'DELETE'
    });
}

var getAll = function(onSuccess, onError)
{
  $.ajax(
    { url: '/all'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
