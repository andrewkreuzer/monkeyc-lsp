using Toybox.Communications;
using Toybox.Application.Storage;
using Toybox.Lang;
using Toybox.Math;

class Logger {
  private var _current_message as Communications.PhoneAppMessage?;
  private var _first_log as Lang.Number;
  private var _last_log as Lang.Number;

  private var _real_life = false;

  function initialize() {
    var mySettings = System.getDeviceSettings();
    var id = mySettings.uniqueIdentifier;

    if (id != null) {
      if (id != "343b762b97c5375184662832804a86b858e11ba4") {
        _real_life = true;
      }
    }

    if (!_real_life) {
      System.println("We're in a simulation Neo");
    }

    _current_message = new Communications.PhoneAppMessage();
    _first_log = 0;
    _last_log = 0;
    Communications.registerForPhoneAppMessages(
      self.method(:phoneMessageCallback) as Communications.PhoneMessageCallback
    );
  }

  function phoneMessageCallback(msg as Communications.PhoneAppMessage) {
    var message = msg.data;
  }

  function sendMessage(message as Lang.String) {
    Communications.transmit(message, null, new Listener());
  }

  function getAllLogs() as Lang.Array {
    return new Lang.Array();
  }

  function getErrorLogs() as Lang.Array {
    return new Lang.Array();
  }

  function error(message as Lang.String) {
    System.println(Lang.format("ERROR: $1$", [message]));
  }

  function info(message as Lang.String) {
    var logLine = Lang.format("INFO: $1$", [message]);
    System.println(logLine);
    sendMessage(message);
  }

  function debug(message as Lang.String) {
    System.println(Lang.format("DEBUG: $1$", [message]));
  }


// signle line
/*  function log(
      message as String,
      options as {
      :persist as Lang.Boolean,
      :send as Lang.Boolean
      }) {
    if (options.get(:persist)) {
      _last_log += 1;
      try {
        Storage.setValue(_last_log, message);
      } catch( ex instanceof Lang.StorageFullException ) {
        System.println("Storage is full removing oldest logs");
        var removalLength = 0;
        while (removalLength < message.length()) {
          var logLength = Storage.getValue(_first_log).length();
          removalLength += logLength;
          Storage.deleteValue(_first_log);
          _first_log += 1;
        }
        Storage.setValue(_last_log, message);
      }
    }
    if (options.get("send") && _real_life) {
      sendMessage(message);
    }

    System.println(message);
  }
} */

class Listener extends Communications.ConnectionListener {

  public function onComplete() {
    System.println("Message sent successfully");
  }

  public function onError() {
    System.println("Message was not sent successfully");
  }
}
