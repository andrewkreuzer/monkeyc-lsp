using Toybox.Lang;
using Toybox.WatchUi;
using Toybox.System;
using Toybox.Graphics;

enum State {
  ACTIVE,
  NOT_ACTIVE,
}

class TrainingView extends WatchUi.View {
  private var _state as State;
  private var _stateText;
  private var _trainingRecorder as TrainingRecorder;
  private var _logger as Logger;

  public function initialize(logger) {
    View.initialize();
    _state = NOT_ACTIVE;
    _trainingRecorder = new TrainingRecorder();
    _logger = logger;
  }

  public function onShow() as Void {
  }

  public function onLayout(dc as Graphics.Dc) as Void {
    setLayout($.Rez.Layouts.MainLayout(dc));
  }

  public function onUpdate(dc as Graphics.Dc) as Void {
    View.onUpdate(dc);
    _stateText = View.findDrawableById("id_active");
    if (_stateText != null) {
      if (_state == ACTIVE) {
        _stateText.setText("Active");
      } else {
        _stateText.setText("Not Active");
      }
    }
  }

  public function onHide() as Void {
  }

  public function toggleState() as Void {
    var state = _state;
    if (state == ACTIVE) {
      _state = NOT_ACTIVE;
      _stateText.setText("Not Active");
      _trainingRecorder.stop();
    } else {
      _state = ACTIVE;
      _stateText.setText("Active");
      _trainingRecorder.start();
    }

    WatchUi.requestUpdate();
  }

  public function endSession() as Void {
    _trainingRecorder.endSession();
  }

  public function discardSession() as Void {
    _trainingRecorder.discardSession();
  }
}

class TrainingInputDelegate extends WatchUi.InputDelegate {
  var _trainingView as TrainingView;
  var _logger as Logger;

  var _count as Lang.Number;

  public function initialize(view as TrainingView, logger as Logger) {
    BehaviorDelegate.initialize();
    _trainingView = view;
    _logger = logger;

    _count = 0;
  }

  function onKeyPressed(keyEvent) as Lang.Boolean {
    System.println("onKeyPressed");
    return true;
  }

  function onKeyReleased(keyEvent) as Lang.Boolean {
    System.println("onKeyReleased");
    return true;
  }

  function onKey(keyEvent) as Lang.Boolean{
    var trainingView = _trainingView;
    if (trainingView == null) {
      return false;
    }

    if (keyEvent.getKey() == WatchUi.KEY_ESC) {
      var _saveMenu = new $.SaveMenu(_logger);
      var _saveMenuDelegate = new $.SaveMenuDelegate(_trainingView, _saveMenu, _logger);
      WatchUi.showActionMenu(_saveMenu, _saveMenuDelegate);
    }

    if (keyEvent.getKey() == WatchUi.KEY_ENTER) {
      var logger = _logger;
      if (logger != null) {
        _count += 1;
        var message = Lang.format("Test Message: Hello number $1$", [_count]);
        logger.info(message);
      }
      trainingView.toggleState();
    }

    return true;
  }
}
