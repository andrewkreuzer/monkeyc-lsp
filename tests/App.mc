using Toybox.Application;
using Toybox.Lang;
using Toybox.WatchUi;

class Training extends Application.AppBase {
  var _logger as Logger;

  public function initialize() {
    AppBase.initialize();
    _logger = new Logger();
  }

  public function onStart(state as Lang.Dictionary?) as Void {
  }

  public function onStop(state as Lang.Dictionary?) as Void {
  }

  public function getInitialView() as Lang.Array<WatchUi.Views or WatchUi.InputDelegates>? {
    var trainingView = new $.TrainingView(_logger);
    var trainingInputDelegate = new $.TrainingInputDelegate(trainingView, _logger);
    return [trainingView, trainingInputDelegate] as Lang.Array<WatchUi.View or WatchUi.InputDelegate>;
  }
}
