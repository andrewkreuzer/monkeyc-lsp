using Toybox.Lang;
using Toybox.WatchUi;
using Toybox.System;
using Toybox.Graphics;

class SaveMenu extends WatchUi.ActionMenu {
  private var _stateText;
  var _logger as Logger;

  public function initialize(logger) {
    var _options = {
      :theme => WatchUi.ACTION_MENU_THEME_DARK
    };
    ActionMenu.initialize(_options);
    _logger = logger;

    addItems();
  }

  public function addItems() {
    var _saveItem = new WatchUi.ActionMenuItem({
      :label => "Save",
    }, 1);
    var _discardItem = new WatchUi.ActionMenuItem({
      :label => "Discard",
    }, 0);
    self.addItem(_saveItem);
    self.addItem(_discardItem);
  }
}

class SaveMenuDelegate extends WatchUi.ActionMenuDelegate {
  var _logger as Logger;
  var _saveMenu as SaveMenu;
  var _trainingView as TrainingView;

  public function initialize(trainingView as TrainingView, menu as SaveMenu, logger as Logger) {
    ActionMenuDelegate.initialize();
    _logger = logger;
    _saveMenu = menu;
    _trainingView = trainingView;
  }

  public function onSelect(item as WatchUi.MenuItem) {
    switch(item.getId()) {
        case 1:
          _trainingView.endSession();
          WatchUi.popView(WatchUi.SLIDE_RIGHT);
          break;
        case 0:
          _trainingView.discardSession();
          WatchUi.popView(WatchUi.SLIDE_RIGHT);
          break;
        default:
          var message = Lang.format("Unexpected item id: $1$", [item.getId()]);
          throw new UnexpectedTypeException(message, null, null);
    }
  }

  public function onBack() {
  }

  public function endSession() as Void {
    _trainingView.endSession();
  }
}
