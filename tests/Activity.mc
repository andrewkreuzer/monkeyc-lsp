using Toybox.ActivityRecording;
using Toybox.WatchUi;
using Toybox.SensorLogging;
using Toybox.Sensor;
using Toybox.Lang;

class TrainingRecorder {
  var _session as ActivityRecording.Session or Null;
  var _logger as SensorLogging.SensorLogger;

  public function initialize() {
    _logger = new SensorLogging.SensorLogger({
      :accelerometer => {:enabled => true},
      :gyroscope => {:enabled => true},
    });

    _session = ActivityRecording.createSession({
      :name=>"Training",
      :sport=>ActivityRecording.SPORT_TRAINING,
      :subSport=>ActivityRecording.SUB_SPORT_STRENGTH_TRAINING,
      :sensorLogger=>_logger,
    });
  }

  public function start() as Lang.Boolean {
    if (Toybox has :ActivityRecording) {
      var session = _session;
      if ((session != null) && (!session.isRecording())) {
        Accel.enableAccel(self.method(:accelCallback));
        System.println("Starting session");
        session.start();
      }
    }

    return true;
  }

  public function stop() as Lang.Boolean {
    if (Toybox has :ActivityRecording) {
      var session = _session;
      if ((session != null) && session.isRecording()) {
        System.println("Stopping session");
        Accel.disableAccel();
        session.stop();
      }
    }

    return true;
  }

  public function endSession() as Lang.Boolean {
    if (Toybox has :ActivityRecording) {
      var session = _session;
      if (session != null) {
        System.println("Ending session");
        Accel.disableAccel();
        if (session.isRecording()) {
          session.stop();
        }
        session.save();
        _session = null;
      }
    }

    return true;
  }

  public function discardSession() as Lang.Boolean {
    if (Toybox has :ActivityRecording) {
      var session = _session;
      if (session != null) {
        System.println("Discarding session");
        Accel.disableAccel();
        if (session.isRecording()) {
          session.stop();
        }
        session.discard();
        _session = null;
      }
    }

    return true;
  }

  public function accelCallback(sensorData as Sensor.SensorData) as Void {
    var _samplesX = sensorData.accelerometerData.x;
    var _samplesY = sensorData.accelerometerData.y;
    var _samplesZ = sensorData.accelerometerData.z;

    /* System.println("Raw samples, X axis: " + _samplesX); */
    /* System.println("Raw samples, Y axis: " + _samplesY); */
    /* System.println("Raw samples, Z axis: " + _samplesZ); */
  }
}
