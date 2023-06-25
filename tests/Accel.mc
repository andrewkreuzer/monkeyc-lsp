using Toybox.Sensor;
using Toybox.System;
using Toybox.Lang;

typedef AccelCallback as Method(data as Sensor.SensorData) as Void;
class Accel {
  public function enableAccel(callback as AccelCallback) as Void {
    System.println("Enabling accelerometer sensor");
    var maxSampleRate = Sensor.getMaxSampleRate();

    try {
      Sensor.registerSensorDataListener(
        callback,
        {
          :period => 1,
          :accelerometer => {
            :enabled => true,
            :sampleRate => maxSampleRate
          }
        }
      );
    }
    catch(e) {
      System.println(e.getErrorMessage());
    }
  }

  public function disableAccel() as Void {
    System.println("Disabling accelerometer sensor");
    Sensor.unregisterSensorDataListener();
  }
}
