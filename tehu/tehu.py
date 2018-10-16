import apsw
import DHT22
import pigpio
import time

tehu_pin = 4
fan_pin = 17
cool_pin = 27
heat_pin = 22

cool_off_minimum_wait_secs = 180

cool_on_humidity = 55
cool_off_humidity = cool_on_humidity - 1

# This will be set from the database which is checked every minute
extra_cool_10x = None

pi = pigpio.pi()
s = DHT22.sensor(pi, tehu_pin)

def update_cool(cool_on):
    i = int(not(cool_on))
    pi.write(fan_pin,  i)
    pi.write(cool_pin, i)
    return cool_on

def want_cool_on(te, hu):
    return hu > humidity_max

last_cool_off_time = time.time()
cool_on = update_cool(False)

while True:
    s.trigger()
    time.sleep(1)
    te = s.temperature()
    hu = s.humidity()

    t = round(time.time())
    if cool_on:
        if (extra_cool_10x == None or 10 * te <= extra_cool_10x - 10) and \
           hu <= cool_off_humidity:
            cool_on = update_cool(False)
            last_cool_off_time = t
    else:
        if t >= last_cool_off_time + cool_off_minimum_wait_secs and \
           (hu >= cool_on_humidity or 
            (extra_cool_10x != None and 10 * te >= extra_cool_10x)
           ):
            cool_on = update_cool(True)

    conn = apsw.Connection("tehu.db")
    c = conn.cursor()

    values = \
	( t
	, round(10 * te)
	, round(10 * hu)
        , int(cool_on)
	)
    c.execute(
        "INSERT INTO tehu (unix_time, temperature, humidity, cool_on) " +
        "VALUES (?, ?, ?, ?)", values)

    c.execute(
        "SELECT MIN(temperature) FROM extra_cool WHERE " +
        "start_time < ? AND end_time > ?", (t, t))
    extra_cool_10x = c.fetchone()

    conn.close()

    time.sleep(59)
