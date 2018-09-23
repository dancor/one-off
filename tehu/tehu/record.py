import DHT22
import pigpio
import sqlite3
import time

tehu_pin = 4

pi = pigpio.pi()
s = DHT22.sensor(pi, tehu_pin)

while True:
    s.trigger()
    time.sleep(1)
    conn = sqlite3.connect("tehu.db")
    c = conn.cursor()
    values = \
	( round(time.time())
	, round(10 * s.temperature())
	, round(10 * s.humidity())
	)
    c.execute("INSERT INTO tehu (unix_time, temperature, humidity) VALUES " +
	"(?, ?, ?)", values)
    conn.commit()
    conn.close()
    #print(values)
    time.sleep(59)
