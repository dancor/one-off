import apsw
from auth import AuthController, require, member_of, name_is
import cherrypy
from datetime import datetime
import time

class RestrictedArea:
    # all methods in this controller (and subcontrollers) is
    # open only to members of the admin group
    
    _cp_config = {
        'auth.require': [member_of('admin')]
    }
    
    @cherrypy.expose
    def index(self):
        return """This is the admin only area."""

class Root:
    _cp_config = {
        'tools.sessions.on': True,
        'tools.sessions.storage_type': 'File',
        'tools.sessions.storage_path': '/var/www/sessions',
        'tools.auth.on': True
    }
    
    auth = AuthController()
    
    restricted = RestrictedArea()
    
    @cherrypy.expose
    @require()
    def index(self, t=None, h=None, c=None, **params):
        time_format = '%Y-%m-%dT%H:%M'

        conn = apsw.Connection("tehu.db")
        cur = conn.cursor()

        cur.execute("SELECT temperature, humidity FROM tehu " +
            "ORDER BY unix_time DESC LIMIT 1")
        row = cur.fetchone()
        if row == None:
            return u"Tehu database empty. Please contact web support."
        temperature, humidity = row

        if t != None:
            h = float(h)
            c = float(c)
            t_unixtime = round(time.mktime(
                datetime.strptime(t, time_format).timetuple()
                ))
            cur.execute("INSERT INTO extra_cool " + 
                "(start_time, end_time, temperature) VALUES (?, ?, ?)",
                (t_unixtime, t_unixtime + round(60 * 60 * h), round(10 * c))
                )

        if params != {}:
            for key in params:
                i = int(key)
                if not i:
                    continue
                cur.execute("DELETE FROM extra_cool WHERE rowid = ?", (i,))

        cur.execute(
            "SELECT rowid, start_time, end_time, temperature " +
            "FROM extra_cool WHERE end_time > ?", (round(time.time()),)
            )
        extra_cool_rows = cur.fetchall()
        conn.close()

        if len(extra_cool_rows) == 0:
            extra_cool_html = '(none)'
        else:
            extra_cool_html = "(check and Remove to remove any)"
        for row_id, start_time, end_time, row_temp in extra_cool_rows:
            extra_cool_html += (
                '<br><input type="checkbox" name="' + str(row_id) + '">' +
                '<input type="datetime-local" readonly value="' + 
                time.strftime(time_format, time.localtime(start_time)) +
                '" /> for ' + str((end_time - start_time) / 3600) + 
                ' hours at ' + str(row_temp / 10) + '°C'
                )
        if len(extra_cool_rows) > 0:
            extra_cool_html += (
                '<br><input type="submit" value="Remove" />' +
                '</form><form action="/stat/" method="post">'
                )

        return ("<html><head></head><body>"
            + '<form style="margin-bottom: 0" action="/stat/" method="post">'
            + "Currently:&nbsp;"
            + str(temperature / 10) + "°C "
            + str(humidity / 10) + "% humidity"
            + "<br><br>Humidity triggers AC at 55%"
            + "<br><br>Additional cool times: "
            + extra_cool_html
            + "</form>"
            + '<form action="/stat/" method="post">'
            + "<br><br>Create additional cool time:"
            + '<br><input name=t type="datetime-local" value="' 
            + datetime.now().strftime("%Y-%m-%dT%H:%M") + '" />'
            + '<br>for <input name=h maxlength=4 size=4 value=3 /> hours'
            + '<br>triggered at <input name=c maxlength=4 size=4 value=23 />°C'
            + '&nbsp;<input type="submit" value="Create" />'
            + "</form>"
            + "</body></html>")
    
    #@cherrypy.expose
    def open(self):
        return "open"
    
    #@cherrypy.expose
    @require(name_is("joe"))
    def only_for_joe(self):
        return """Hello Joe - this page is available to you only"""

    # This is only available if the user name is joe _and_ he's in group admin
    #@cherrypy.expose
    @require(name_is("joe"))
    @require(member_of("admin"))   # equivalent: @require(name_is("joe"), member_of("admin"))
    def only_for_joe_admin(self):
        return """Hello Joe Admin - this page is available to you only"""

def application(environ, start_response):
    cherrypy.tree.mount(Root(), '/stat', None)
    return cherrypy.tree(environ, start_response)

if __name__ == '__main__':
    cherrypy.config.update({'server.socket_port': 5000,
        'server.socket_host': '0.0.0.0',
        })
    cherrypy.quickstart(Root(), '/stat')
