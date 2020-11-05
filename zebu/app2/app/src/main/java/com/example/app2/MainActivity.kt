package com.example.app2
import android.content.Intent
import android.content.IntentFilter
import android.media.AudioFormat
import android.media.AudioManager
import android.media.AudioTrack
import android.os.Bundle
import android.util.Log
import com.google.android.material.floatingactionbutton.FloatingActionButton
import com.google.android.material.snackbar.Snackbar
import androidx.appcompat.app.AppCompatActivity
import android.view.KeyEvent
import android.view.Menu
import android.view.MenuItem
import android.widget.TextView
import java.io.File
import android.support.v4.media.session.MediaSessionCompat
import android.view.MotionEvent

class MainActivity : AppCompatActivity() {
    var lastKeyTime : Long = 0
    var lastKeyCode = 0
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        var f : IntentFilter = IntentFilter(Intent.ACTION_MEDIA_BUTTON);
        var r = MyReceiver();
        f.setPriority(999);
        registerReceiver(r, f);
    }
    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        // Inflate the menu; this adds items to the action bar if it is present.
        menuInflater.inflate(R.menu.menu_main, menu)
        return true
    }
    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        return when (item.itemId) {
            R.id.action_settings -> true
            else -> super.onOptionsItemSelected(item)
        }
    }
    /*
    override fun onTrackballEvent(e: MotionEvent): Boolean {
        Log.e("LOL-TRACKBALL", e.toString())
        return false // true
    }
     */

    override fun onKeyDown(keyCode: Int, e: KeyEvent): Boolean {
        var c = when (e.keyCode) {
            KeyEvent.KEYCODE_DPAD_RIGHT -> "f"
            KeyEvent.KEYCODE_DPAD_LEFT -> "b"
            KeyEvent.KEYCODE_DPAD_UP -> "u"
            KeyEvent.KEYCODE_DPAD_DOWN -> "d"
            KeyEvent.KEYCODE_ENTER -> "t"
            KeyEvent.KEYCODE_FORWARD_DEL -> "F"
            KeyEvent.KEYCODE_DEL -> "B"
            KeyEvent.KEYCODE_VOLUME_UP -> "U"
            KeyEvent.KEYCODE_VOLUME_DOWN -> "D"
            KeyEvent.KEYCODE_BACK -> "T"
            else -> return false
        }
        var curTime : Long = System.currentTimeMillis()
        if (curTime < lastKeyTime + 100) return true
        /*
        if (keyCode != KeyEvent.KEYCODE_MEDIA_PLAY &&
                keyCode != KeyEvent.KEYCODE_MEDIA_PAUSE)
            return super.onKeyUp(keyCode, event)
        */
        Log.e("LOL-KEYDOWN", e.toString())
        val t: TextView = findViewById(R.id.textview_first)
        t.append(c)
        lastKeyTime = curTime
        lastKeyCode = e.keyCode
        //File("/storage/external/danl-button/" + System.currentTimeMillis()).createNewFile()
        return true
    }
}