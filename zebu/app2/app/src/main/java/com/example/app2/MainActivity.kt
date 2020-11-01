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

class MainActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        setSupportActionBar(findViewById(R.id.toolbar))

        findViewById<FloatingActionButton>(R.id.fab).setOnClickListener { view ->
            Snackbar.make(view, "Replace with your own action", Snackbar.LENGTH_LONG)
                    .setAction("Action", null).show()
        }
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
    override fun onKeyUp(keyCode: Int, event: KeyEvent): Boolean {
        if (keyCode != KeyEvent.KEYCODE_MEDIA_PLAY &&
                keyCode != KeyEvent.KEYCODE_MEDIA_PAUSE)
            return super.onKeyUp(keyCode, event)
        val s = "You pressed:[" + keyCode + "] at " + System.currentTimeMillis()
        Log.e("ZEDZED", s)
        val t: TextView = findViewById(R.id.textview_first)
        t.text = s
        File("/storage/external/danl-button/" + System.currentTimeMillis()).createNewFile()
        return true
    }
    */
}