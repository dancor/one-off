package com.example.app2

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.util.Log
import java.io.File

class MyReceiver : BroadcastReceiver() {
    var waitingOn2 = false
    override fun onReceive(context: Context, intent: Intent) {
        // This runs twice for some reason, with the same intent:
        // action = android.intent.action.MEDIA_BUTTON, data = null.
        if (!this.waitingOn2) {this.waitingOn2 = true; return}
        this.waitingOn2 = false
        var s = "You pressed!" // + keyCode + "] at " + System.currentTimeMillis()
        Log.e("LOL-BUTTON", s)
        //val t: TextView = findViewById(R.id.textview_first)
        //t.text = s
        File("/storage/external/danl-button/" + System.currentTimeMillis()).createNewFile()
    }
}