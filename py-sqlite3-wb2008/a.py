import sqlite3

def m_str(m):
    if m:
        return chr(ord("a") - 1 + m)
    return ""

conn = sqlite3.connect("/home/danl/Downloads/wb2008.db")
c = conn.cursor()
for row in c.execute("SELECT m0, m1, m2, m3, phrase, freq FROM phrases"):
    (m0, m1, m2, m3, phrase, freq) = row
    print(
        m_str(m0) + m_str(m1) + m_str(m2) + m_str(m3) + "\t" + phrase + "\t" +
        str(freq))
