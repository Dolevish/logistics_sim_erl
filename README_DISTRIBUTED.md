# מערכת לוגיסטיקה מבוזרת - Distributed Logistics Simulation

## סקירה כללית

מערכת זו מדמה מערכת לוגיסטיקה מבוזרת הפועלת על 5 מחשבים שונים:

- **מחשב 1**: מרכז הבקרה (Control Center)
- **מחשב 2**: ממשק המשתמש והוויזואליזציה (UI)
- **מחשבים 3, 4, 5**: מנהלי אזורים (Zone Managers)

## ארכיטקטורה מבוזרת

### מחשב 1 - מרכז הבקרה (control@computer1.local)
- `control_center.erl` - ניהול מצב הסימולציה
- `map_server.erl` - ניהול המפה
- `logistics_state_collector.erl` - איסוף נתונים
- `random_order_generator.erl` - יצירת הזמנות

### מחשב 2 - ממשק המשתמש (ui@computer2.local)
- `logistics_web_server.erl` - שרת HTTP/WebSocket
- `logistics_ws_handler.erl` - טיפול בתקשורת עם הדפדפן
- קבצי UI ב-`priv/static/`

### מחשבים 3, 4, 5 - מנהלי אזורים
- **מחשב 3** (zone1@computer3.local): אזורים 1, 2
- **מחשב 4** (zone2@computer4.local): אזורים 3, 4  
- **מחשב 5** (zone3@computer5.local): אזורים 5, 6

כל מחשב אזור מריץ:
- `zone_manager.erl` - ניהול אזורים
- `courier_pool.erl` - מאגר שליחים
- `courier.erl` - שליחים
- `location_tracker.erl` - מעקב מיקומים

## דרישות מקדימות

1. **Erlang/OTP 24+** מותקן על כל המחשבים
2. **rebar3** מותקן על כל המחשבים
3. כל המחשבים באותה רשת
4. קובץ `/etc/hosts` מעודכן עם כתובות המחשבים:
   ```
   192.168.1.10  computer1.local
   192.168.1.11  computer2.local
   192.168.1.12  computer3.local
   192.168.1.13  computer4.local
   192.168.1.14  computer5.local
   ```

## הגדרת הפרויקט

1. העתק את הפרויקט לכל המחשבים:
   ```bash
   scp -r logistics_sim/ user@computerX.local:/path/to/logistics_sim/
   ```

2. על כל מחשב, הרץ קומפילציה:
   ```bash
   cd /path/to/logistics_sim
   rebar3 compile
   ```

## הפעלת המערכת

### שלב 1: הפעלת מרכז הבקרה (מחשב 1)
```bash
cd /path/to/logistics_sim
./scripts/start_control.sh
```
או באופן ידני:
```bash
erl -name control@computer1.local \
    -setcookie logistics_cookie \
    -config config/control_computer \
    -pa _build/default/lib/*/ebin \
    -s logistics_sim_app start
```

### שלב 2: הפעלת מנהלי האזורים (מחשבים 3, 4, 5)

**מחשב 3:**
```bash
cd /path/to/logistics_sim
./scripts/start_zone1.sh
```

**מחשב 4:**
```bash
cd /path/to/logistics_sim
./scripts/start_zone2.sh
```

**מחשב 5:**
```bash
cd /path/to/logistics_sim
./scripts/start_zone3.sh
```

או באופן ידני:
```bash
# מחשב 3
erl -name zone1@computer3.local -setcookie logistics_cookie -config config/zone1_computer -pa _build/default/lib/*/ebin -s logistics_sim_app start

# מחשב 4
erl -name zone2@computer4.local -setcookie logistics_cookie -config config/zone2_computer -pa _build/default/lib/*/ebin -s logistics_sim_app start

# מחשב 5
erl -name zone3@computer5.local -setcookie logistics_cookie -config config/zone3_computer -pa _build/default/lib/*/ebin -s logistics_sim_app start
```

### שלב 3: הפעלת ממשק המשתמש (מחשב 2)
```bash
cd /path/to/logistics_sim
./scripts/start_ui.sh
```
או באופן ידני:
```bash
erl -name ui@computer2.local \
    -setcookie logistics_cookie \
    -config config/ui_computer \
    -pa _build/default/lib/*/ebin \
    -s logistics_sim_app start
```

### שלב 4: גישה לממשק
פתח דפדפן ולך ל: `http://computer2.local:8080`

## פקודות ניהול

### מתוך Erlang Shell במרכז הבקרה:
```erlang
% התחלת סימולציה
control_center:start_simulation(#{map_size => 100, num_couriers => 12, order_interval => 3000}).

% עצירת סימולציה
control_center:stop_simulation().

% השהיית/המשכת סימולציה
control_center:pause_simulation().
control_center:continue_simulation().

% בדיקת מצב
control_center:get_status().

% רשימת נודים מחוברים
nodes().
```

### בדיקת קישוריות בין נודים:
```erlang
% מתוך כל נוד, בדוק חיבור לנודים אחרים
net_kernel:connect_node('control@computer1.local').
net_kernel:connect_node('ui@computer2.local').
nodes().
```

## פתרון בעיות

### בעיות חיבור בין נודים:
1. וודא ש-firewall מאפשר תקשורת על פורטים של Erlang
2. בדוק שהשמות ב-`/etc/hosts` נכונים
3. וודא שה-cookie זהה בכל הנודים

### לוגים ודיבוג:
```erlang
% הצגת לוגים
observer:start().

% בדיקת תהליכים
supervisor:which_children(logistics_sim_sup).

% בדיקת מצב אזור
zone_manager:get_stats().
```

### הפעלה מקומית לבדיקה:
אם תרצה לבדוק את המערכת על מחשב אחד:
```bash
# טרמינל 1 - מרכז בקרה
erl -name control@192.168.64.3 -setcookie logistics_cookie -config config/control_computer -pa _build/default/lib/*/ebin -s logistics_sim_app start(logistics_sim).

# טרמינל 2 - מנהל אזורים
erl -name zone1@192.168.64.3 -setcookie logistics_cookie -config config/zone1_computer -pa _build/default/lib/*/ebin -s logistics_sim_app start(logistics_sim)

# טרמינל 3 - UI
erl -name ui@192.168.64.3 -setcookie logistics_cookie -config config/ui_computer -pa _build/default/lib/*/ebin -s logistics_sim_app start(logistics_sim)
```

## שינויים שבוצעו לביזור

1. **מודול אפליקציה** (`logistics_sim_app.erl`):
   - תמיכה בתפקידים שונים (control_center, ui, zone_manager)
   - התחברות אוטומטית לנודים הנדרשים

2. **supervisors נפרדים**:
   - `logistics_sim_sup.erl` - רק למרכז הבקרה
   - `logistics_ui_sup.erl` - רק לממשק המשתמש
   - `logistics_zone_sup.erl` - למנהלי האזורים

3. **תקשורת מרוחקת**:
   - שימוש ב-RPC לתקשורת בין נודים
   - עדכון מודולים לעבודה עם מרכז בקרה מרוחק

4. **קבצי הגדרות** לכל נוד בתיקיית `config/`

5. **סקריפטי הפעלה** בתיקיית `scripts/`
