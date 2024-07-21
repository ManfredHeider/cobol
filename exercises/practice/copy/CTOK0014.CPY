      ******************************************************************
      * DCLGEN TABLE ABGLEICH-CONTROL
      ******************************************************************
CCC005*    EXEC SQL DECLARE ABGLEICH_CONTROL TABLE
CCC004*    ( AK_KEY                         TIMESTAMP NOT NULL,
CCC004*      AK_SQL_START                   TIMESTAMP,
CCC004*      AK_SQL_ENDE                    TIMESTAMP,
CCC004*      AK_SQL_ANZAHL                  INTEGER,
CCC004*      AK_SATZ_ANZAHL                 INTEGER,
CCC004*      AK_OKI_START                   TIMESTAMP,
CCC004*      AK_OKI_ENDE                    TIMESTAMP,
CCC004*      AK_OKI_SQL_ANZAHL              INTEGER,
CCC004*      AK_OKI_SATZ_ANZAHL             INTEGER,
CCC004*      AK_SQL_STATUS                  CHAR(10),
CCC004*      AK_STATUS                      CHAR(1),
CCC004*      AK_OKI_BIND_ANZ                INTEGER,
CCC004*      AK_OKI_BIND_OKAN               INTEGER,
CCC004*      AK_OKI_SQL_BIND                INTEGER,
CCC004*      AK_LOGMAS_START                TIMESTAMP,
CCC004*      AK_LOGMAS_ENDE                 TIMESTAMP,
CCC004*      AK_LOGMAS_DATEI                CHAR(36),
CCC004*      AK_LOGMAS_GEN                  CHAR(8),
CCC004*      AK_EINARB_DATEI                CHAR(36),
CCC004*      AK_EINARB_GEN                  CHAR(8),
CCC004*      AK_LOGMAS_ANZ                  INTEGER,
CCC004*      AK_EINARB_ANZ                  INTEGER,
CCC004*      AK_LOGMAS_STATUS               CHAR(1) NOT NULL,
CCC004*      AK_EINARB_STATUS               CHAR(1) NOT NULL,
CCC004*      AK_LOGMAS_JOB                  CHAR(8),
CCC004*      AK_EINARB_JOB                  CHAR(8),
CCC004*      AK_LOGMAS_TSTP                 TIMESTAMP,
CCC004*      AK_EINARB_TSTP                 TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  ABGLEICH-CONTROL
      ******************************************************************
       01  ABGLEICH-CONTROL-GESAMT.
         05 NI-ABGLEICH-CONTROL-TAB.
           10 NI-ABGLEICH-CONTROL-ELE PIC S9(4) COMP OCCURS 29 TIMES.
         05 NI-ABGLEICH-CONTROL REDEFINES NI-ABGLEICH-CONTROL-TAB.
           10 NI-AK-KEY               PIC S9(4) COMP.
           10 NI-AK-SQL-START         PIC S9(4) COMP.
           10 NI-AK-SQL-ENDE          PIC S9(4) COMP.
           10 NI-AK-SQL-ANZAHL        PIC S9(4) COMP.
           10 NI-AK-SATZ-ANZAHL       PIC S9(4) COMP.
           10 NI-AK-OKI-START         PIC S9(4) COMP.
           10 NI-AK-OKI-ENDE          PIC S9(4) COMP.
           10 NI-AK-OKI-SQL-ANZAHL    PIC S9(4) COMP.
           10 NI-AK-OKI-SATZ-ANZAHL   PIC S9(4) COMP.
           10 NI-AK-SQL-STATUS        PIC S9(4) COMP.
           10 NI-AK-STATUS            PIC S9(4) COMP.
           10 NI-AK-OKI-BIND-ANZ      PIC S9(4) COMP.
           10 NI-AK-OKI-BIND-OKAN     PIC S9(4) COMP.
           10 NI-AK-OKI-SQL-BIND      PIC S9(4) COMP.
           10 NI-AK-LOGMAS-START      PIC S9(4) COMP.
           10 NI-AK-LOGMAS-ENDE       PIC S9(4) COMP.
           10 NI-AK-LOGMAS-DATEI      PIC S9(4) COMP.
           10 NI-AK-LOGMAS-GEN        PIC S9(4) COMP.
           10 NI-AK-EINARB-DATEI      PIC S9(4) COMP.
           10 NI-AK-EINARB-GEN        PIC S9(4) COMP.
           10 NI-AK-LOGMAS-ANZ        PIC S9(4) COMP.
           10 NI-AK-EINARB-ANZ        PIC S9(4) COMP.
           10 NI-AK-LOGMAS-STATUS     PIC S9(4) COMP.
           10 NI-AK-EINARB-STATUS     PIC S9(4) COMP.
           10 NI-AK-LOGMAS-JOB        PIC S9(4) COMP.
           10 NI-AK-EINARB-JOB        PIC S9(4) COMP.
           10 NI-AK-LOGMAS-TSTP       PIC S9(4) COMP.
           10 NI-AK-EINARB-TSTP       PIC S9(4) COMP.
         05 DCL-ABGLEICH-CONTROL.
           10 AK-KEY               PIC X(26).
           10 AK-SQL-START         PIC X(26).
           10 AK-SQL-ENDE          PIC X(26).
           10 AK-SQL-ANZAHL        PIC S9(9)  COMP.
           10 AK-SATZ-ANZAHL       PIC S9(9)  COMP.
           10 AK-OKI-START         PIC X(26).
           10 AK-OKI-ENDE          PIC X(26).
           10 AK-OKI-SQL-ANZAHL    PIC S9(9)  COMP.
           10 AK-OKI-SATZ-ANZAHL   PIC S9(9)  COMP.
           10 AK-SQL-STATUS        PIC X(10).
           10 AK-STATUS            PIC X(1).
           10 AK-OKI-BIND-ANZ      PIC S9(9) COMP.
           10 AK-OKI-BIND-OKAN     PIC S9(9) COMP.
           10 AK-OKI-SQL-BIND      PIC S9(9) COMP.
           10 AK-LOGMAS-START      PIC X(26).
           10 AK-LOGMAS-ENDE       PIC X(26).
           10 AK-LOGMAS-DATEI      PIC X(36).
           10 AK-LOGMAS-GEN        PIC X(8).
           10 AK-EINARB-DATEI      PIC X(36).
           10 AK-EINARB-GEN        PIC X(8).
           10 AK-LOGMAS-ANZ        PIC S9(9) USAGE COMP.
           10 AK-EINARB-ANZ        PIC S9(9) USAGE COMP.
           10 AK-LOGMAS-STATUS     PIC X(1).
           10 AK-EINARB-STATUS     PIC X(1).
           10 AK-LOGMAS-JOB        PIC X(8).
           10 AK-EINARB-JOB        PIC X(8).
           10 AK-LOGMAS-TSTP       PIC X(26).
           10 AK-EINARB-TSTP       PIC X(26).
      ******************************************************************
