      ******************************************************************
      * DCLGEN TABLE(BBH9.T13AES.VIVB0106)                             *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVB0106))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0106 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      REG_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      REG_DAT_VON                    TIMESTAMP NOT NULL,
CCC004*      REG_HAFT_AB                    DATE,
CCC004*      REG_NR                         CHAR(30),
CCC004*      REG_NAME                       CHAR(50),
CCC004*      REG_VORNAME                    CHAR(50),
CCC004*      REG_PLZ                        CHAR(10),
CCC004*      REG_ORT                        CHAR(50),
CCC004*      REG_STR                        CHAR(50),
CCC004*      REG_GEBDAT                     DATE,
CCC004*      REG_AKTION                     CHAR(3) NOT NULL,
CCC004*      REG_STATUS                     CHAR(6) NOT NULL,
CCC004*      REG_REGISTER_TS                CHAR(35),
CCC004*      REG_TAET_LAND                  CHAR(60),
CCC004*      REG_CODE                       INTEGER,
CCC004*      REG_ART                        CHAR(1) NOT NULL,
CCC004*      REG_IHK_NR                     SMALLINT,
CCC004*      REG_BUCH_KZ                    CHAR(1) NOT NULL,
CCC004*      REG_CODE_TEXT                  CHAR(255)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0106                           *
      ******************************************************************
       01  VIVB0106-GESAMT.
        05  NI-VIVB0106-TAB.
           10  NI-VIVB0106-ELE     PIC S9(04)  COMP   OCCURS  13  TIMES.
        05  NI-VIVB0106            REDEFINES   NI-VIVB0106-TAB.
           10 NI-REG-HAFT-AB        PIC S9(4) USAGE COMP.
           10 NI-REG-NR             PIC S9(4) USAGE COMP.
           10 NI-REG-NAME           PIC S9(4) USAGE COMP.
           10 NI-REG-VORNAME        PIC S9(4) USAGE COMP.
           10 NI-REG-PLZ            PIC S9(4) USAGE COMP.
           10 NI-REG-ORT            PIC S9(4) USAGE COMP.
           10 NI-REG-STR            PIC S9(4) USAGE COMP.
           10 NI-REG-GEBDAT         PIC S9(4) USAGE COMP.
           10 NI-REG-REGISTER-TS    PIC S9(4) USAGE COMP.
           10 NI-REG-TAET-LAND      PIC S9(4) USAGE COMP.
           10 NI-REG-CODE           PIC S9(4) USAGE COMP.
           10 NI-REG-IHK-NR         PIC S9(4) USAGE COMP.
           10 NI-REG-CODE-TEXT      PIC S9(4) USAGE COMP.
        05  DCL-VIVB0106.
           10 MA-ST-NR              PIC S9(09) USAGE COMP.
           10 REG-LAST-UPDATE       PIC X(026).
           10 REG-DAT-VON           PIC X(026).
           10 REG-HAFT-AB           PIC X(010).
           10 REG-NR                PIC X(030).
           10 REG-NAME              PIC X(050).
           10 REG-VORNAME           PIC X(050).
           10 REG-PLZ               PIC X(010).
           10 REG-ORT               PIC X(050).
           10 REG-STR               PIC X(050).
           10 REG-GEBDAT            PIC X(010).
           10 REG-AKTION            PIC X(003).
           10 REG-STATUS            PIC X(006).
           10 REG-REGISTER-TS       PIC X(035).
           10 REG-TAET-LAND         PIC X(060).
           10 REG-CODE              PIC S9(08) USAGE COMP.
           10 REG-ART               PIC X(001).
           10 REG-IHK-NR            PIC S9(04) USAGE COMP.
           10 REG-BUCH-KZ           PIC X(001).
           10 REG-CODE-TEXT         PIC X(255).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
