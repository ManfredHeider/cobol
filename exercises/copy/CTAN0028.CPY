      ******************************************************************
      * DCLGEN TABLE(T13VBS.TBAN0028)                                  *
      *        LIBRARY(T130005.VB010000.SRCDCL(CTAN0028))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAN0028 TABLE
CCC004*    ( GEIG_VORSCHLAG_NR              CHAR(9) NOT NULL,
CCC004*      GEIG_PIN                       INTEGER NOT NULL,
CCC004*      GEIG_AVERM                     INTEGER NOT NULL,
CCC004*      GEIG_AVERM_PZ                  CHAR(1) NOT NULL,
CCC004*      GEIG_VM                        INTEGER NOT NULL,
CCC004*      GEIG_VM_PZ                     CHAR(1) NOT NULL,
CCC004*      GEIG_NAME                      CHAR(26) NOT NULL,
CCC004*      GEIG_VORNAME                   CHAR(26) NOT NULL,
CCC004*      GEIG_STRASSE                   CHAR(45) NOT NULL,
CCC004*      GEIG_PLZ                       CHAR(6) NOT NULL,
CCC004*      GEIG_ORT                       CHAR(30) NOT NULL,
CCC004*      GEIG_LKZ                       CHAR(3) NOT NULL,
CCC004*      GEIG_KUNDE_NR                  DECIMAL(18, 0) NOT NULL,
CCC004*      GEIG_ANREDE                    CHAR(5) NOT NULL,
CCC004*      GEIG_TITEL                     CHAR(20) NOT NULL,
CCC004*      GEIG_GEBURT_DATUM              DATE NOT NULL,
CCC004*      GEIG_EMAIL                     CHAR(50) NOT NULL,
CCC004*      GEIG_TELEFON                   CHAR(30) NOT NULL,
CCC004*      GEIG_IBAN                      CHAR(34) NOT NULL,
CCC004*      GEIG_BANK_NAME                 CHAR(50) NOT NULL,
CCC004*      GEIG_NAME_K_INH                CHAR(26) NOT NULL,
CCC004*      GEIG_VNAME_K_INH               CHAR(26) NOT NULL,
CCC004*      GEIG_OKI_ZEITPUNKT             TIMESTAMP NOT NULL,
CCC004*      GEIG_STEUERNUMMER              CHAR(13) NOT NULL,
CCC004*      GEIG_VERSION                   CHAR(4) NOT NULL,
CCC004*      GEIG_FIRMA                     CHAR(50) NOT NULL,
CCC004*      GEIG_ZEIT_INSERT               TIMESTAMP NOT NULL,
CCC004*      GEIG_KENNUNG_INS               CHAR(8) NOT NULL,
CCC004*      GEIG_ZEIT_UPDATE               TIMESTAMP NOT NULL,
CCC004*      GEIG_KENNUNG_UP                CHAR(8) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13VBS.TBAN0028                    *
      ******************************************************************
       01  TBAN0028-GESAMT.
           10 GEIG-VORSCHLAG-NR    PIC X(9).
           10 GEIG-PIN             PIC S9(9) USAGE COMP-5.
           10 GEIG-AVERM           PIC S9(9) USAGE COMP-5.
           10 GEIG-AVERM-PZ        PIC X(1).
           10 GEIG-VM              PIC S9(9) USAGE COMP-5.
           10 GEIG-VM-PZ           PIC X(1).
           10 GEIG-NAME            PIC X(26).
           10 GEIG-VORNAME         PIC X(26).
           10 GEIG-STRASSE         PIC X(45).
           10 GEIG-PLZ             PIC X(6).
           10 GEIG-ORT             PIC X(30).
           10 GEIG-LKZ             PIC X(3).
           10 GEIG-KUNDE-NR        PIC S9(18)V USAGE COMP-3.
           10 GEIG-ANREDE          PIC X(5).
           10 GEIG-TITEL           PIC X(20).
           10 GEIG-GEBURT-DATUM    PIC X(10).
           10 GEIG-EMAIL           PIC X(50).
           10 GEIG-TELEFON         PIC X(30).
           10 GEIG-IBAN            PIC X(34).
           10 GEIG-BANK-NAME       PIC X(50).
           10 GEIG-NAME-K-INH      PIC X(26).
           10 GEIG-VNAME-K-INH     PIC X(26).
           10 GEIG-OKI-ZEITPUNKT   PIC X(26).
           10 GEIG-STEUERNUMMER    PIC X(13).
           10 GEIG-VERSION         PIC X(4).
           10 GEIG-FIRMA           PIC X(50).
           10 GEIG-ZEIT-INSERT     PIC X(26).
           10 GEIG-KENNUNG-INS     PIC X(8).
           10 GEIG-ZEIT-UPDATE     PIC X(26).
           10 GEIG-KENNUNG-UP      PIC X(8).
           10 PGEB-GES             PIC S9(4) USAGE COMP-5.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 31      *
      ******************************************************************
