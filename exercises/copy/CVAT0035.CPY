      *****************************************************************
      *    ERSTELLT: PRETSCH                                          *
      *    DATUM:    16.11.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01244!16.11.12!1.0 ! NEU                       ! B.PRETSCH *
290513* AVB01244!29.05.13!2.0 ! EINBAU IBAN-REGEL ATTR.   ! B.PRETSCH *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIAT0035) BLZ                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0035)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIAT0035 TABLE
CCC004*    (
CCC004*      KNR_LAND_KZ                 SMALLINT NOT NULL
CCC004*    , BLZ                         CHAR(11) NOT NULL
CCC004*    , KNR_NIEDERL_INTERN          SMALLINT NOT NULL
CCC004*    , PLZ                         CHAR(09) NOT NULL
CCC004*    , BIC                         CHAR(11) NOT NULL
CCC004*    , NIEDERL_KZ                  CHAR(01) NOT NULL
CCC004*    , BANKNAME                    CHAR(58) NOT NULL
CCC004*    , KTY_LAND_KZ                 SMALLINT NOT NULL
CCC004*    , KTY_NIEDERL_INTERN          SMALLINT NOT NULL
CCC004*    , BLZ_LAST_UPDATE             TIMESTAMP NOT NULL
CCC004*    , AENDERUNGS_KZ               CHAR(01)
CCC004*    , LOESCH_KZ                   SMALLINT NOT NULL
CCC004*    , IBAN_REGEL                  INTEGER  NOT NULL
CCC004*    , FOLGE_BLZ                   CHAR(11) NOT NULL
CCC004*    , SATZNUMMER                  INTEGER
CCC004*    )
CCC003*    END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIAT0035                           *
      *****************************************************************

       01  VIAT0035-GESAMT.
           05 NI-VIAT0035-TAB.
290513        10 NI-VIAT0035-ELE      PIC S9(04)  BINARY OCCURS  15.
           05 NI-VIAT0035             REDEFINES   NI-VIAT0035-TAB.
              10 NI-KNR-LAND-KZ          PIC S9(04)       BINARY.
              10 NI-BLZ                  PIC S9(04)       BINARY.
              10 NI-KNR-NIEDERL-INTERN   PIC S9(04)       BINARY.
              10 NI-PLZ                  PIC S9(04)       BINARY.
              10 NI-BIC                  PIC S9(04)       BINARY.
              10 NI-NIEDERL-KZ           PIC S9(04)       BINARY.
              10 NI-BANKNAME             PIC S9(04)       BINARY.
              10 NI-KTY-LAND-KZ          PIC S9(04)       BINARY.
              10 NI-KTY-NIEDERL-INTERN   PIC S9(04)       BINARY.
              10 NI-BLZ-LAST-UPDATE      PIC S9(04)       BINARY.
290513        10 NI-AENDERUNGS-KZ        PIC S9(04)       BINARY.
290513        10 NI-LOESCH-KZ            PIC S9(04)       BINARY.
290513        10 NI-IBAN-REGEL           PIC S9(04)       BINARY.
290513        10 NI-FOLGE-BLZ            PIC S9(04)       BINARY.
290513        10 NI-SATZNUMMER           PIC S9(04)       BINARY.

           05 DCL-VIAT0035.
              10 KNR-LAND-KZ             PIC S9(04)       BINARY.
              10 BLZ                     PIC  X(11).
              10 KNR-NIEDERL-INTERN      PIC S9(04)       BINARY.
              10 PLZ                     PIC  X(09).
              10 BIC                     PIC  X(11).
              10 NIEDERL-KZ              PIC  X(01).
              10 BANKNAME                PIC  X(58).
              10 KTY-LAND-KZ             PIC S9(04)       BINARY.
              10 KTY-NIEDERL-INTERN      PIC S9(04)       BINARY.
              10 BLZ-LAST-UPDATE         PIC  X(26).
290513        10 AENDERUNGS-KZ           PIC  X(01).
290513        10 LOESCH-KZ               PIC S9(04)       BINARY.
290513        10 IBAN-REGEL              PIC S9(09)       BINARY.
290513        10 FOLGE-BLZ               PIC  X(11).
290513        10 SATZNUMMER              PIC S9(09)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  15                    *
      *****************************************************************
