      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0113.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0113.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:52:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   14 May 1998 12:23:02   P13SP99
      *Neuerstellung
      *
      *   Rev 1.1   14 May 1998 10:33:56   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:48:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0113 - KONSTANTEN SUB-TYPEN               *
      *    --------------------------------------------------------   *
      *    VERSION   01                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMITZ                                          *
      *    DATUM:    10.02.1998                                       *
      *    STAND:    10.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!10.02.98! 01 ! NEU                       ! SCHMITZ  *
      *         !22.04.98! 02 ! AKTUALISIERUNG ANALOG DOKU! HUEBNER  *
      *         !23.04.98! 03 ! AKTUALISIERUNG ANALOG DOKU! HUEBNER  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      * NAMENSKONVENTIONEN:
      *
      * K-ST-HAUSHALT-PERSONKUNDENART
      * ! !     !           !
      * ! !     !           +--------- ATTRIBUT OHNE SUFFIX "SUBTYPE"
      * ! !     +--------------------- SUB-TYP AUSPRAEGUNG
      * ! +--------------------------- SUBTYP
      * +----------------------------- KONSTANTE
      *
      *
      *    AUSPRÄGUNGEN DER SUB-TYPEN JE TABELLE
      *
       01  TABELLE-PERSON.
           05 K-ST-HAUSHALT-PERSKUNDART  PIC X(001) VALUE 'H'.
           05 K-ST-JURPERS-PERKUNDART    PIC X(001) VALUE 'J'.
           05 K-ST-NATPERS-PERSKUNDART   PIC X(001) VALUE 'N'.
      *
       01  TABELLE-GUTHABENPRODUKT.
220498**** 05 K-ST-AKTIERENTEBTLG-GHP    PIC X(001) VALUE 'A'.
           05 K-ST-BAUSPAR-GHP           PIC X(001) VALUE 'B'.
220498**** 05 K-ST-FONTANT-GHP           PIC X(001) VALUE 'F'.
220498**** 05 K-ST-SPARGUT-GHP           PIC X(001) VALUE 'S'.
      *
       01  TABELLE-NOTIZ.
           05 K-ST-NTZEIGENTUM-NOTIZ     PIC X(001) VALUE 'E'.
           05 K-ST-NTZEINKOM-NOTIZ       PIC X(001) VALUE 'K'.
           05 K-ST-NTZMIETE-NOTIZ        PIC X(001) VALUE 'M'.
           05 K-ST-NTZPERS-NOTIZ         PIC X(001) VALUE 'P'.
           05 K-ST-NTZVERTR-NOTIZ        PIC X(001) VALUE 'V'.
220498     05 K-ST-NTZHYPO               PIC X(001) VALUE 'H'.
      *
       01  TABELLE-VERSOBJEKT.
           05 K-ST-GEBAEUDE-VERSOBJEKT   PIC X(001) VALUE 'G'.
           05 K-ST-KFZ-VERSOBJEKT        PIC X(001) VALUE 'K'.
220498***  05 K-ST-PERS-VERSOBJEKT       PIC X(001) VALUE 'P'.
220498     05 K-ST-PERS-VERSPERSD        PIC X(001) VALUE 'P'.
      *
       01  TABELLE-VERSICHERTERTARIF.
           05 K-ST-VERSKRANKENTARIF-VET  PIC X(001) VALUE 'K'.
           05 K-ST-VERSLEBENTARIF-VET    PIC X(001) VALUE 'L'.
           05 K-ST-VERSSACHTARIF-VET     PIC X(001) VALUE 'S'.
           05 K-ST-UNFALL-VET            PIC X(001) VALUE 'U'.
      *
       01  TABELLE-VERSLEBENTARIF.
220498**** 05 K-ST-LEBENDYN-LTA          PIC X(001) VALUE 'D'.
           05 K-ST-LEBENRW-LTA           PIC X(001) VALUE 'R'.
      *
       01  TABELLE-VERSSACHTARIF.
220498**** 05 K-ST-HAFTPFLICHT-SAT       PIC X(001) VALUE 'H'.
220498**** 05 K-ST-KFZMOPEDVERS-SAT      PIC X(001) VALUE 'K'.
220498**** 05 K-ST-RECHTSCHUTZ-SAT       PIC X(001) VALUE 'R'.
220498**** 05 K-ST-SVHAUSRATGLAS-SAT     PIC X(001) VALUE 'G'.
220498**** 05 K-ST-SVHMASCHNINENBU-SAT   PIC X(001) VALUE 'M'.
220498**** 05 K-ST-SVWOHNGEBAEUDE-SAT    PIC X(001) VALUE 'W'.
220498**** 05 K-ST-TRANSPLUFTUNFALL-SAT  PIC X(001) VALUE 'T'.
220498     05 K-ST-KFZMOPEDVERS-SAT      PIC X(001) VALUE 'M'.
      *
220498*01  TABELLE-GUTHABENPRODUKT.
220498**** 05 K-ST-FONDVERTR-GHP         PIC X(001) VALUE 'F'.
      *
       01  TABELLE-AUTHORISIERUNG.
           05 K-ST-OBJAUTH-AUTART        PIC X(001) VALUE 'O'.
           05 K-ST-OBJASPAUTH-AUTART     PIC X(001) VALUE 'A'.
           05 K-ST-PERFORMAUTH-AUTART    PIC X(001) VALUE 'P'.
           05 K-ST-READAUTH-AUTART       PIC X(001) VALUE 'R'.
           05 K-ST-WRITEAUTH-AUTART      PIC X(001) VALUE 'W'.
230498     05 K-ST-READASPE-AUTART       PIC X(001) VALUE 'E'.
230498     05 K-ST-WRITEASPE-AUTART      PIC X(001) VALUE 'T'.
230498     05 K-ST-PERFOASPE-AUTART      PIC X(001) VALUE 'M'.
      *
       01  TABELLE-BENUTZER.
           05 K-ST-GUESTUSER-BENART      PIC X(001) VALUE 'G'.
      *
230498 01  TABELLE-DOKUMENT.
230498     05 K-ST-DOK-ARBAUFTRAG        PIC X(004) VALUE 'AAUF'.
230498     05 K-ST-DOK-ORGDOKUMENT       PIC X(004) VALUE 'ORGD'.
230498     05 K-ST-DOK-PARTDOKUMENT      PIC X(004) VALUE 'DOPA'.
230498     05 K-ST-DOK-RUECKLKONTR       PIC X(004) VALUE 'RKL '.
230498     05 K-ST-DOK-STARWDOKUMENT     PIC X(004) VALUE 'STWD'.
230498     05 K-ST-DOK-TERMIN            PIC X(004) VALUE 'TERM'.
230498     05 K-ST-DOK-VERMPDOKUMENT     PIC X(004) VALUE 'VPD '.
      *
230498 01  TABELLE-ORGDOKUMENT.
230498     05 K-ST-ORGD-ORGDOKUMENT      PIC X(004) VALUE 'ORGD'.
230498     05 K-ST-ORGD-ARBAUFTRAG       PIC X(004) VALUE 'AAUF'.
230498     05 K-ST-ORGD-TERMIN           PIC X(004) VALUE 'TERM'.
      *
      ******** ENDE COPY-MEMBER KONSTANTEN (DRKI0113)      ********
