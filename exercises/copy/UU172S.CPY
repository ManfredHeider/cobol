      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU172S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU172S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Feb 02 1996 13:07:00   T13AE24
      *AFVB0266 KNR_ST_TYP neu aufgenommen
      *
      *   Rev 1.1   Dec 13 1995 15:28:06   T13AE14
      *AFVB0252: Erweiterung Schnittstelle Ma-Konten-DTA um Fax-Nr.
      *
      *   Rev 1.0   Nov 04 1995 09:55:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   3    23.02.95 14:44:07
      ** -- UU172S ----> SATZDEFINITION MITARBEITER-KONTENSATZ --
      **                                                             **
      *****************************************************************
      **                                                             **
      **   MITARBEITER-KONTENSATZ                                    **
      **                                                             **
      **   DIESER COPYSATZ ENTSPRICHT DER SATZ-DEFINITION DES        **
      **   MITARBEITER-KONTENSATZES.                                 **
      **   DIE FELDNAMEM UND DEREN BEDEUTUNG ENTSPRECHEN DEM DERZEIT **
      **   GUELTIGEN DATENKATALOG FUER DEN MITARBEITER-KONTENSATZ.   **
      **                                                             **
      **   ACHTUNG:                                                  **
      **           ES WERDEN NUR DIE LETZTEN BEIDEN STELLEN DER      **
      **           INTERNEN STUFENNUMMER DES VB'S UEBERMITTELT.      **
      **           GEFAHR VON REDUNDANTEN STUF.-NUMMERN BEI ER-      **
      **           NEUTER NUMMERNEINFUEHRUNG                         **
      **                                                             **
      **   NAME:       UU172S (FRUEHER: OM510S)                      **
      **                                                             **
      **   SATZLAENGE: 800  BYTE'S                                   **
      **                                                             **
      **   STAND:  11.05.87   ABRNR-DATEN EINGEFüGT                  **
      **                      RESERVEBEREICH AUF 47 BYTES VERKüRZT   **
      **                      BETRAGSFELDER VON S9(7)V99             **
      **                                    AUF S9(9)V99 ERWEITERT.  **
      **                                                             **
      **   STAND:  30.07.90   tel-nr pic x(14) eingefuegt            **
      **           (BIEHL)    RESERVEBEREICH AUF 33 BYTES VERKuerzt  **
      **                                     (vorher: 47 byte)       **
      **                                                             **
      **   STAND:  06.05.93   Feld DIREKTIONSLEITER X(10) eingefügt, **
      **         (Berghofer)  Feld HAUPT-AGT-NR in UEBETREUER        **
      **                      umbenannt und Feld ABH-AGT-NR in       **
      **                      DBETREUER umbenannt, sowie BEW-KZ      **
      **                      eingeführt. RESERVEBEREICH auf         **
      **                      19 Bytes gekürzt (vorher 33 Bytes).    **
      **                                                             **
      **   FEVB0157                                                  **
      **   STAND:  29.09.94   FELD BKTONR VON S9(10) COMP-3 AUF      **
      **         (SCHMIDT)    S9(11) COMP-3 GEÄNDERT, WEGEN 11 ST.   **
      **                      KONTO-NR. IN ÖSTEREICH.                **
      **                                                             **
      **   AFVB0189                                                  **
      **   STAND:  21.02.95   FELD GEBDAT - GEBURTSDATUM IN DEN      **
      **         (WIECHERS)   DATENSATZ AUFGENOMMEN                  **
      **                                                             **
      **                                                             **
      **   AFVB0252                                                  **
      **   STAND:  04.12.95   FELD FAX-NR - TELEFAX-NUMMER IN DEN    **
      **         (WOLFF)      DATENSATZ AUFGENOMMEN                  **
      **                                                             **
      **                                                             **
      **   AFVB0266                                                  **
      **   STAND:  04.12.95   FELD KNR-ST-TYP - KNR-ST-TYP IN DEN    **
      **         (WOLFF)      DATENSATZ AUFGENOMMEN                  **
      **                                                             **
      **                                                             **
      *****************************************************************
      **                                                             **
       01  MA-KONTENSATZ.
      *
           05  SA                PIC X(6).
      *
           05  MA-NR.
               10  ORGGS.
                   15  ORG       PIC 9.
                   15  GS        PIC 99.
               10  RORGGS REDEFINES ORGGS PIC 999.
               10  AGT           PIC 9999.
               10  AV.
                   15  AV1       PIC 9.
                   15  AV2       PIC 9.
               10  PRZ           PIC X.
           05  RMA-NR     REDEFINES MA-NR.
               10  MA-NR1-9      PIC 9(9).
               10  MA-NR10       PIC X.
      *
           05  UEBETREUER.
               10  UEBETREUER-VB-NR1-9.
                   15  UEBETREUER-VB-NR1-7.
                       20  UEBETREUER-VB-ORGGS PIC 9(3).
                       20  UEBETREUER-VB-AGT   PIC 9(4).
                   15  UEBETREUER-VB-AV12      PIC 9(2).
               10  UEBETREUER-VB-NR-PRZ        PIC X.
      *
           05  DBETREUER.
               10  DBETREUER-VB-NR1-9.
                   15  DBETREUER-VB-NR1-7.
                       20  DBETREUER-VB-ORGGS  PIC 9(3).
                       20  DBETREUER-VB-AGT    PIC 9(4).
                   15  DBETREUER-VB-AV12       PIC 9(2).
               10  DBETREUER-VB-NR-PRZ         PIC X.
      *
           05  FUNKTION.
               10  FUNK1         PIC 9.
               10  FUNK2         PIC 9.
               10  FUNK3         PIC 9.
               10  FUNK4         PIC 9.
      *
           05  EROEFF-DAT        PIC S9(6)         COMP-3.
           05  SCHLIE-DAT        PIC S9(6)         COMP-3.
      *
           05  EINR-DAT          PIC S9(6)         COMP-3.
           05  AEND-DAT          PIC S9(6)         COMP-3.
           05  AEND-NR           PIC S9(5)         COMP-3.
      *
           05  MA-ANSCHRIFT.
               10  ANREDE        PIC 9.
               10  ZUNAME        PIC X(21).
               10  VORNAME       PIC X(16).
               10  TITEL         PIC X(14).
               10  ZUSATZ        PIC X(15).
               10  AUSL-KZ       PIC XXX.
               10  PLZ           PIC X(5).
               10  ORT           PIC X(16).
               10  STR           PIC X(25).
      *
           05  GESTITEL.
               10  GESTIT1       PIC 99.
               10  GESTITVON1    PIC S9(6)     COMP-3.
               10  GEBDAT        REDEFINES GESTITVON1
                                 PIC S9(6)     COMP-3.
               10  GESTIT2       PIC 99.
               10  GESTITVON2    PIC S9(6)     COMP-3.
      *
           05  STORESPROZ.
               10  STORES1       PIC S999V9    COMP-3.
               10  STORESVON1    PIC S9(6)     COMP-3.
               10  STORES2       PIC S999V9    COMP-3.
               10  STORESVON2    PIC S9(6)     COMP-3.
      *
           05  BANKVERB.
               10  BKTOINHORT    PIC X(25).
               10  BKTONR        PIC S9(11)    COMP-3.
               10  GELDINST      PIC X(22).
               10  BLZ           PIC S9(8)     COMP.
      *
           05  ABRNR-LGJ         PIC S999      COMP-3.
           05  ABRNR-FGJ         PIC S999      COMP-3.
      *
           05  SU-TAB-LGJ        OCCURS 10.
               10  VGT-LGJ       PIC S9(9)V99  COMP-3.
               10  STOR-LGJ      PIC S9(9)V99  COMP-3.
      *
           05  SUAUSZ-LGJ        PIC S9(9)V99  COMP-3.
      *
           05  SU-TAB-FGJ        OCCURS 10.
               10  VGT-FGJ       PIC S9(9)V99  COMP-3.
               10  STOR-FGJ      PIC S9(9)V99  COMP-3.
      *
           05  SUAUSZ-FGJ        PIC S9(9)V99  COMP-3.
      *
           05  AVBG-DATEN.
               10  STRUK-DIR      PIC XXX.
               10  STRUK-STUFE    PIC XX.
               10  MWST-KZ        PIC X.
               10  W-STRUK-STUFE  PIC XX.
      *
           05  UBERW-DAETEN.
               10  UEB-BETRAG-LGJ PIC S9(7)V99 COMP-3.
               10  UEB-DATUM-LGJ  PIC S9(6)    COMP-3.
               10  UEB-BETRAG-FGJ PIC S9(7)V99 COMP-3.
               10  UEB-DATUM-FGJ  PIC S9(6)    COMP-3.
      *
           05  ABRNR-DATEN.
               10  ANR-DATUM-LGJ  PIC S9(6)    COMP-3.
               10  ANR-DATUM-FGJ  PIC S9(6)    COMP-3.
               10  ANR-DATUM-AP   PIC S9(6)    COMP-3.
      *
           05  SPERR-KENNZEICHEN  PIC X.
      *
           05  TEL-NR             PIC X(14).
      *
           05  BEW-KZ             PIC X(4).
      *
           05  DIRBETREUER.
               10  DIRBETREUER-VB-NR1-9.
                   15  DIRBETREUER-VB-NR1-7.
                       20  DIRBETREUER-VB-ORGGS  PIC 9(3).
                       20  DIRBETREUER-VB-AGT    PIC 9(4).
                   15  DIRBETREUER-VB-AV12       PIC 9(2).
               10  DIRBETREUER-VB-NR-PRZ         PIC  X.
      *
           05  FAX-NR             PIC X(14).
      *
           05  KNR-ST-TYP         PIC S9(4) BINARY.
      *
           05  RESERVE-BEREICH    PIC X(3).
      *
           05  HPT-KTO.
               10  KTOSTANDH-LGJ  PIC S9(9)V99 COMP-3.
               10  BUCHDATH-LGJ   PIC S9(6)    COMP-3.
               10  KTOSTANDH-FGJ  PIC S9(9)V99 COMP-3.
               10  BUCHDATH-FGJ   PIC S9(6)    COMP-3.
      *
           05  UKTO-TAB           OCCURS 9.
               10  KTOSTAND-LGJ   PIC S9(9)V99 COMP-3.
               10  BUCHDAT-LGJ    PIC S9(6)    COMP-3.
               10  KTOSTAND-FGJ   PIC S9(9)V99 COMP-3.
               10  BUCHDAT-FGJ    PIC S9(6)    COMP-3.
      *
      ********
      ******** ENDE COPY-SATZ - MITARBEITER-KONTENSATZ (UU172S) ******
      ********


