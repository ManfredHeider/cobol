      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0794.cpy  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:\MF\CPY\drki0794.cpv  $
      *
      *   Rev 2.8   Apr 06 2000 11:18:56   T13EX39
      *AKI00099 LOE-KZ
      *
      *   Rev 2.6   Jul 28 1999 15:02:04   T13EX47
      *in produktion
      *
      *
      *   Rev 2.3   May 10 1999 16:22:26   T13EX47
      *INFOSPERRE-KZ eingeführt
      *
      *   Rev 2.2   May 10 1999 11:06:14   T13EX47
      *Diverse Aenderungen ...
      *
      *   Rev 2.1   Apr 26 1999 17:10:04   T13EX47
      *Person1 und 1 unterschieden
      *
      *   Rev 1.0   Apr 21 1999 16:59:40   T13EX47
      *Neu Erstellt
      *
      *   Rev 2.0   Mar 30 1999 16:54:18   T13EX47
      *Versionswechsel
      *
      *   Rev 1.0   06 Oct 1998 15:41:04   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    20.07.93 10:07:09
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    ! NEU                       ! BERTRAM  *
      *         !18.02.99!    ! -alt und -neu fuer Felder !   gbe    *
      *         !        !    ! INV-ANREDE und INV-NAME   !          *
      *--------------------------------------------------------------*
      *         !30.03.99!    ! alle comp-definitionen    !   gbe    *
      *         !        !    ! raus, fillers verkürzt    !          *
      *--------------------------------------------------------------*
      *AKI00099 !20.03.00!    ! INV-LOE-KZ aufgenommen    ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *--------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER ERMITTELTE VERAENDERUNGEN BFG:[IMMO]INVEST *
      *    ----------------------------------------------------------- *
      *    ERSTELLT 12/98 VON GERD BERTRAM (GBE)                       *
      *    AUF DER BASIS VON UU080S.CPY                                *
      *    LAENGE: 192 ==> 224 ==> 400 ==> 550                         *
      *----------------------------------------------------------------*

       01  DRKI0794.

         05  INV-SATZ.
           10  INV-SA                    PIC X(001).
      * Verarbeitungs-Kennzeichen :
      *   I=Insert
      *   U=Update
      *   D=Delete
           10  INV-VERARB-KZ             PIC X(001).

240699     10  INV-DTA-GES               PIC X(001).

         05  INV-CSATZ.
           10  INV-SANR                  PIC X(001).
           10  INV-ORGA.
               15  INV-BFGNL             PIC 9(004).
               15  INV-BFGKTO            PIC 9(010).
19             15  INV-BFGGES            PIC 9(001).

           10  INV-PERSON.
               15  INV-GES               PIC 9(002).
               15  INV-GES-KZ            PIC X.
               15  INV-ORGGS             PIC 9(003).
               15  INV-ORGGS-KZ          PIC X.
               15  INV-VERM              PIC 9(010).
               15  INV-VERM-KZ           PIC X.
               15  INV-PIF               PIC X(020).
58             15  INV-PIF-KZ            PIC X.
               15  INV-ANREDE-alt        PIC X(002).
               15  INV-ANREDE-neu        PIC X(002).
63             15  INV-ANREDE-KZ         PIC X.
               15  INV-NNAME-1-alt       PIC X(040).
               15  INV-NNAME-1-neu       PIC X(040).
               15  INV-NNAME-1-KZ        PIC X.
               15  INV-VNAME-1-alt       PIC X(040).
               15  INV-VNAME-1-neu       PIC X(040).
               15  INV-VNAME-1-KZ        PIC X.
               15  INV-NNAME-2-alt       PIC X(040).
               15  INV-NNAME-2-neu       PIC X(040).
               15  INV-NNAME-2-KZ        PIC X.
               15  INV-VNAME-2-alt       PIC X(040).
               15  INV-VNAME-2-neu       PIC X(040).
               15  INV-VNAME-2-KZ        PIC X.
               15  INV-STRASSE           PIC X(042).
338            15  INV-STRASSE-KZ        PIC X.
               15  INV-PLZORT.
                   20  INV-PLZ           PIC X(008).
                   20  INV-PLZ-KZ        PIC X.
                   20  INV-ORT           PIC X(040).
                   20  INV-ORT-KZ        PIC X.
               15  INV-GEB-DAT-2         pic 9(008).
               15  INV-GEB-DAT-2-R redefines INV-GEB-DAT-2.
                   20 INV-GEB-DAT-2-JH   PIC 9(002).
                   20 INV-GEB-DAT-2-JJ   PIC 9(002).
                   20 INV-GEB-DAT-2-MM   PIC 9(002).
                   20 INV-GEB-DAT-2-TT   PIC 9(002).
               15  INV-GEB-DAT-2-KZ      PIC X.
               15  INV-GEB-DAT-1         pic 9(008).
               15  INV-GEB-DAT-1-R redefines INV-GEB-DAT-1.
                   20 INV-GEB-DAT-1-JH   PIC 9(002).
                   20 INV-GEB-DAT-1-JJ   PIC 9(002).
                   20 INV-GEB-DAT-1-MM   PIC 9(002).
                   20 INV-GEB-DAT-1-TT   PIC 9(002).
               15  INV-GEB-DAT-1-KZ      PIC X.
               15  INV-BERUF             PIC X(002).
               15  INV-BERUF-KZ          PIC X.
               15  INV-LAND              PIC X(003).
505            15  INV-LAND-KZ           PIC X.
550            15  FILLER-C1             PIC X(049).

            10 INV-PRODUKT REDEFINES INV-PERSON.
               15  INV-FONDS             PIC 9(003).
               15  INV-FONDS-KZ          PIC X.
               15  INV-PRODSCHL          PIC 9(002).
               15  INV-PRODSCHL-KZ       PIC X.
               15  INV-V-BEG             PIC 9(008).
               15  INV-V-BEG-R REDEFINES INV-V-BEG.
                   20 INV-V-BEG-JH       PIC 9(002).
                   20 INV-V-BEG-JJ       PIC 9(002).
                   20 INV-V-BEG-MM       PIC 9(002).
                   20 INV-V-BEG-TT       PIC 9(002).
               15  INV-V-BEG-KZ          PIC X.
               15  INV-V-ENDE            PIC 9(008).
               15  INV-V-ENDE-R REDEFINES INV-V-ENDE.
                   20 INV-V-ENDE-JH      PIC 9(002).
                   20 INV-V-ENDE-JJ      PIC 9(002).
                   20 INV-V-ENDE-MM      PIC 9(002).
                   20 INV-V-ENDE-TT      PIC 9(002).
               15  INV-V-ENDE-KZ         PIC X.
               15  INV-KUSCHU-ABL        PIC 9(008).
               15  INV-KUSCHU-ABL-R REDEFINES INV-KUSCHU-ABL.
                   20 INV-KUSCHU-ABL-JH  PIC 9(002).
                   20 INV-KUSCHU-ABL-JJ  PIC 9(002).
                   20 INV-KUSCHU-ABL-MM  PIC 9(002).
                   20 INV-KUSCHU-ABL-TT  PIC 9(002).
               15  INV-KUSCHU-ABL-KZ     PIC X.
170699         15  INV-WAEHRUNG          PIC X(3).
170699         15  INV-WAEHRUNG-KZ       PIC X.
               15  INV-ISTSUMME          PIC S9(9)V9(2).
               15  INV-ISTSUMME-KZ       PIC X.
               15  INV-URSPRUNGSSUM      PIC 9(9)V9(3).
               15  INV-URSPRUNGSSUM-KZ   PIC X.
               15  INV-RATE              PIC 9(9)V9(2).
               15  INV-RATE-KZ           PIC X.
               15  INV-MODUS             PIC X(001).
               15  INV-MODUS-KZ          PIC X.
               15  INV-HAUSTARIF         PIC X(001).
               15  INV-HAUSTARIF-KZ      PIC X.
               15  INV-PROVKZ            PIC X(001).
               15  INV-PROVKZ-KZ         PIC X.
               15  INV-GES-2             PIC 9(002).
               15  INV-GES-2-KZ          PIC X.
               15  INV-ORGGS-2           PIC 9(003).
               15  INV-ORGGS-2-KZ        PIC X.
               15  INV-VERM-2            PIC 9(010).
               15  INV-VERM-2-KZ         PIC X.
               15  INV-PIF-2             PIC X(020).
               15  INV-PIF-2-KZ          PIC X.
               15  INV-LOE-KZ            PIC X(003).
               15  INV-LOE-KZ-KZ         PIC X.

      * INFO-SPERRE
      * Wird vom Ermittlungs-PGM dann erkannt, wenn in dem DTA Name und
      * Vorname von Person-1 nicht gefüllt sind ( = SPACE )
      * In dieser Schnittstelle zur Aufbereitung hin bedeutet:
      *  INF-INFO-SP = 'J' Info-Sperre besteht bzw. wird eingeschaltet
      *  INF-INFO-SP = 'N' Info-Sperre besteht nicht bzw. wird ausgesch.
      *  Vereinbarung zur Übergabe von Gruppen:
      *  Info-Sperre wird eingeschaltet oder ausgeschaltet:
      *      Die komplette Gruppe, also der Personen-Satz und alle
      *      Vertragssätze werden übergeben
      *  Info-Sperre besteht:
      *      Nur die Änderuns-Sätze werden übergeben
               15  INV-INFO-SP           PIC X(001).
               15  INV-INFO-SP-KZ        PIC X.

170699         15  FILLER-C2             PIC X(409).

         05  INV-ASATZ REDEFINES INV-CSATZ.
           10  INV-ABSENDER              PIC X(027).
           10  INV-ERST-DAT              PIC 9(006).
           10  INV-DATEISTAND            PIC 9(006).
           10  INV-EMPFAENGER            PIC X(027).
086        10  INV-INHALT                PIC X(020).
           10  FILLER-A1                 PIC X(465).

         05  INV-ESATZ REDEFINES INV-CSATZ.
           10  INV-SATZANZAHL            PIC 9(007).
           10  FILLER-E1                 PIC X(544).
























