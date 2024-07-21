      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU077S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU077S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:53:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    22.07.93 10:26:15
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE ADVOCARD                     *
      *    ---------------------------------------                     *
      *                                                                *
      *    LAENGE = VARIABEL         ERSTELLT AM 24.09.90.             *
      *       MIN =  430 BYTE                    KUHN                  *
      *       MAX = 1600 BYTE                                          *
      *                                                                *
      *    NEUE FELDER :             GEAENDERT AM 02.04.91.            *
      *    ROL-VNR, AUSL, ANGEST-KZ              KUHN                  *
      *                                                                *
      *    GEÄNDERT 25.05.93 / BORK                                    *
      *    DIE PLZ WIRD KÜNFTIG 7-STELLIG ÜBERGEBEN(VORHER X(4))       *
      *    NEUE FELDER: LÄNDER-KZ, BLZ, KTONR, PLZ-ALT                 *
      *                                                                *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU077S.
           10  ADV-CSATZ.
               20  ADV-SA              PIC X.
               20  ADV-ORGANR1.
                   30  ADV-GES         PIC XX.
                   30  ADV-FD          PIC X(3).
                   30  ADV-BD          PIC X(3).
                   30  ADV-VBNR        PIC X(8).
               20  ADV-VNR             PIC S9(11)      COMP-3.
               20  ADV-PNR             PIC X(15).
               20  ADV-SPARTE          PIC X.
               20  ADV-BAKKZ           PIC X.
               20  ADV-VBEG            PIC S9(7)       COMP-3.
               20  ADV-VABL            PIC S9(7)       COMP-3.
               20  ADV-AEND            PIC S9(7)       COMP-3.
               20  ADV-RUHVON          PIC S9(7)       COMP-3.
               20  ADV-RUHBIS          PIC S9(7)       COMP-3.
               20  ADV-STORNODAT       PIC S9(7)       COMP-3.
               20  ADV-STORNOGR        PIC XX.
               20  ADV-ZAHLW           PIC X.
               20  ADV-FAEL.
                   30  ADV-FAELJ       PIC XX.
                   30  ADV-FAELM       PIC XX.
               20  ADV-LAST            PIC X.
               20  ADV-AVB             PIC XX.
               20  ADV-TARIF           PIC XX.
               20  ADV-BVTR            PIC X.
               20  ADV-VSU             PIC S9(7)       COMP-3.
               20  ADV-KLAUS           PIC XX            OCCURS 6.
               20  ADV-ANREDE          PIC X.
               20  ADV-TITEL           PIC X(30).
               20  ADV-NAME            PIC X(30).
250593         20  ADV-PLZ             PIC X(7).
               20  ADV-ORT             PIC X(25).
               20  ADV-STR             PIC X(30).
               20  ADV-ZUPOST          PIC X(30).
               20  ADV-GEBD            PIC S9(7)       COMP-3.
               20  ADV-PRAEMIE         PIC S9(8)V9     COMP-3.
               20  ADV-SCHADENTAB      OCCURS 3.
                   30 ADV-SANZ         PIC S9(5)       COMP-3.
                   30 ADV-SDM          PIC S9(9)       COMP-3.
               20  ADV-AKTKZ           PIC X(4).
               20  ADV-KZ.
                   30  ADV-CARD        PIC X.
                   30  ADV-DATBIS      PIC X(6).
               20  ADV-RING.
                   30  ADV-VORVERS     PIC S9(11)      COMP-3.
                   30  ADV-WVERTR1     PIC S9(11)      COMP-3.
                   30  ADV-WVERTR2     PIC S9(11)      COMP-3.
               20  ADV-AUSL            PIC X(4).
               20  ADV-ANGEST-KZ       PIC XX.
               20  ADV-ROL-VNR         PIC X(11).
250593         20  ADV-LKZ             PIC X(03).
250593         20  ADV-BLZ             PIC X(08).
250593         20  ADV-KTONR           PIC X(10).
250593         20  ADV-PLZ-ALT         PIC X(04).
250593         20  FILLER              PIC X(48).

               20  ADV-RISZAEHL        PIC S9(3)       COMP-3.
               20  ADV-VARIABEL OCCURS 10 DEPENDING ON ADV-RISZAEHL.
                   30 ADV-LFDNR        PIC S9(3)       COMP-3.
                   30 ADV-RISBEG       PIC S9(6)       COMP-3.
                   30 ADV-RISAEND      PIC S9(6)       COMP-3.
                   30 ADV-WAG          PIC X(4).
                   30 ADV-TARIFGR      PIC XX.
                   30 ADV-VART         PIC X(3).
                   30 ADV-VTEXT        PIC X(25).
                   30 ADV-STCK         PIC S9(5)       COMP-3.
                   30 ADV-JBTR         PIC S9(6)V9     COMP-3.
                   30 ADV-RISIKO.
                      40  ADV-RISTXT1  PIC X(25).
                      40  ADV-RISTXT2  PIC X(20).
                   30 FILLER           PIC X(24).

       01  ADV-ASATZ.
           10  FILLER                  PIC X.
           10  ADV-ABS                 PIC X(27).
           10  ADV-ERSTDAT.
               20  ADV-ERST-TT         PIC 9(2).
               20  ADV-ERST-MM         PIC 9(2).
               20  ADV-ERST-JJ         PIC 9(2).
           10  ADV-ABSDAT              PIC 9(6).
           10  ADV-EMPF                PIC X(27).
           10  ADV-INHALT              PIC X(20).

       01  ADV-ESATZ.
           10  FILLER                  PIC X.
           10  ADV-SATZANZ             PIC S9(7)      COMP-3.
           10  ADV-SATZSUM             PIC S9(17)     COMP-3  OCCURS 5.

