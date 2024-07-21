      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0674.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRKI0674.CPv  $
      *
      *   Rev 2.3   31 Jul 2000 15:44:04   T13EX49
      *FKI00386 - Datumsdefinition geändert
      *
      *   Rev 2.2   Dec 27 1999 15:43:28   T13EX49
      *AKI00061 - Währungskennzeichen und
      *8-stelliges Datum
      *
      *   Rev 2.1   May 18 1999 14:15:12   T13EX19
      *aki00025 - neu erstellung
      *
      *   Rev 2.0   May 17 1999 19:16:12   T13EX19
      *aki00001 - neu erstellung
      *
      *   Rev 1.0   25 Aug 1998 09:29:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                    ADVOCARD                                    *
      *                                                                *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM PMKI0187               *
      *        - KOMPLETTBESTAND
      *                                                                *
      *    SATZLÄNGE xxx                                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *23.09.98!NEUERSTELLUNG                           !Zgur-Vujic 01 *
      *AKI00001!                                        !         !    *
151199*AKI00061!Datumswerte mit Jahrhundert sowie       !P. Pohl  !    *
151199*        !Währungskennzeichen auf Vertragsebene   !         !    *
200700*FKI00386!Datum im Vorlaufsatz geändert           !P. Pohl  !    *
200700*        !                                        !         !    *
      *--------!----------------------------------------!---------!----*

       01  DRKI0674.
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
151199*        20  ADV-VBEG            PIC S9(7)       COMP-3.
151199         20  ADV-VBEG            PIC S9(8)       COMP-3.
151199*        20  ADV-VABL            PIC S9(7)       COMP-3.
151199         20  ADV-VABL            PIC S9(8)       COMP-3.
151199*        20  ADV-AEND            PIC S9(7)       COMP-3.
151199         20  ADV-AEND            PIC S9(8)       COMP-3.
151199*        20  ADV-RUHVON          PIC S9(7)       COMP-3.
151199         20  ADV-RUHVON          PIC S9(8)       COMP-3.
151199*        20  ADV-RUHBIS          PIC S9(7)       COMP-3.
151199         20  ADV-RUHBIS          PIC S9(8)       COMP-3.
151199*        20  ADV-STORNODAT       PIC S9(7)       COMP-3.
151199         20  ADV-STORNODAT       PIC S9(8)       COMP-3.
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
               20  ADV-PLZ             PIC X(7).
               20  ADV-ORT             PIC X(25).
               20  ADV-STR             PIC X(30).
               20  ADV-ZUPOST          PIC X(30).
151199*        20  ADV-GEBD            PIC S9(7)       COMP-3.
151199         20  ADV-GEBD            PIC S9(8)       COMP-3.
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
               20  ADV-LKZ             PIC X(03).
               20  ADV-BLZ             PIC X(08).
               20  ADV-KTONR           PIC X(10).
               20  ADV-PLZ-ALT         PIC X(04).
151199*        20  FILLER              PIC X(48).
151199         20  ADV-GST2                   PIC X(03).
151199         20  ADV-VERMITTLER2            PIC X(04).
151199         20  ADV-ANTEIL-PROV            PIC X(01).
151199         20  ADV-AENDERUNGS-KZ          PIC X(02).
151199         20  ADV-BETR-GESCHAEFTSSTELLE  PIC X(04).
151199         20  ADV-BETR-BEZIRK            PIC X(04).
151199         20  FILLER                     PIC X(20).
151199         20  ADV-WKZ                    PIC X(03).

               20  ADV-zaehler         PIC S9(3)       COMP-3.
               20  ADV-VARIABEL OCCURS 10 DEPENDING ON ADV-zaehler.
                   30 ADV-LFDNR        PIC S9(3)       COMP-3.
151199*            30 ADV-RISBEG       PIC S9(6)       COMP-3.
151199             30 ADV-RISBEG       PIC S9(8)       COMP-3.
151199*            30 ADV-RISAEND      PIC S9(6)       COMP-3.
151199             30 ADV-RISAEND      PIC S9(8)       COMP-3.
                   30 ADV-WAG          PIC X(4).
                   30 ADV-TARIFGR      PIC XX.
                   30 ADV-VART         PIC X(3).
                   30 ADV-VTEXT        PIC X(25).
                   30 ADV-STCK         PIC S9(5)       COMP-3.
                   30 ADV-JBTR         PIC S9(6)V9     COMP-3.
                   30 ADV-RISIKO.
                      40  ADV-RISTXT1  PIC X(25).
                      40  ADV-RISTXT2  PIC X(20).
151199*            30 FILLER           PIC X(24).
151199             30 FILLER           PIC X(22).

       01  ADV-KB-ASATZ.
           10  FILLER                  PIC X.
           10  ADV-ABS                 PIC X(27).
           10  ADV-ERSTDAT.
               20  ADV-ERST-TT         PIC 9(2).
               20  ADV-ERST-MM         PIC 9(2).
200700*        20  ADV-ERST-JJ         PIC 9(2).
200700         20  ADV-ERST-JJJJ       PIC 9(4).
200700*    10  ADV-ABSDAT              PIC 9(6).
200700     10  ADV-ABSDAT              PIC 9(8).
           10  ADV-EMPF                PIC X(27).
           10  ADV-INHALT              PIC X(20).
      * auf der Host das muss weg
      *    10  filler                  pic x(1517).

       01  ADV-KB-ESATZ.
           10  FILLER                  PIC X.
           10  ADV-SATZANZ             PIC S9(7)      COMP-3.
           10  ADV-SATZSUM             PIC S9(17)     COMP-3  OCCURS 5.
      * auf der Host das muss weg
      *    10  filler                  pic x(1551).


