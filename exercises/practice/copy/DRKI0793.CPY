      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0793.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI0793.CPv  $
      *
      *   Rev 2.7   Mar 30 2020 08:36:34   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.6   Jun 30 2017 10:01:58   t13ex78
      *AKI01513: Korr. PMKI0475 und DRKI0793
      *
      *   Rev 2.5   Sep 24 2013 13:33:34   t13ex19
      *AKI01204: SEPA - DB-Online - AdvoCard
      *
      *   Rev 2.4   Jul 21 2009 14:00:02   t13ex14
      *AKI00817: Ausweis von Zusatzinfos: Ermittlung, Aufbereitung,
      * Aktualisierung
      *
      *   Rev 2.3   Apr 17 2009 08:04:54   t13ex14
      *AKI00794: Advocard Erweiterung der Schnittstellen / "Neue Ge
      *nerali"
      *
      *   Rev 2.2   Dec 27 1999 15:44:10   T13EX49
      *AKI00061 - Währungskennzeichen und
      *8-stelliges Datum
      *
      *   Rev 2.1   May 18 1999 14:16:08   T13EX19
      *aki00025 - neu erstellung
      *
      *   Rev 2.0   May 17 1999 19:35:32   T13EX19
      *aki00001
      *
      *   Rev 1.0   06 Oct 1998 15:40:58   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                    ADVOCARD                                    *
      *                                                                *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM PMKI0475                *
      *        - abgewiesensätze
      *                                                                *
      *    SATZLÄNGE 2651                                              *
      *    SATZLÄNGE 2754                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *16.12.98!NEUERSTELLUNG                           !Zgur-Vujic 01 *
      *AKI00001!                                        !         !    *
151199*AKI00061!8-stellige Datumsfelder und Währungs-   !P. Pohl  !    *
      *        !kennzeichen                             !         !    *
      *        !                                        !         !    *
200209*AKI00794!20.02.09! DTA-Änderung                  !W.Misgeld!    *
150409* ---------------------------------------------------------------*
150409*AKI00817!15.04.09! 2.3! neue Felder f. Nachlässe,!W. Misgeld    *
150409*        !        !    ! Bonus u. Schadenfreiheit !              *
150409* ---------------------------------------------------------------*
030713*AKI01204!03.07.13! 2.5! neue Felder f. SEPA,     !B. Stoldt     *
030713*        !        !    ! Sofort-Rabatt            !              *
300617*AKI01513!30.06.17! 2.6! neuer DTA Aufbau         !tfl           *
      *--------!----------------------------------------!---------!----*
      *AKI01766!17.02.20!    ! neue Felder Mitrarbeiter !P. Hübner     *
      *        !        !    ! Rabatt und ID sowie      !              *
      *        !        !    ! Nettobeiträge            !              *
      *--------!--------!----!--------------------------!--------------*
CCC009*    SKIP1
       01  DRKI0793.
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
151199         20  ADV-VBEG            PIC S9(8)       COMP-3.
151199         20  ADV-VABL            PIC S9(8)       COMP-3.
151199         20  ADV-AEND            PIC S9(8)       COMP-3.
151199         20  ADV-RUHVON          PIC S9(8)       COMP-3.
151199         20  ADV-RUHBIS          PIC S9(8)       COMP-3.
151199         20  ADV-STORNODAT       PIC S9(8)       COMP-3.
               20  ADV-STORNOGR        PIC XX.
               20  ADV-ZAHLW           PIC X.
               20  ADV-FAEL.
                   30  ADV-FAELJ       PIC XX.
                   30  ADV-FAELM       PIC XX.
               20  ADV-LAST            PIC X.
               20  ADV-AVB             PIC XX.
               20  ADV-TARIF           PIC XXX.
               20  ADV-BVTR            PIC X.
               20  ADV-VSU             PIC S9(7)       COMP-3.
               20  ADV-KLAUS           PIC XX   OCCURS 6.
               20  ADV-ANREDE          PIC X.
               20  ADV-TITEL           PIC X(30).
               20  ADV-NAME            PIC X(30).
250593         20  ADV-PLZ             PIC X(7).
               20  ADV-ORT             PIC X(25).
               20  ADV-STR             PIC X(30).
               20  ADV-ZUPOST          PIC X(30).
151199         20  ADV-GEBD            PIC S9(8)       COMP-3.
220102         20  ADV-PRAEMIE         PIC S9(7)V99    COMP-3.
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
151199         20  ADV-GST2                     PIC X(03).
200209         20  ADV-VERMITTLER2              PIC X(06).
151199         20  ADV-ANTEIL-PROV              PIC X(01).
151199         20  ADV-AENDERUNGS-KZ            PIC X(02).
151199         20  ADV-BETR-GESCHAEFTSSTELLE    PIC X(04).
200209         20  ADV-BETR-BEZIRK              PIC X(06).
150409         20  FILLER                       PIC X(16).
150409         20  ADV-KUNDENBONUS              PIC X(01).
150409         20  ADV-DAUERNACHLASS            PIC X(01).
150409         20  ADV-VSPNACHLASS              PIC X(01).
150409         20  ADV-OEFFDIENSTNL             PIC X(01).
151199         20  ADV-WKZ                      PIC X(03).
260613         20  ADV-SOFORTRABATT             PIC X(02).
..             20  ADV-MAND-REF                 PIC X(18).
..             20  ADV-MAND-STATUS              PIC X(01).
..             20  ADV-BIC                      PIC X(11).
..             20  ADV-IBAN                     PIC X(34).
..             20  ADV-ANREDE-BZ                PIC X(01).
..             20  ADV-NACHNAME-BZ              PIC X(30).
..             20  ADV-VORNAME-BZ               PIC X(30).
..             20  ADV-PLZ-BZ                   PIC X(07).
..             20  ADV-ORT-BZ                   PIC X(30).
..             20  ADV-STR-BZ                   PIC X(30).
260613         20  ADV-GEBD-BZ                  PIC S9(08)  COMP-3.
      *
               20  ADV-RISIKOCOUNT     PIC S9(3)       COMP-3.
090517         20  ADV-KORRMAKLER      PIC S9(1)       COMP-3.
               20  ADV-MITARB-RAB      PIC S9(2)       COMP-3.
               20  ADV-MITARB-ID       PIC X(12).
               20  ADV-VORNAME         PIC X(30).
               20  ADV-PRAEMIE-NETTO   PIC S9(7)V99    COMP-3.
               20  FILLER              PIC X(96).
      *
               20  ADV-VARIABEL OCCURS 10 DEPENDING ON ADV-RISIKOCOUNT.
                   30 ADV-LFDNR        PIC S9(3)       COMP-3.
151199             30 ADV-RISBEG       PIC S9(8)       COMP-3.
151199             30 ADV-RISAEND      PIC S9(8)       COMP-3.
                   30 ADV-WAG          PIC X(4).
                   30 ADV-TARIFGR      PIC XXX.
                   30 ADV-VART         PIC X(3).
                   30 ADV-VTEXT        PIC X(25).
                   30 ADV-STCK         PIC S9(5)       COMP-3.
220102             30 FILLER           PIC X(05).
                   30 ADV-RISIKO.
                      40  ADV-RISTXT1  PIC X(25).
                      40  ADV-RISTXT2  PIC X(20).
150409             30 ADV-SELBSTBETEIL PIC S9(7).
150409             30 ADV-SELBSTBETEIL-X REDEFINES ADV-SELBSTBETEIL
                                       PIC  X(7).
150409             30 ADV-SFRSTUFE     PIC  X(1).
150409             30 ADV-SFRVARIANTE  PIC  X(1).
                   30 ADV-RISBTR-NETTO PIC S9(7)V99    COMP-3.
                   30 FILLER           PIC X(86).


