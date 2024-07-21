      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0853.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRKI0853.CPv  $
      *
      *   Rev 2.10   Mar 30 2020 08:17:34   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.9   Dec 09 2019 14:00:24   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050
      *
      *   Rev 2.8   Jun 08 2017 09:10:00   t13ex78
      *AKI01513: Programme & Copies
      *
      *   Rev 2.7   Sep 24 2013 13:33:16   t13ex19
      *AKI01204: SEPA - DB-Online - AdvoCard
      *
      *   Rev 2.6   Jul 21 2009 13:59:12   t13ex14
      *AKI00817: Ausweis von Zusatzinfos: Ermittlung, Aufbereitung,
      * Aktualisierung
      *
      *   Rev 2.5   Apr 09 2009 13:51:16   t13ex14
      *AKI00794: Advocard Erweiterung der Schnittstellen / "Neue Ge
      *nerali"
      *
      *   Rev 2.4   Jan 31 2002 08:01:04   t13ex49
      *AKI00265 - Dezimalstellen
      *
      *   Rev 2.3   Dec 27 1999 15:44:30   T13EX49
      *AKI00061 - Währungskennzeichen und
      *8-stelliges Datum
      *
      *   Rev 2.2   May 25 1999 15:42:22   T13EX19
      *AKI00025 - neu Erstellung
      *
      *   Rev 2.1   May 12 1999 16:43:34   T13EX19
      *aki00001 - neu Erstellung
      *
      *   Rev 2.0   Apr 29 1999 13:56:56   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****  VERSION   1    22.07.93 10:26:15
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE ADVOCARD                     *
      *    ---------------------------------------                     *
      *                                                                *
      *    LAENGE = VARIABEL         ERSTELLT AM 24.09.90.             *
      *       MIN =  632 BYTE                    KUHN                  *
090517*       MAX = 2754 BYTE                                          *
      *                                                                *
      *    NEUE FELDER :             GEAENDERT AM 02.04.91.            *
      *    ROL-VNR, AUSL, ANGEST-KZ              KUHN                  *
      *                                                                *
      *    GEÄNDERT 25.05.93 / BORK                                    *
      *    DIE PLZ WIRD KÜNFTIG 7-STELLIG ÜBERGEBEN(VORHER X(4))       *
      *    NEUE FELDER: LÄNDER-KZ, BLZ, KTONR, PLZ-ALT                 *
      *                                                                *
      *    Geändert von Zgur-Vujic                                     *
      *    am 11.05.99 Erstelldatum 9(8)                               *
      *                Absendedatum 9(8)                               *
      *                Waerung-KZ   x(3)                               *
      *                                                                *
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00061!15.11.99!    ! Datumswerte mit Jahr-     !Peter Pohl*
      *         !        !    ! hundert sowie Währungs-   !          *
      *         !        !    ! kennzeichen auf Vertrags- !          *
      *         !        !    ! ebene                     !          *
      *         !        !    !                           !          *
220102* AKI00265!22.01.02!    ! DTA-Änderung              !Peter Pohl*
      *         !        !    !                           !          *
200209* AKI00794!20.02.09!    ! DTA-Änderung              !W. Misgeld*
      *         !        !    !                           !          *
150409* AKI00817!15.04.09!    ! neue Felder f. Nachlässe, !W. Misgeld*
150409*         !        !    ! Bonus u. Schadenfreiheit  !          *
260613* AKI01204!26.06.13!    ! neue Felder f. SEPA und   !B. Stoldt *
260613*         !        !    ! Sofort-Rabatt             !          *
090517* AKI01513!09.05.17!    ! DTA Erweiterung um Kenn-  !tfl       *
      *         !        !    ! zeichen Maklerabgang und  !          *
      *         !        !    ! Einbau von Fillern        !          *
      *         !        !    !                           !          *
      * AKI01734!08.11.19!    ! Risikobruttobeitrag       !P. Hübner *
      *         !        !    ! aktivieren                !          *
      *AKI01766!12.02.20!     ! neue Felder Mitrarbeiter  !P. Hübner *
      *        !        !     ! Rabatt und ID sowie       !          *
      *        !        !     ! Nettobeiträge             !          *
      *DBO-8432!20.04.21!     ! neue Felder               !P. Hübner *  
      *--------!--------!-----!---------------------------!----------*
CCC009*   SKIP1
       01  DRKI0853.
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
               20  ADV-RISZAEHL        PIC S9(3)       COMP-3.
090517         20  ADV-KORRMAKLER      PIC S9(1)       COMP-3.
               20  ADV-MITARB-RAB      PIC S9(2)       COMP-3.
               20  ADV-MITARB-ID       PIC X(12).
               20  ADV-VORNAME         PIC X(30).
               20  ADV-PRAEMIE-NETTO   PIC S9(7)V99    COMP-3.
               20  ADV-ANTRAGS-ART     PIC S9(3) COMP-3.
               20  ADV-ANNAHME-DAT     PIC S9(8) COMP-3.
               20  ADV-UMST-FAEHIGKEIT PIC X.    

               20  FILLER              PIC X(88).
               
      *
               20  ADV-VARIABEL OCCURS 10 DEPENDING ON ADV-RISZAEHL.
                   30 ADV-LFDNR        PIC S9(3)       COMP-3.
151199             30 ADV-RISBEG       PIC S9(8)       COMP-3.
151199             30 ADV-RISAEND      PIC S9(8)       COMP-3.
                   30 ADV-WAG          PIC X(4).
                   30 ADV-TARIFGR      PIC XXX.
                   30 ADV-VART         PIC X(3).
                   30 ADV-VTEXT        PIC X(25).
                   30 ADV-STCK         PIC S9(5)       COMP-3.
                   30 ADV-RISBTR       PIC S9(7)V99    COMP-3.
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
       01  ADV-ASATZ.
           10  ADV-SA-A                PIC X.
           10  ADV-ABS                 PIC X(27).
           10  ADV-ERSTDAT.
               20  ADV-ERST-TT         PIC 9(2).
               20  ADV-ERST-MM         PIC 9(2).
.              20  ADV-ERST-JJJJ       PIC 9(4).
.          10  ADV-ABSDAT              PIC 9(8).
           10  ADV-EMPF                PIC X(27).
           10  ADV-INHALT              PIC X(20).
110599     10  ADV-WAE-KZ              PIC X(3).

       01  ADV-ESATZ.
           10  ADV-SA-E                PIC X.
           10  ADV-SATZANZ             PIC S9(7)      COMP-3.
220102     10  ADV-SATZSUM-E OCCURS 5.
220102         20 ADV-UEBERLAUF        PIC X(1).
220102         20 ADV-SATZSUM          PIC S9(15)V99  COMP-3.




