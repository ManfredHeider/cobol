      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1123.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRKI1123.CPv  $
      *
      *   Rev 2.10   Mar 30 2020 08:20:20   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.9   Jun 08 2017 09:10:06   t13ex78
      *AKI01513: Programme & Copies
      *
      *   Rev 2.8   Jun 16 2016 09:21:32   t13ex78
      *AKI01416: Programmanpassungen und Neuerstellungen
      *
      *   Rev 2.7   Jun 09 2016 15:09:52   t13ex78
      *AKI01416: Programmanpassungen und Neuerstellungen
      *
      *   Rev 2.6   Sep 24 2013 13:33:10   t13ex19
      *AKI01204: SEPA - DB-Online - AdvoCard
      *
      *   Rev 2.5   Nov 11 2011 14:37:30   t13ex14
      *AKI01079: Anpassung Aufbereitung u. Aktualisierung
      *
      *   Rev 2.4   Jul 21 2009 13:59:18   t13ex14
      *AKI00817: Ausweis von Zusatzinfos: Ermittlung, Aufbereitung,
      * Aktualisierung
      *
      *   Rev 2.3   Apr 09 2009 13:51:22   t13ex14
      *AKI00794: Advocard Erweiterung der Schnittstellen / "Neue Ge
      *nerali"
      *
      *   Rev 2.2   Jan 31 2002 08:01:44   t13ex49
      *AKI00265 Anpassung Dezimalstellen
      *
      *   Rev 2.1   30 Oct 2000 17:32:00   T13EX49
      *AKI00147 - erstmalige Prod.-Übergabe
      *
      *   Rev 2.0   07 Jun 2000 17:08:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    Komplettbestand AdvoCard als KSDS-Datei                    *
      *                                                               *
      *                                                               *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMMEN                      *
      *                                                               *
      *                                                               *
090517*    SATZLÄNGE 2659                                             *
      *    SATZLÄNGE 2759                                             *
      *                                                               *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *                                                               *
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI00147!11.09.00! Neuerstellung                    !P. Pohl   *
      *        !        !                                  !          *
220102*AKI00265!22.01.02! DTA-Änderung dadurch ändert sich !Peter Pohl*
      *        !        ! auch die KSDS-Datei              !          *
200209*AKI00794!20.02.09! DTA-Änderung dadurch ändert sich !W. Misgeld*
      *        !        ! auch die KSDS-Datei              !          *
150409* --------------------------------------------------------------*
150409*AKI00817!15.04.09!    ! neue Felder f. Nachlässe,   !W. Misgeld*
150409*        !        !    ! Bonus u. Schadenfreiheit    !          *
      *--------!-------------------------------------------!----------*
181011*AKI01079!18.10.11! KB-WAG redefiniert für Migration !W. Misgeld*
      *--------!-------------------------------------------!----------*
260613*AKI01204!26.06.13! neue Felder wg. SEPA,            !B. Stoldt *
260613*        !        ! Sofort-Rabatt                    !          *
      *--------!-------------------------------------------!----------*
      *--------!-------------------------------------------!----------*
210116*AKI01416!21.01.16! neue Felder wg. CoC Sperrdatum   !tfl       *
      *--------!-------------------------------------------!----------*
090517*AKI01513!09.05.17! DTA Erweiterung um Kennzeichen   !tfl       *
      *        !        ! Maklerabgang und Einbau von neuen!          *
      *        !        ! Fillern                          !          *
      *--------!--------!----------------------------------!          *
      *AKI01766!12.02.20! DTA Erweiterung um Mitarb Rabatt !HHU       *
      *        !        ! Mitarb ID und Nettobeiträge      !          *
      *--------!        !                                  !          *
      *DBO-8432!20.04.21! neue Felder                      !P. Hübner * 
      *--------!        !                                  !          *
       01  DRKI1123.

           05 KB-VERTRAG-KEY.
              10 KB-VNR                   PIC 9(015).

           05 KB-ORGANR1.
              10 KB-GES                   PIC XX.
              10 KB-FD                    PIC X(3).
              10 KB-BD                    PIC X(3).
              10 KB-VBNR                  PIC X(8).
           05 KB-PNR                      PIC X(15).
           05 KB-SPARTE                   PIC X.
           05 KB-BAKKZ                    PIC X.
           05 KB-VBEG                     PIC S9(8)       COMP-3.
           05 KB-VABL                     PIC S9(8)       COMP-3.
           05 KB-AEND                     PIC S9(8)       COMP-3.
           05 KB-RUHVON                   PIC S9(8)       COMP-3.
           05 KB-RUHBIS                   PIC S9(8)       COMP-3.
           05 KB-STORNODAT                PIC S9(8)       COMP-3.
           05 KB-STORNOGR                 PIC XX.
           05 KB-ZAHLW                    PIC X.
           05 KB-FAEL.
              10 KB-FAELJ                 PIC XX.
              10 KB-FAELM                 PIC XX.
           05 KB-LAST                     PIC X.
           05 KB-AVB                      PIC XX.
           05 KB-TARIF                    PIC XXX.
           05 KB-BVTR                     PIC X.
           05 KB-VSU                      PIC S9(7)       COMP-3.
           05 KB-KLAUS                    PIC XX     OCCURS 6.
           05 KB-ANREDE                   PIC X.
           05 KB-TITEL                    PIC X(30).
           05 KB-NAME                     PIC X(30).
           05 KB-PLZ                      PIC X(7).
           05 KB-ORT                      PIC X(25).
           05 KB-STR                      PIC X(30).
           05 KB-ZUPOST                   PIC X(30).
           05 KB-GEBD                     PIC S9(8)       COMP-3.
220102     05 KB-PRAEMIE                  PIC S9(7)V99    COMP-3.
           05 KB-SCHADENTAB                          OCCURS 3.
              10 KB-SANZ                  PIC S9(5)       COMP-3.
              10 KB-SDM                   PIC S9(9)       COMP-3.
           05 KB-AKTKZ                    PIC X(4).
           05 KB-KZ.
              10 KB-CARD                  PIC X.
              10 KB-DATBIS                PIC X(6).
           05 KB-RING.
              10 KB-VORVERS               PIC S9(11)      COMP-3.
              10 KB-WVERTR1               PIC S9(11)      COMP-3.
              10 KB-WVERTR2               PIC S9(11)      COMP-3.
           05 KB-AUSL                     PIC X(4).
           05 KB-ANGEST-KZ                PIC XX.
           05 KB-ROL-VNR                  PIC X(11).
           05 KB-LKZ                      PIC X(03).
           05 KB-BLZ                      PIC X(08).
           05 KB-KTONR                    PIC X(10).
           05 KB-PLZ-ALT                  PIC X(04).
           05 KB-GST2                     PIC X(03).
200209     05 KB-VERMITTLER2              PIC X(06).
           05 KB-ANTEIL-PROV              PIC X(01).
           05 KB-AENDERUNGS-KZ            PIC X(02).
           05 KB-BETR-GESCHAEFTSSTELLE    PIC X(04).
200209     05 KB-BETR-BEZIRK              PIC X(06).
210116     05 FILLER                      PIC X(10).
210116     05 KB-COC-SPERRDATUM           PIC S9(08)      COMP-3.
210116     05 KB-COC-SPERRAKTION          PIC X(01).
150409     05 KB-KUNDENBONUS              PIC X(01).
150409     05 KB-DAUERNACHLASS            PIC X(01).
150409     05 KB-VSPNACHLASS              PIC X(01).
150409     05 KB-OEFFDIENSTNL             PIC X(01).
           05 KB-WKZ                      PIC X(03).
      *
260613     05 KB-SOFORTRABATT             PIC X(02).
..         05 KB-MAND-REF                 PIC X(18).
..         05 KB-MAND-STATUS              PIC X(01).
..         05 KB-BIC                      PIC X(11).
..         05 KB-IBAN                     PIC X(34).
..         05 KB-ANREDE-BZ                PIC X(01).
..         05 KB-NACHNAME-BZ              PIC X(30).
..         05 KB-VORNAME-BZ               PIC X(30).
..         05 KB-PLZ-BZ                   PIC X(07).
..         05 KB-ORT-BZ                   PIC X(30).
..         05 KB-STR-BZ                   PIC X(30).
260613     05 KB-GEBD-BZ                  PIC S9(08)      COMP-3.
      *
           05 KB-RISZAEHL                 PIC S9(3)       COMP-3.
090517     05 KB-KORRMAKLER               PIC X(1).
           05 KB-MITARB-RAB               PIC S9(2) COMP-3.
           05 KB-MITARB-ID                PIC X(12).
           05 KB-VORNAME                  PIC X(30).
           05 KB-PRAEMIE-NETTO            PIC S9(7)V99    COMP-3.
           05 KB-ANTRAGS-ART              PIC 99.   
           05 KB-ANNAHME-DAT              PIC 9(8). 
           05 KB-UMST-FAEHIGKEIT          PIC X.    
           05 FILLER                      PIC X(85).
           
      *
           05 KB-VARIABEL OCCURS 10.
              10 KB-LFDNR                 PIC S9(3)       COMP-3.
              10 KB-RISBEG                PIC S9(8)       COMP-3.
              10 KB-RISAEND               PIC S9(8)       COMP-3.
              10 KB-WAG                   PIC X(4).
181011        10 KB-WAG-NUM REDEFINES KB-WAG PIC 9(4).
              10 KB-TARIFGR               PIC XXX.
              10 KB-VART                  PIC X(3).
              10 KB-VTEXT                 PIC X(25).
              10 KB-STCK                  PIC S9(5)       COMP-3.
220102        10 KB-JBTR                  PIC S9(7)V99    COMP-3.
              10 KB-RISIKO.
                 15 KB-RISTXT1            PIC X(25).
                 15 KB-RISTXT2            PIC X(20).
150409        10 KB-SELBSTBETEIL          PIC S9(7).
150409        10 KB-SELBSTBETEIL-X REDEFINES KB-SELBSTBETEIL
                                          PIC  X(7).
150409        10 KB-SFRSTUFE              PIC  X(1).
150409        10 KB-SFRVARIANTE           PIC  X(1).
              10 KB-JBTR-NETTO            PIC S9(7)V99    COMP-3.
              10 FILLER                   PIC X(86).
           05 KB-STATUS                   PIC X(1).
      *       D --> Gelöscht - sonst SPACE






