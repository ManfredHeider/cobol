      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0631.CPY  $
      *    $Revision:   2.30  $
      *
      *    $Log:   L:/mf/CPY/DRKI0631.CPv  $
      *
      *   Rev 2.30   Mar 30 2020 08:18:56   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.29   Dec 09 2019 14:01:10   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050
      *
      *   Rev 2.28   Feb 24 2017 14:22:18   t13ex78
      *AKI01497: G81 -Antrags-PIN weitergeben
      *
      *
      *   Rev 2.12   Jan 11 2016 15:01:02   t13ex78
      *AKI01404: AdvoCard - Kombirabatt und Kundenbonus trennen - Ä
      *nderungen an PMKI's
      *
      *   Rev 2.11   Sep 24 2013 13:33:20   t13ex19
      *AKI01204: SEPA - DB-Online - AdvoCard
      *
      *   Rev 2.10   Nov 11 2011 14:37:38   t13ex14
      *AKI01079: Anpassung Aufbereitung u. Aktualisierung
      *
      *   Rev 2.9   Jul 21 2009 13:58:56   t13ex14
      *AKI00817: Ausweis von Zusatzinfos: Ermittlung, Aufbereitung,
      * Aktualisierung
      *
      *   Rev 2.8   Feb 26 2009 09:18:26   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.7   Feb 23 2009 08:07:30   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.6   Feb 23 2009 07:46:04   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.5   Feb 13 2006 10:16:36   t13ae31
      *FKI01010: DMB /Advocard Feld Kontonr zu klein
      *
      *   Rev 2.4   May 12 2005 08:54:44   t13ae31
      *AKI00465: Vertragsdaten Advocard: Berücksichtigung Selbstbet
      *eiligung.
      *
      *   Rev 2.3   16 Oct 2001 14:08:58   t13ex49
      *FKI00503 - Risikotabelle
      *
      *   Rev 2.2   Dec 27 1999 15:43:02   T13EX49
      *AKI00061 - Währungskennzeichen und
      *8-stelliges Datum
      *
      *   Rev 2.1   May 18 1999 14:12:52   T13EX19
      *AKI00025 - neu erstellung
      *
      *   Rev 2.0   Mar 24 1999 18:23:28   T13EX19
      *
      *
      *   Rev 1.2   Mar 18 1999 13:31:00   T13EX19
      *
      *
      *   Rev 1.1   Mar 17 1999 21:07:20   T13EX19
      *ae-vart eingefügt
      *
      *   Rev 1.0   25 Aug 1998 09:25:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                    RECHTSCHUTZ: ADVOCARD, DMB, ROLAND
      *
      *                                                                *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMMEN                       *
      *        - BESTANDSAENDERUNGSDATEN
      *                                                                *
      *    SATZLÄNGE 480                                               *
270801*    SATZLÄNGE 646
280205*    SATZLÄNGE 660
190109*    SATZLÄNGE 880
150409*    SATZLÄNGE 970
260613*    SATZLÄNGE 1184
      *    SATZLÄnge 1300
      *    SATZLÄNGE 1400                                             *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *21.09.98!NEUERSTELLUNG                           !Zgur-Vujic 01 *
      *AKI00001!                                        !         !    *
      *        !                                        !         !    *
151199*AKI00061!Änderung für Währungskennzeichen        !P. Pohl  ! 02 *
151199*        !erweitert um 4 Byte                     !         !    *
      *        !                                        !         !    *
270801*FKI00503!Anpassung RISIKO-Tabellen an DTA        !P.Pohl   !    *
270801*        !und KZ für Änderung eingefügt           !         !    *
      *        !                                        !         !    *
280205*AKI00465!Selbstbeteilung (Tarif)+ Wagnisüberarb. !P.Hübner !    *
      *        !                                        !         !    *
130206*FKI01010!Kontonr von 9(10)auf 9(15)              !P.Hübner !    *
      *        !                                        !         !    *
190109*AKI00642!AE-RISIKO-TEXT u. RISTXT2 aufgenommen   !W.Misgeld!    *
150409* ---------------------------------------------------------------*
150409*AKI00817!15.04.09! 2.9! neue Felder f. Nachlässe,!W. Misgeld    *
150409*        !        !    ! Bonus u. Schadenfreiheit !              *
      *--------!----------------------------------------!---------!----*
181011*AKI01079!18.10.11! AE-WAG numerisch definiert    !W. Misgeld *
      *--------!----------------------------------------!--------------*
260613*AKI01204!26.06.13! neue Felder wg. SEPA und      !B. Stoldt     *
260613*        !        ! Sofort-Rabatt                 !              *
      *--------!----------------------------------------!--------------*
161215*AKI01404!16.12.15!    ! KombiRabatt (aus Kunden- !tfl           *
161215*        !        !    ! bonus abgeleitet bis die !              *
161215*        !        !    ! AdvoCard den DTA erw.    !              *
      *--------!----------------------------------------!--------------*
210217*AKI01497!21.02.17!    ! Antrags-PIN weitergeben  !tfl           *
      *--------!----------------------------------------!--------------*
      *AKI01734!08.11.19!    ! Risikobruttobeitrag      !P. Hübner     *
      *        !        !    ! aktivieren               !              *
      *--------!----------------------------------------!--------------*
      *AKI01766!17.02.20!    ! neue Felder Mitrarbeiter !P. Hübner     *
      *        !        !    ! Rabatt und ID sowie      !              *
      *        !        !    ! Nettobeiträge            !              *
      *--------!--------!----!--------------------------!--------------*
      *DBO-8432!20.04.21!    ! neue Felder              !P. Hübner     *
      *        !        !    !                          !              *
      *        !        !    !                          !              *
      *--------!--------!----!--------------------------!--------------*
       01  DRKI0631.

      *  Vertragsnummer
           05  AE-VNR                       PIC 9(11).

      *  Geselschaft
           05  AE-ges                       PIC 9(02).

      * Verarbeitungs KZ
      *   I=Insert
      *   U=Update
      *   D=Delete
           05  AE-VERARB-KZ                 PIC X.

      * VB Nummer
           05  AE-VBNR                      PIC X(8).
      * 'J' - VB-Änderung
           05  AE-VBNR-KZ                   PIC X.

      * Vertragbeginn
           05  AE-VBEG                      PIC 9(8).
           05  AE-VBEG-KZ                   PIC X.

      * Vertragablaufdatum
           05  AE-VABL                      PIC 9(8).
           05  AE-VABL-KZ                   PIC X.

      * Aenderungsdatum
           05  AE-AEND                      PIC 9(8).
           05  AE-AEND-KZ                   PIC X.

      * Stornodatum
           05  AE-STORNODAT                 PIC 9(8).
           05  AE-STORNODAT-KZ              PIC X.

      * Zahlungsweise
           05  AE-ZAHLW                     PIC 9.
           05  AE-ZAHLW-KZ                  PIC X.

      * Lastschriftzahler - zahlart
           05  AE-zahlart                   PIC 9.
           05  AE-zahlart-KZ                PIC X.

      * Versicherungssumme
           05  AE-VSU                       PIC 9(9)V99.
           05  AE-VSU-KZ                    PIC X.

      * Anrede
           05  AE-ANREDE                    PIC 9.
           05  AE-ANREDE-KZ                 PIC X.

      * Name
           05  AE-NACHNAME                  PIC X(30).
           05  AE-NACHNAME-ALT              PIC X(30).
           05  AE-NACHNAME-KZ               PIC X.
           05  AE-VORNAME                   PIC X(15).
           05  AE-VORNAME-ALT               PIC X(15).
           05  AE-VORNAME-KZ                PIC X.
           05  AE-TITEL                     PIC X(15).
           05  AE-TITEL-ALT                 PIC X(15).

      * PLZ
           05  AE-PLZ                       PIC X(5).
           05  AE-PLZ-KZ                    PIC X.

      * Ort
           05  AE-ORT                       PIC X(25).
           05  AE-ORT-KZ                    PIC X.

      * Strasse
           05  AE-STR                       PIC X(30).
           05  AE-STR-KZ                    PIC X.

      * Geburtsdatum
           05  AE-GEBD                      PIC 9(8).
           05  AE-GEBD-KZ                   PIC X.

      * Praemnie
           05  AE-PRAEMIE                   PIC 9(9)V99.
           05  AE-PRAEMIE-KZ                PIC X.

      * Ausland KZ
           05  AE-AUSL                      PIC X(4).
           05  AE-AUSL-KZ                   PIC X.

      * Bankleitzahl
           05  AE-blz                       PIC 9(08).
           05  AE-blz-KZ                    PIC X.

      * Kontonummer
           05  AE-ktonr                     PIC 9(15).
           05  AE-ktonr-KZ                  PIC X.

      * Monat der hauptfälligkeit
           05  AE-faelm                     PIC 9(2).
           05  AE-faelm-KZ                  PIC X.

150409* Vertragsbedingung
150409     05  AE-AVB                       PIC 9(02).
150409     05  AE-AVB-KZ                    PIC X(01).

151199* Währungskennzeichen
151199     05  AE-WKZ                       PIC X(003).
151199     05  AE-WKZ-KZ                    PIC X.

150409* Stornogrund
150409     05  AE-STORNOGRUND               PIC X(2).
150409     05  AE-STORNOGRUND-KZ            PIC X.
150409
150409* Bonus u. Nachlässe
150409     05  AE-KDBONUS                    PIC X(01).
150409     05  AE-KDBONUS-KZ                 PIC X(01).
150409     05  AE-DAUERNL                    PIC X(01).
150409     05  AE-DAUERNL-KZ                 PIC X(01).
150409     05  AE-VSPNACH                    PIC X(01).
150409     05  AE-VSPNACH-KZ                 PIC X(01).
150409     05  AE-OEDIENL                    PIC X(01).
150409     05  AE-OEDIENL-KZ                 PIC X(01).
260613     05  AE-SOFORTRAB                  PIC X(02).
260613     05  AE-SOFORTRAB-KZ               PIC X(01).
161215     05  AE-KOMBIRABATT                PIC S9(04) COMP.
161215     05  AE-KOMBIRABATT-KZ             PIC X(01).

260613* SEPA - abw. Beitragszahler
..         05  AE-MAND-REF                   PIC X(18).
..         05  AE-MAND-REF-KZ                PIC X(01).
..         05  AE-MAND-STATUS                PIC X(01).
..         05  AE-MAND-STATUS-KZ             PIC X(01).
..         05  AE-BIC                        PIC X(11).
..         05  AE-BIC-KZ                     PIC X(01).
..         05  AE-IBAN                       PIC X(34).
..         05  AE-IBAN-KZ                    PIC X(01).
..         05  AE-ANREDE-BZ                  PIC X(01).
..         05  AE-ANREDE-BZ-KZ               PIC X(01).
..         05  AE-NACHNAME-BZ                PIC X(30).
..         05  AE-NACHNAME-BZ-KZ             PIC X(01).
..         05  AE-VORNAME-BZ                 PIC X(30).
..         05  AE-VORNAME-BZ-KZ              PIC X(01).
..         05  AE-PLZ-BZ                     PIC X(07).
..         05  AE-PLZ-BZ-KZ                  PIC X(01).
..         05  AE-ORT-BZ                     PIC X(30).
..         05  AE-ORT-BZ-KZ                  PIC X(01).
..         05  AE-STR-BZ                     PIC X(30).
..         05  AE-STR-BZ-KZ                  PIC X(01).
260613     05  AE-GEBD-BZ                    PIC 9(08).
260613     05  AE-GEBD-BZ-KZ                 PIC X(01).
210217     05  AE-PNR                        PIC 9(09) COMP.
210217     05  AE-PNR-KZ                     PIC X(01).

      *
270801     05  AE-WAG-INSERT        OCCURS 10.
      *
181011*         10 AE-WAG-INS              PIC X(4).
181011          10 AE-WAG-INS              PIC 9(4).
                10 ae-vart-ins  redefines ae-wag-ins pic 9(7) comp-3.
                10 AE-RISBTR               PIC 9(7)V99 COMP-3.
190109          10 AE-RISIKO.
190109             15 AE-RISTXT1           PIC X(25).
190109             15 AE-RISTXT2           PIC X(20).

150409          10 AE-SELBSTBETEIL         PIC S9(7).
150409          10 AE-SFRSTUFE             PIC  X(1).
150409          10 AE-SFRVARIANTE          PIC  X(1).
                10 AE-RISBTR-NETTO         PIC 9(7)V99    COMP-3.
      *
270801*    05  AE-WAG-DELETE OCCURS 5.
270801     05  AE-WAG-DELETE OCCURS 10.
      *

181011*         10 AE-WAG-DEL              PIC X(4).
181011          10 AE-WAG-DEL              PIC 9(4).
                10 ae-vart-del redefines ae-wag-del pic 9(7) comp-3.

270801     05  AE-WAG-KZ                    PIC X.
           05  AE-RIS-KZ                    PIC X.

280205*** Tarif
280205     05  AE-TARIF                     PIC XXX.
280205     05  AE-TARIF-KZ                  PIC X.
           05  AE-MITARB-RAB                PIC S9(2) COMP-3.
           05  AE-MITARB-RAB-KZ             PIC X.
           05  AE-MITARB-ID                 PIC X(12).
           05  AE-MITARB-ID-KZ              PIC X.
           05  AE-PRAEMIE-NETTO             PIC S9(7)V99    COMP-3.
           05  AE-PRAEMIE-NETTO-KZ          PIC X.
           05  AE-ANTRAGS-ART               PIC XX.   
           05  AE-ANTRAGS-ART-KZ            PIC X.    
           05 AE-ANNAHME-DAT                PIC 9(8). 
           05 AE-ANNAHME-DAT-KZ             PIC X.    
           05 AE-UMST-FAEHIGKEIT            PIC X.    
           05 AE-UMST-FAEHIGKEIT-KZ         PIC X.    
           05 FILLER                        PIC X(82).
           


