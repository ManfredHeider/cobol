      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2007
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2061.CPY  $
      *    $Revision:   2.26  $
      *
      *    $Log:   L:/mf/CPY/DRKI2061.CPv  $
      *
      *   Rev 2.26   Feb 05 2021 10:08:12   t13ae31
      *AKI01827: G10 SUH VSPKZ Programme
      *
      *   Rev 2.25   Jun 17 2020 10:22:42   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.24   Aug 10 2017 09:42:22   t13ae31
      *AKI01530: G10 SUH SA614 Beruf auf 48 St. Copies
      *
      *   Rev 2.23   Mar 01 2017 10:59:26   t13ae31
      *AKI01493: SUH neue Felder FLEXN Pgme
      *
      *   Rev 2.22   Sep 19 2016 07:30:48   t13ae31
      *AKI01458: SUH Namensvorwort bei VP Copies
      *
      *   Rev 2.21   Jun 23 2016 10:32:30   t13ae31
      *AKI01438: G10 SUH Angleichung DTA Pgme
      *
      *   Rev 2.20   May 05 2015 09:01:38   t13ae31
      *AKI01358: SUH Ermittlungsprogramm überarbeiten
      *
      *   Rev 2.19   Mar 19 2015 08:42:02   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 2.18   Mar 19 2015 08:03:32   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 2.17   Feb 23 2012 11:12:18   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.16   Feb 03 2012 15:01:16   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.15   Aug 05 2011 11:13:42   t13ae31
      *AKI01037: SUH - FK-And. 613 Erm+Aufb+Akt
      *
      *   Rev 2.14   Mar 07 2011 13:28:46   t13ae31
      *AKI01012: VSP Vorteilsargumentation
      *
      *   Rev 2.13   Jun 15 2010 09:50:10   t13ae31
      *AKI00974: VSP 2010
      *
      *   Rev 2.12   Feb 18 2009 14:10:16   t13ae31
      *AKI00795: Diff.deck in FK Best.verträgen Ermittl.
      *
      *   Rev 2.11   Dec 12 2008 14:42:56   t13ex52
      *AKI00772: Reisegepäckversicherung
      *
      *   Rev 2.9   Dec 05 2008 09:33:14   t13ex52
      *FKL00012: Pruefen SA401 auf vorkommende SA5nn
      *
      *   Rev 2.8   Jun 21 2008 12:51:00   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.7   May 08 2008 07:58:38   t13ae31
      *AKL00665: Allfinanz; REOB AMV KSDS VB
      *
      *   Rev 2.6   Apr 09 2008 17:02:52   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.5   Apr 02 2008 10:29:58   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.4   Mar 13 2008 14:28:18   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.3   Feb 12 2008 09:43:32   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.2   Jan 22 2008 12:17:20   t13ex14
      *AKL00665: Allfinanz; REOB-PK/FK Copystrecken
      *
      *   Rev 2.1   Dec 04 2007 16:46:04   t13ex52
      *AKL00664: Allfinanz: FK Bestandsdaten AM-Vers
      *
      *   Rev 2.0   Aug 14 2007 19:00:16   t13ex52
      *Archiverstellung
      *
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - Schablone für die Datensätze in der internen Tabelle, die*
      *       genutzt wird um die Vergleiche mit der KSDS vorzunehmen  *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *14.08.07! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
210108*21.01.08! Satzarten  f. Reob-PK eingefügt      ! WMI     !      *
      *        !                                      !         !      *
090408*09.04.08!  Alle Entitaeten auf 1.600 Byte      ! HSL     !      *
      *        !  gekürzt. Achtung !!! Dokumentation  !         !      *
      *        !  wurde nicht aktualisiert.           !         !      *
      *        !                                      !         !      *
060508*06.05.08! Umstellung FB in VB                  ! HHU     !      *
      *        !                                      !         !      *
230508*23.05.08! Erweiterung um je 1 Feld in der Enti-! HSL     !      *
      *        ! taet 701 und 702 und Verschiebung der!         !      *
      *        ! Felder INT-LFDNR und MAX-INTLFDNR an !         !      *
      *        ! das Ende der Satzarten.              !         !      *
      *        !                                      !         !      *
170908*17.09.08! Verarbeitung zur Deaktitivierung der ! HSL     !      *
      *        ! Entitaet 524[PK-Vertrag]             !         !      *
      *        !                                      !         !      *
091208*09.12.08! Integration neues AMV-Produkt Reise- ! HSL     !      *
      *        ! paeck-Versicherung Entitaet 504.     !         !      *
      *        ! Es wird hierfuer die nicht mehr be-  !         !      *
      *        ! noetigte Entitaet 524 in die Entitaet!         !      *
      *        ! 504 geaendert.                       !         !      *
      *        !                                      !         !      *
170209*17.02.09! Differenzdeckung FK eingebaut        ! HHU     !      *
      *        !                                      !         !      *
100610*10.06.10! VSP2010 eingebaut                    ! HHU     !      *
      *        !                                      !         !      *
220211*AKI01012|  Vorteilsargumentation eingefügt     |P. Hübner       *
      *        !                                      !         !      *
030811*AKI01037! Änd. SA 613                          !P. Hübner!      *
      *        !                                      !         !      *
      *AKI01102! neue SA 617                          !B. Stoldt!      *
      *        !                                      !         !      *
310115*AKI01295! neue SA 522 und 705 und Änd. 521     !P. Hübner!      *
      *        !                                      !         !      *
200415*AKI01358! Feldnamen gekuerzt                   !P. Hübner!      *
      *        !                                      !         !      *
120416*AKI01424! neue Felder Bestandasmanagement      !P. Hübner!      *
      *        !                                      !         !      *
180416*AKI01426! neue SA505 Reiseversicherung         !P. Hübner!      *
      *        !                                      !         !      *
120516*AKI01438! neue Felder SA400 SA401 Produktbez   !P. Hübner!      *
      *        !                                      !         !      *
260716*AKI01458! neue Felder SA614 Namensvorwort      !P. Hübner!      *
      *        !                                      !         !      *
080816*AKI01460! neue Felder SA400 SA510 Smart-Home   !P. Hübner!      *
      *        !                                      !         !      *
010217*AKI01460! neue Felder SA400 FLEXN und          !P. Hübner!      *
      *        ! unnötige alte Definitionen entfernt  !         !      *
      *        !                                      !         !      *
170717*AKI01530! SA614 Beruf auf 48 Stellen           !P. Hübner!      *
      *        !                                      !         !      *
      *AKI01785! 2 neue Felder FLEXN SA401            !P. Hübner!      *
      *        !                                      !         !      *
      *AKI01827! neues Feld SA400 PKBS-KBEZ für VSPKZ !P. Hübner!      *
 
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *                   A L L F I N A N Z - R E O B                  *
      *                    D T A - D A T E N S A T Z                   *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 2.000 BYTES                 STAND: 14.08.2007      *
      *                                                                *
      **************************************************************** *
 
      *01 SUH-TAB-SATZ.
      *Alias:
      *01 ZWSP-DTA-SATZ.
      *Alias:
      *01 ZWSP-DTA400-SATZ.
      **************************************************************** *
      *                          H E A D E R                           *
      *                                                                *
      *                                         STAND: 14.08.2007      *
      **************************************************************** *
       01 SUH-TAB-SATZ.
          05 SUH-TAB-HEADER.
            10 SUH-TAB-HD-KEY.
      ***  Schlüssel: Vertragsnummer
              15  SUH-TAB-HD-VTRGNUMMER            PIC 9(15).
      **** Schlüssel: Einzelvertrag HOST-ID
              15  SUH-TAB-HD-CVEZHOSTNR            PIC X(15).
      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
              15  SUH-TAB-HD-SATZART               PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
              15 SUH-TAB-HD-ETKEY                  PIC X(30).
 
      * **** BEGINN DER REDEFINITION FUER DIE ENTITAETEN-SCHLÜSSEL
      * **** JE SATZART WIRD EIN ENTITÄTEN-SCHLÜSSEL GEBRAUCHT.
              15 SUH-TAB-HD-ETKEY-400
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 4009
                20  SUH-TAB-HD-400-NGESELLST         PIC 9(9).
                20  SUH-TAB-HD-400-LEER              PIC X(21).
      ***
              15 SUH-TAB-HD-ETKEY-401
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 4066
                20  SUH-TAB-HD-401-NGESELLST         PIC 9(9).
      ***  Schlüssel: VEZ-Sparte FeldNr. 4057
                20  SUH-TAB-HD-401-NVEZSPARTE        PIC 9(9).
      ***  Datenfeld: VEZ-Tarif/Produkt FeldNr. 4058
                20  SUH-TAB-HD-401-NVEZPBRDNR        PIC 9(9).
                20  SUH-TAB-HD-401-LEER              PIC X(3).
      ***
              15 SUH-TAB-HD-ETKEY-403
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6359
                20  SUH-TAB-HD-403-NGESELLST         PIC 9(9).
      ***  Schlüssel: Risikoadresse-HostID FeldNr. 6366
                20  SUH-TAB-HD-403-NRISADRHID        PIC 9(9).
                20  SUH-TAB-HD-403-LEER              PIC X(12).
 
091208        15 SUH-TAB-HD-ETKEY-504
    >>                                REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5889
    <<           20  SUH-TAB-HD-504-NGESELLST         PIC 9(9).
091208           20  SUH-TAB-HD-504-LEER              PIC X(21).
 
              15 SUH-TAB-HD-ETKEY-505
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5889
                 20  SUH-TAB-HD-505-NGESELLST         PIC 9(9).
                 20  SUH-TAB-HD-505-LEER              PIC X(21).
 
210108        15 SUH-TAB-HD-ETKEY-508
>>                                    REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5293
                 20  SUH-TAB-HD-508-NGESELLST         PIC 9(9).
                 20  SUH-TAB-HD-508-LEER              PIC X(21).
 
              15 SUH-TAB-HD-ETKEY-509
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5321
                 20  SUH-TAB-HD-509-NGESELLST         PIC 9(9).
                 20  SUH-TAB-HD-509-LEER              PIC X(21).
 
              15 SUH-TAB-HD-ETKEY-510
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5361
                 20  SUH-TAB-HD-510-NGESELLST         PIC 9(9).
                 20  SUH-TAB-HD-510-LEER              PIC X(21).
 
              15 SUH-TAB-HD-ETKEY-520
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5654
                 20  SUH-TAB-HD-520-NGESELLST         PIC 9(9).
                 20  SUH-TAB-HD-520-LEER              PIC X(21).
 
              15 SUH-TAB-HD-ETKEY-521
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5704
                 20  SUH-TAB-HD-521-NGESELLST         PIC 9(9).
    <<           20  SUH-TAB-HD-521-LEER              PIC X(21).
210108
310115        15 SUH-TAB-HD-ETKEY-522
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 8004
310115           20  SUH-TAB-HD-522-NGESELLST         PIC 9(9).
    <<           20  SUH-TAB-HD-522-LEER              PIC X(21).
210108
              15 SUH-TAB-HD-ETKEY-523
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5889
                20  SUH-TAB-HD-523-NGESELLST         PIC 9(9).
                20  SUH-TAB-HD-523-LEER              PIC X(21).
      ***
              15 SUH-TAB-HD-ETKEY-613
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6140
               20  SUH-TAB-HD-613-NGESELLST         PIC 9(9).
      *** Schlüssel: Komponentengruppe FeldNr. 9317
030811         20  SUH-TAB-HD-613-RGID              PIC 9(4).
      ***  Schlüssel: laufende Nummer FeldNr. 6136
               20  SUH-TAB-HD-613-PSGBLFDNR         PIC 999.
030811         20  SUH-TAB-HD-613-LEER              PIC X(14).
      ***
210108        15 SUH-TAB-HD-ETKEY-614
>>                                    REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6165
                 20  SUH-TAB-HD-614-NGESELLST         PIC 9(9).
      ***  Schlüssel: versicherte Person laufende Nummer (Schlüssel)
      ***             FeldNr. 6144
                 20  SUH-TAB-HD-614-PUFVPLFDNR        PIC 999.
                 20  SUH-TAB-HD-614-LEER              PIC X(18).
 
              15 SUH-TAB-HD-ETKEY-615
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6178
                 20  SUH-TAB-HD-615-NGESELLST         PIC 9(9).
      ***  Schlüssel: Laufende Nummer der Selbstbeteiligung
      ***             FeldNr. 6170
                 20  SUH-TAB-HD-615-PVGSBLFDNR        PIC 999.
                 20  SUH-TAB-HD-615-LEER              PIC X(18).
 
              15 SUH-TAB-HD-ETKEY-616
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6459
                 20  SUH-TAB-HD-616-NGESELLST         PIC 9(9).
      ***  Schlüssel: VP laufende Nummer FeldNr. 6445
                 20  SUH-TAB-HD-616-PUFVPLFDNR        PIC 999.
      ***  Schlüssel: MVP laufende Nummer FeldNr. 6446
                 20  SUH-TAB-HD-616-PUFMVPLFDNR       PIC 999.
                 20  SUH-TAB-HD-616-LEER              PIC X(15).
    <<
210108
231111        15 SUH-TAB-HD-ETKEY-617
    >>                               REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 138
                 20  SUH-TAB-HD-617-NGESELLST         PIC 9(9).
      ***  Schlüssel: Komponentengruppennummer FeldNr. 141
                 20  SUH-TAB-HD-617-PVRIGRP           PIC 9(04).
      ***  Schlüssel: lfd. Vertragsjahr FeldNr. 142
                 20  SUH-TAB-HD-617-LFD-JAHR          PIC 99.
    <<           20  SUH-TAB-HD-617-LEER              PIC X(15).
231111
 
              15 SUH-TAB-HD-ETKEY-628
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6401
                20  SUH-TAB-HD-628-NGESELLST         PIC 9(9).
      ***  Schlüssel: Subjekt-Host-ID FeldNr. 6399
                20  SUH-TAB-HD-628-NVRGSUBHID        PIC 9(9).
      ***  Schlüssel: Risikogruppe-ID FeldNr. 6352
                20  SUH-TAB-HD-628-PVRGID            PIC 9(4).
                20  SUH-TAB-HD-628-LEER              PIC X(8).
      ***
              15 SUH-TAB-HD-ETKEY-629
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6408
                20  SUH-TAB-HD-629-NGESELLST         PIC 9(9).
      ***  Schlüssel: Risikogruppe-ID FeldNr. 6361
                20  SUH-TAB-HD-629-PVRGID            PIC 9(4).
      ***  Schlüssel: Risikomerkmalwert-ID FeldNr. 6362
                20  SUH-TAB-HD-629-PVRISMID          PIC 999.
                20  SUH-TAB-HD-629-LEER              PIC X(14).
      ***
              15 SUH-TAB-HD-ETKEY-630
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6415
                20  SUH-TAB-HD-630-NGESELLST         PIC 9(9).
      ***  Schlüssel: Risikogruppe-ID FeldNr. 6372
                20  SUH-TAB-HD-630-PVRGID            PIC 9(4).
      ***  Schlüssel: Selbstbeteiligung-ID FeldNr. 6373
                20  SUH-TAB-HD-630-PVRSBID           PIC 999.
                20  SUH-TAB-HD-630-LEER              PIC X(14).
      ***
              15 SUH-TAB-HD-ETKEY-631
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6391
                20  SUH-TAB-HD-631-NGESELLST         PIC 9(9).
      ***  Schlüssel: Risikogruppe-ID FeldNr. 6381
                20  SUH-TAB-HD-631-PVRGID            PIC 9(4).
      ***  Schlüssel: Leistung-ID FeldNr. 6382
                20  SUH-TAB-HD-631-PVLSTID           PIC 999.
      ***  Schlüssel: Themengebietsschlüssel FeldNr. 6443
                20  SUH-TAB-HD-631-PTGSCHL           PIC 999.
                20  SUH-TAB-HD-631-LEER              PIC X(11).
      ***
              15 SUH-TAB-HD-ETKEY-701
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7062
                20  SUH-TAB-HD-701-NGESELLST         PIC 9(9).
      ***  Schlüssel: Universelle Laufende Nummer FeldNr. 7055
                20  SUH-TAB-HD-701-PZUNILFDNR        PIC 999.
                20  SUH-TAB-HD-701-LEER              PIC X(18).
      ***
              15 SUH-TAB-HD-ETKEY-702
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7082
                20  SUH-TAB-HD-702-NGESELLST         PIC 9(9).
      ***  Schlüssel: Risikogruppe FeldNr. 7066
                20  SUH-TAB-HD-702-PVRIGRP           PIC 9(4).
      ***  Schlüssel: Themengebietsschlüssel FeldNr. 7067
                20  SUH-TAB-HD-702-PTGSCHL           PIC 999.
      ***  Schlüssel: Risiko-Lfdnr FeldNr. 7068
                20  SUH-TAB-HD-702-PVRILFDNR         PIC 999.
                20  SUH-TAB-HD-702-LEER              PIC X(11).
 
310115     15 SUH-TAB-HD-ETKEY-705
                                      REDEFINES SUH-TAB-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7503
             20 SUH-TAB-HD-705-NGESELLST          PIC 9(9).
      ***  Schlüssel: Risikogruppe FeldNr. 7504
             20 SUH-TAB-HD-705-PVRIGRP            PIC 9(4).
      ***  Schlüssel: Themengebietsschluessel FeldNr. 7505
             20 SUH-TAB-HD-705-PTGSCHL            PIC 999.
      ***  Schlüssel: Risiko-Lfdnr FeldNr. 7506
             20 SUH-TAB-HD-705-PVRILFDNR          PIC 999.
             20 SUH-TAB-HD-705-LEER               PIC X(11).
      * **** Ende der Redefinition fuer die ENTITAETEN-Schlüssel
 
      ***  Headerinformation: Vermögensberater
            10  SUH-TAB-HD-VB-NUMMER                 PIC 9(7).
      ***  Headerinformation: Herkunftssystem
            10  SUH-TAB-HD-HERKUNFT                  PIC XXX.
      ***  Headerinformation: Aenderungskennzeichen
      ***  Inhalt: 00 = neuer Vertrag(immer der gesamte Vertrag)
      ***          01 = Vertragsänderung
      ***          99 = Vertragsstorno(immer der gesamte Vertrag)
            10  SUH-TAB-HD-AEND-KNZ                  PIC 99.
      ***  Headerinformation: DVAG-interne Vertragsnummer PIF oder PIN
            10  SUH-TAB-HD-PIN                       PIC 9(9).
      ***  Headerinformation: Prüfziffer zur PIN
            10  SUH-TAB-HD-PIN-PRUEF                 PIC 99.
      ***  Headerinformation: Quittungsnummer
            10  SUH-TAB-HD-QUITTUNGSNR               PIC 9(15).
      ***  Headerinformation: Erstellungsdatum (im Format JJJJMMTT)
            10  SUH-TAB-HD-ERSTELL-DATUM             PIC 9(8).
      ***  Headerinformation: Erstellungszeit (im Format HHMMSS)
            10  SUH-TAB-HD-ERSTELL-ZEIT              PIC 9(6).
      ***  Headerinformation: Leerstellen
            10  SUH-TAB-HD-LEERSTELLEN               PIC X(80).
      **************************************************************** *
      *                                                                *
      *                       D A T E N T E I L                        *
      *                                                                *
      *     LAENGE: 1.405 BYTES                 STAND: 17.02.2009      *
      *                                                                *
      **************************************************************** *
 
     **** 05 SUH-TAB-DATEN                           PIC X(1805).
060508    05 SUH-TAB-DATEN                           PIC X(1405).
 
      **************************************************************** *
      *                                                                *
      *                         V E R T R A G                          *
      *                                                                *
      **************************************************************** *
 
          05 SUH-TAB-400-VERTRAG REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Antragsnummer FeldNr. 4003
               10  SUH-TAB-400-CANNR             PIC X(18).
 
      ***  Datenfeld: Merkmal Vermittler 1 zum Vertrag FeldNr. 4004
               10  SUH-TAB-400-CV1MERKM          PIC X(20).
 
      ***  Datenfeld: Vermittlernummer 1 zum Vertrag FeldNr. 4005
               10  SUH-TAB-400-CV1VTVMNR         PIC X(10).
 
      ***  Datenfeld: Merkmal Vermittler 2 zum Vertrag FeldNr. 4006
               10  SUH-TAB-400-CV2MERKM          PIC X(20).
 
      ***  Datenfeld: Vermittlernummer 2 zum Vertrag FeldNr. 4007
               10  SUH-TAB-400-CV2VTVMNR         PIC X(10).
 
      ***  Datenfeld: Tarif/Produkt FeldNr. 4008
               10  SUH-TAB-400-CPBRDNR           PIC X(50).
 
      ***  Datenfeld: Vertragssparte FeldNr. 4010
               10  SUH-TAB-400-NSPARTE           PIC 9(9).
 
      ***  Datenfeld: Vertragsart FeldNr. 4011
               10  SUH-TAB-400-NVTART            PIC 9(9).
 
      ***  Datenfeld: Vertragsbeginn FeldNr. 4012
               10  SUH-TAB-400-CVBEGINN          PIC X(8).
 
      ***  Datenfeld: Vertragsablauf FeldNr. 4013
               10  SUH-TAB-400-CVABLAUF          PIC X(8).
 
      ***  Datenfeld: Vertragsstatus FeldNr. 4014
               10  SUH-TAB-400-NVTSTATUS         PIC 9(9).
 
      ***  Datenfeld: Versichertes Risiko FeldNr. 4015
               10  SUH-TAB-400-CVERSRISK         PIC X(23).
 
      ***  Datenfeld: Änderungsdatum FeldNr. 4016
               10  SUH-TAB-400-CVAEND1D          PIC X(8).
 
      ***  Datenfeld: Änderungsgrund FeldNr. 4017
               10  SUH-TAB-400-CVAEND1G          PIC X(50).
 
      ***  Datenfeld: AFB-Anteil in Prozent FeldNr. 4018
               10  SUH-TAB-400-NAFBPROZ          PIC 99V999.
 
      ***  Datenfeld: Bündelungskennzeichen FeldNr. 4020
               10  SUH-TAB-400-CBUEKZ            PIC X.
 
      ***  Datenfeld: Vertragsinfo FeldNr. 4022
               10  SUH-TAB-400-CVTZINFO          PIC X(80).
 
      ***  Datenfeld: Vertragssumme FeldNr. 4027
               10  SUH-TAB-400-NVSUMME           PIC 9(10).
 
      ***  Datenfeld: Ursprungswährung FeldNr. 4030
               10  SUH-TAB-400-NVTURWAEHR        PIC 9(9).
 
      ***  Datenfeld: Inkassoart FeldNr. 4031
               10  SUH-TAB-400-CINKFORM          PIC X(30).
 
      ***  Datenfeld: Versicherungszahlungsweise FeldNr. 4032
               10  SUH-TAB-400-NINKZHLWSE        PIC 9(9).
 
      ***  Datenfeld: Beitrag gemäss Zahlungsweise FeldNr. 4033
               10  SUH-TAB-400-NINKVSGBTR        PIC 9(10)V99.
 
      ***  Datenfeld: Jahresgesamtbeitrag brutto gemäss
      ***             Zahlungsweise. FeldNr. 4034
               10  SUH-TAB-400-NINKJGSBTR        PIC 9(10)V99.
 
      ***  Datenfeld: Fälligkeitsdatum FeldNr. 4035
               10  SUH-TAB-400-CINKFLGDAT        PIC X(8).
 
      ***  Datenfeld: Jahresgesamtbeitrag netto FeldNr. 4036
               10  SUH-TAB-400-NINKJGNBTR        PIC 9(8)V99.
 
      ***  Datenfeld: Mahnstufe FeldNr. 4037
               10  SUH-TAB-400-CINKMHNSTF        PIC X(25).
 
      ***  Datenfeld: Mahndatum FeldNr. 4038
               10  SUH-TAB-400-CINKMHNDAT        PIC X(8).
 
      ***  Datenfeld: Rückstandsbetrag FeldNr. 4039
               10  SUH-TAB-400-NINKRSBTR         PIC 9(7)V99.
 
      ***  Datenfeld: Laufzeitrabatt in % FeldNr. 4040
               10  SUH-TAB-400-NINKLFZRB         PIC 99.
 
      ***  Datenfeld: Ratenzuschlag FeldNr. 4041
               10  SUH-TAB-400-NINKRTZS          PIC 99.
 
      ***  Datenfeld: Zeichnung FeldNr. 4042
               10  SUH-TAB-400-CINKZCHNNG        PIC X(25).
 
      ***  Datenfeld: F/B-Beitrag brutto FeldNr. 4043
               10  SUH-TAB-400-NINKFBBTR         PIC 9(8)V99.
 
      ***  Datenfeld: F/B-Summe FeldNr. 4044
               10  SUH-TAB-400-NINKFBSUM         PIC 9(12).
 
      ***  Datenfeld: Einlösungsbeitrag brutto FeldNr. 4045
               10  SUH-TAB-400-NINKELBTBR        PIC 9(8)V99.
 
      ***  Datenfeld: Einlösungsbeitrag netto FeldNr. 4046
               10  SUH-TAB-400-NINKELBTNT        PIC 9(8)V99.
 
      ***  Datenfeld: Letzte Synchronisation FeldNr. 4047
               10  SUH-TAB-400-CINKLSTSYN        PIC X(8).
 
      ***  Datenfeld: Letzte Agenturbearbeitung FeldNr. 4048
               10  SUH-TAB-400-CINKLSTAGT        PIC X(8).
 
      ***  Datenfeld: Letzte Bewegung FeldNr. 4049
               10  SUH-TAB-400-CINKLSTBWG        PIC X(61).
 
      ***  Datenfeld: Kundenbonus FeldNr. 4050
               10  SUH-TAB-400-CINKKDBON         PIC X(26).
 
      ***  Datenfeld: Ostrabatt FeldNr. 4051
               10  SUH-TAB-400-NINKOSTRBT        PIC 99.
 
      ***  Datenfeld: Kurztext Bestandssicherungsaktion FeldNr. 4094
               10  SUH-TAB-400-CINKBSA           PIC X(6).
 
      ***  Datenfeld: Bankleitzahl FeldNr. 4100
               10  SUH-TAB-400-NINKBLZ           PIC 9(8).
 
      ***  Datenfeld: Kontonummer FeldNr. 4101
               10  SUH-TAB-400-CINKKTONR         PIC X(15).
 
      ***  Datenfeld: Kontoinhaber FeldNr. 4102
               10  SUH-TAB-400-CINKKTOINH        PIC X(40).
 
      ***  Datenfeld: Langtext Bestandssicherungsaktion FeldNr. 4110
               10  SUH-TAB-400-CVGBSALTXT        PIC X(50).
 
      ***  Datenfeld: Versicherungsgesellschaft Text FeldNr. 4111
               10  SUH-TAB-400-CGESELLST         PIC X(50).
 
      ***  Datenfeld: VD-Nr. FeldNr. 4112
               10  SUH-TAB-400-NVDNR             PIC 9(5).
 
      ***  Datenfeld: VD-Name FeldNr. 4113
               10  SUH-TAB-400-CVDNAME           PIC X(20).
 
      ***  Datenfeld: Vor-Vertragsnummer FeldNr. 4114
               10  SUH-TAB-400-CVORVNR           PIC X(17).
 
120416***  Datenfeld: alte Vertragsnummer
             10 SUH-DTA-400-ALTVNR             PIC X(17).
 
120416***  Datenfeld: Aktionsergebnis FeldNr. 4119
             10  SUH-TAB-400-AKTIONERG        PIC X(3).
 
      ***  Datenfeld: Produktkombibezeichnung
120516       10 SUH-TAB-400-PRDKOMBIBEZ       PIC X(50).
 
      ***  Datenfeld: Smart-Home KundenID FeldNr. 4151
080816       10 SUH-TAB-400-KUNDENIDSMH       PIC X(10).
 
      ***  Datenfeld: FLEX-VSP-Nachlass Feld 4156
010217       10 SUH-TAB-400-FLEXNVSP          PIC 9(9)V9(2).
 
      ***  Datenfeld: FLEX-VSP-Nachlass gewährt  Feld 4157
010217       10 SUH-TAB-400-FLEXNVSPGW        PIC X.
 
      ***  Datenfeld: FLEX-VGV-Nachlass Feld 4158
010217       10 SUH-TAB-400-FLEXNVGV          PIC 9(9)V9(2).
 
      ***  Datenfeld: FLEX-VGV-Nachlass gewährt  Feld 4159
010217       10 SUH-TAB-400-FLEXNVGVGW        PIC X.
 
      ***Datenfeld: PKBS-KBEZ (VSP = J / N) Feld 4152
             10 SUH-TAB-400-PKBS-KBEZ         PIC X(3).
      ***  Datenfeld: Leerstellen
010217         10  SUH-TAB-400-LEERSTELLEN         PIC X(431).
 
      ******************************************************************
      *                                                                *
      *              401  E I N Z E L V E R T R A G                    *
      *                                                                *
      ******************************************************************
 
          05 SUH-TAB-401-EINZELVERTRAG REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: VEZ-Beginn FeldNr. 4059 Format: JJJJMMTT
               10  SUH-TAB-401-CVEZBEGINN        PIC X(8).
 
      ***  Datenfeld: VEZ-Ablauf FeldNr. 4060 Format: JJJJMMTT
               10  SUH-TAB-401-CVEZABLAUF        PIC X(8).
 
      ***  Datenfeld: VEZ-Status FeldNr. 4061
               10  SUH-TAB-401-NVEZSTATUS        PIC 9(9).
 
      ***  Datenfeld: VEZ-Bruttobeitrag gem. ZW FeldNr. 4062
               10  SUH-TAB-401-NVEZBTRG          PIC 9(8)V99.
 
      ***  Datenfeld: VEZ-Summe FeldNr. 4063
               10  SUH-TAB-401-NVEZSUMME         PIC 9(10).
 
      ***  Datenfeld: Vertragsart FeldNr. 4065
               10  SUH-TAB-401-NVTART            PIC 9(9).
 
      ***  Datenfeld: VEZ-Aenderungsgrund FeldNr. 4095
               10  SUH-TAB-401-CVEZAENDG         PIC X(50).
 
      ***  Datenfeld: VEZ-Aenderungsdatum FeldNr. 4096
               10  SUH-TAB-401-CVEZAENDDT        PIC X(8).
 
      ***  Datenfeld: VEZ-Nettojahresbeitrag FeldNr. 4106
               10  SUH-TAB-401-NVEZNJB           PIC 9(8)V99.
 
      ***  Datenfeld: VEZ-Bruttojahresbeitrag FeldNr. 4107
               10  SUH-TAB-401-NVEZBJB           PIC 9(8)V99.
 
      ***  Datenfeld: VEZ-Nettobeitrag gem. ZW FeldNr. 4108
               10  SUH-TAB-401-NVEZFBNZW         PIC 9(8)V99.
 
      ***  Datenfeld: VEZ-Datum der letzten Bearbeitung FeldNr. 4109
               10  SUH-TAB-401-CVEZLBEARB        PIC X(8).
 
100610***  Datenfeld: Geschäftszweig FeldNr. 4115 ( FK FH PK )
               10  SUH-TAB-401-CGZWEIG           PIC XX.
100610*** Datenfeld: Rabattierter Jahresnettobeitr. Feldnr. 9301
      ***    inkl. TZ-Zuschlag exkl. Steuer
             10  SUH-TAB-401-RABNETTO          PIC 9(8)V99.
      ***  Datenfeld: Summe aller Komponentenbeiträge Feldnr. 9302
             10  SUH-TAB-401-STAKTBTR          PIC 9(8)V99.
220211***  Datenfeld: REOBPRODUKTNUMMER  Feldnr. 4164
220211       10  SUH-TAB-401-NREOBPRDNR        PIC 9(5).
120516       10  SUH-TAB-401-NBFELDER          PIC X(35).
120516***  Datenfeld: Produktbezeichnung
             10  SUH-TAB-401-PRODUKTBEZ        PIC X(50).
      ***  Datenfeld: FLEXN Betrag Feldnr. 4160
             10  SUH-TAB-401-FLEXNSP            PIC 9(9)V99.
             10  SUH-TAB-401-FLEXNSPGW          PIC X.
      *** V = FlexN gewährt G = FlexN geplatzt
      *** E = FlexN erhöht  S = FlexN storniert mit Guthaben
      ***  Datenfeld: Leerstellen
120516       10  SUH-TAB-401-LEERSTELLEN         PIC X(1123).
      *
230508**** Datenfeld: Interne lfd. Nummer für die Datenbank
    >>         10  SUH-TAB-401-INTLFDNR          PIC 9(4).
 
      **** Datenfeld: Max Interne lfd. Nummer für die Datenbank
    <<         10  SUH-TAB-401-MAX-INTLFDNR      PIC 9(4).
230508***
      *
      ******************************************************************
      *                                                                *
      *      403  V E R T R A G S R I S I K O A D R E S S E            *
      *                                                                *
      ******************************************************************
 
          05 SUH-TAB-403-RISIKOADRESSE REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Strasse FeldNr. 6367
               10  SUH-TAB-403-CRISADRSTR        PIC X(40).
 
      ***  Datenfeld: PLZ FeldNr. 6368
               10  SUH-TAB-403-CRISADRPLZ        PIC X(6).
 
      ***  Datenfeld: Ort FeldNr. 6369
               10  SUH-TAB-403-CRISADRORT        PIC X(40).
 
      ***  Datenfeld: Länderkennzeichen FeldNr. 6370
               10  SUH-TAB-403-CRISADRLKZ        PIC 9(9).
 
      ***  Datenfeld: Leerstellen
090408         10  SUH-TAB-403-LEERSTELLEN         PIC X(1310).
 
091208******************************************************************
    >>*                                                                *
      *                504 - P K - R E I S E G E P A E C K -           *
      *                    V E R S I C H E R U N G                     *
      *                                                                *
      ******************************************************************
         05 SUH-TAB-504-REISEGEPAECK REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Bedingung FeldNr. 9177
            10 SUH-TAB-504-BEDINGUNG                PIC X(30).
 
      ***  Datenfeld: Versicherungssumme. FeldNr. 9178
            10 SUH-TAB-504-VERSSUMME                PIC 9(8).
 
      ***  Datenfeld: Mailingaktion FeldNr. 9179
            10 SUH-TAB-504-MAILING                  PIC X(70).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 9180
            10 SUH-TAB-504-ZUSATZINFO               PIC X(100).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9181
            10 SUH-TAB-504-DIFFDECK                 PIC X(2).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9182
            10 SUH-TAB-504-GESELLSCHAFT             PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 9183
            10 SUH-TAB-504-DDVERSNR                 PIC X(40).
 
      ***  Datenfeld: Umstellungstermin   FeldNr. 9184
            10 SUH-TAB-504-UMSTELLTERMIN            PIC 9(8).
 
      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9185
            10 SUH-TAB-504-DDKUENDBEST              PIC X(2).
      ***
      ***  Datenfeld: Leerstellen
            10 SUH-TAB-504-LEERSTELLEN              PIC X(1091).
 
    <<**** Datenfeld: Interne lfd. Nummer für die Datenbank
091208      10  SUH-TAB-504-INTLFDNR                PIC 9(4).
 
    ***  Allgemeiner Datenteil ************************************
         05  SUH-TAB-505-REISE REDEFINES SUH-TAB-DATEN.
    ***  Datenfeld: Bedingung FeldNr. 9260
           10 SUH-TAB-505-BEDINGUNG                PIC X(30).
 
    ***  Datenfeld: Versicherungssumme. Reisegepäck FeldNr. 9261
           10 SUH-TAB-505-VSUMRGEPCK               PIC 9(8).
 
    ***  Datenfeld: Versicherungssumme. Reiserücktritt FeldNr. 9262
           10 SUH-TAB-505-VSUMRRUECK               PIC 9(8).
 
    ***  Datenfeld: Mailingaktion FeldNr. 9663
           10 SUH-TAB-505-MAILING                  PIC X(70).
 
    ***  Datenfeld: Zusatzinfo FeldNr. 9664
           10 SUH-TAB-505-ZUSATZINFO               PIC X(100).
 
    ***  Datenfeld: Informationsquelle FeldNr. 9665
           10 SUH-TAB-505-LVTQUELLP                PIC 9(9).
 
    ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9670
           10 SUH-TAB-505-DIFFDECK                 PIC X(2).
 
    ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9671
           10 SUH-TAB-505-GESELLSCHAFT             PIC X(50).
 
    ***  Datenfeld: Versicherungsnummer FeldNr. 9672
           10 SUH-TAB-505-DDVERSNR                 PIC X(40).
 
    ***  Datenfeld: Umstellungstermin   FeldNr. 9673
           10 SUH-TAB-505-UMSTELLTERMIN            PIC 9(8).
 
    ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9674
           10 SUH-TAB-505-DDKUENDBEST              PIC X(2).
 
    ***  Datenfeld: Leerstellen
            10 SUH-TAB-505-LEERSTELLEN              PIC X(1074).
 
      **** Datenfeld: Interne lfd. Nummer für die Datenbank
            10  SUH-TAB-505-INTLFDNR                PIC 9(4).
 
 
210108******************************************************************
>>    *                                                                *
      *                508 G L A S - V E R T R A G                     *
      *                                                                *
      *                                                                *
      ******************************************************************
 
           05  SUH-TAB-508-GLAS REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5276
               10  SUH-TAB-508-CGLRSPLZ          PIC X(6).
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5277
               10  SUH-TAB-508-CGLRSSTR          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5278
               10  SUH-TAB-508-CGLRSORT          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5279
               10  SUH-TAB-508-CGLRSHSNR         PIC X(15).
 
      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5280
               10  SUH-TAB-508-NGLRSNKZ          PIC 9(9).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5281
               10  SUH-TAB-508-CGLKLAUS          PIC X(80).
 
      ***  Datenfeld: Bedingung FeldNr. 5282
               10  SUH-TAB-508-CGLLBVBEDG        PIC X(16).
 
      ***  Datenfeld: Geb{udetyp FeldNr. 5283
               10  SUH-TAB-508-NGLLBGEBTY        PIC 9(9).
 
      ***  Datenfeld: Risiko FeldNr. 5285
               10  SUH-TAB-508-CGLLBRIS          PIC X(50).
 
      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5286
               10  SUH-TAB-508-NGLJNMBTR         PIC XX.
 
      ***  Datenfeld: SB-Prz FeldNr. 5287
               10  SUH-TAB-508-NGLSBPRZ          PIC 99V99.
 
      ***  Datenfeld: SB-Fest FeldNr. 5288
               10  SUH-TAB-508-NGLSBFES          PIC 9(5).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5289
               10  SUH-TAB-508-CGLZUSINFO        PIC X(100).
 
      ***  Datenfeld: Leistungsfrei von Datum FeldNr. 5290
               10  SUH-TAB-508-CGLDTLSTFV        PIC X(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum FeldNr. 5291
               10  SUH-TAB-508-CGLDTLSTFB        PIC X(8).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5940
               10  SUH-TAB-508-CMAILING          PIC X(70).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5533
               10  SUH-TAB-508-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 5534
               10  SUH-TAB-508-CUMSTELLTERM      PIC X(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5536
               10  SUH-TAB-508-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5537
               10  SUH-TAB-508-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein FeldNr. 5538
               10  SUH-TAB-508-NKUENDBEST        PIC X(2).
230508***
      ***  Datenfeld: Leerstellen
090408         10  SUH-TAB-508-LEERSTELLEN         PIC X(857).
230508***
    >>**** Datenfeld: Interne lfd. Nummer für die Datenbank
    <<         10  SUH-TAB-508-INTLFDNR          PIC 9(4).
230508***
      *
      *****************************************************************
      *                                                               *
      *        509  H A F T P F L I C H T - V E R T R A G             *
      *                                                               *
      *                                                               *
      *****************************************************************
      ***
           05  SUH-TAB-509-HAFTPFLICHT REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Bedingung FeldNr. 5297
               10  SUH-TAB-509-CHFVBEDG          PIC X(16).
 
      ***  Datenfeld: Risiko FeldNr. 5298
               10  SUH-TAB-509-CHFLBRIS          PIC X(50).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5299
               10  SUH-TAB-509-CHFKLAUS          PIC X(80).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5301
               10  SUH-TAB-509-CHFZUSINFO        PIC X(100).
 
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5302
               10  SUH-TAB-509-CHFRSPLZ          PIC X(6).
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5303
               10  SUH-TAB-509-CHFRSSTR          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5304
               10  SUH-TAB-509-CHFRSORT          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5305
               10  SUH-TAB-509-NHFRSNKZ          PIC 9(9).
 
      ***  Datenfeld: Mitversicherte Personen FeldNr. 5306
               10  SUH-TAB-509-CHFMVPERS         PIC X(35).
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5307
               10  SUH-TAB-509-CHFRSHSNR         PIC X(15).
 
      ***  Datenfeld: Regulierung Ja/Nein FeldNr. 5308
               10  SUH-TAB-509-NHFJNREGUL        PIC XX.
 
      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5309
               10  SUH-TAB-509-NHFJNMBTR         PIC XX.
 
      ***  Datenfeld: Leistungsfrei von Datum FeldNr. 5310
               10  SUH-TAB-509-CHFDTLSTFV        PIC X(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum FeldNr. 5311
               10  SUH-TAB-509-CHFDTLSTFB        PIC X(8).
 
      ***  Datenfeld: SB Ja/Nein FeldNr. 5312
               10  SUH-TAB-509-NHFJNSB           PIC XX.
 
      ***  Datenfeld: SB-Prz FeldNr. 5313
               10  SUH-TAB-509-NHFSBPRZ          PIC 999.
 
      ***  Datenfeld: SB-Fest FeldNr. 5314
               10  SUH-TAB-509-NHFSBFES          PIC 9(5).
 
      ***  Datenfeld: SB-Text FeldNr. 5315
               10  SUH-TAB-509-CHFSBTXT          PIC X(50).
 
      ***  Datenfeld: Auslandsch{den Ja/Nein FeldNr. 5316
               10  SUH-TAB-509-NHFJNAUSL         PIC XX.
 
      ***  Datenfeld: Verwandschaftsschäden Ja/Nein FeldNr. 5317
               10  SUH-TAB-509-NHFJNVERW         PIC XX.
 
      ***  Datenfeld: Produkt FeldNr. 5318
               10  SUH-TAB-509-CHFPRD            PIC X(30).
 
      ***  Datenfeld: Namen Elternteil/volljährige Kinder FeldNr. 5319
               10  SUH-TAB-509-CHNAMENEK         PIC X(45).
 
      ***  Datenfeld: Namen Elternteil / volljährige Kinder FeldNr. 5859
               10  SUH-TAB-509-CHNAMEKIND        PIC X(45).
 
      ***  Datenfeld: abweichende VP FeldNr. 5860
               10  SUH-TAB-509-CHNAMEABVP        PIC X(35).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5941
               10  SUH-TAB-509-CMAILING          PIC X(70).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5527
               10  SUH-TAB-509-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 5528
               10  SUH-TAB-509-CUMSTELLTERM      PIC X(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5530
               10  SUH-TAB-509-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5531
               10  SUH-TAB-509-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: Kœndigungsbest{tigung Ja/Nein FeldNr. 5532
               10  SUH-TAB-509-NKUENDBEST        PIC X(2).
 
      ***  Datenfeld: Zielgruppe FeldNr. 5987
               10  SUH-TAB-509-CZIELGRP          PIC X(30).
230508***
      ***  Datenfeld: Leerstellen
090408         10  SUH-TAB-509-LEERSTELLEN         PIC X(589).
230508***
    >>**** Datenfeld: Interne lfd. Nummer für die Datenbank
    <<         10  SUH-TAB-509-INTLFDNR          PIC 9(4).
230508***
      *****************************************************************
      *                                                               *
      *           510   H A U S R A T - V E R T R A G                 *
      *                                                               *
      *****************************************************************
      ***
           05  SUH-TAB-510-HAUSRAT REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5325
               10  SUH-TAB-510-CHRRSPLZ          PIC X(6).
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5326
               10  SUH-TAB-510-CHRRSSTR          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5327
               10  SUH-TAB-510-CHRRSORT          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5328
               10  SUH-TAB-510-NHRRSNKZ          PIC 9(9).
 
      ***  Datenfeld: Bedingung FeldNr. 5329
               10  SUH-TAB-510-CHRLBVBEDG        PIC X(16).
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5330
               10  SUH-TAB-510-CHRRSHSNR         PIC X(15).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5331
               10  SUH-TAB-510-CHRLBKLAUS        PIC X(80).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5333
               10  SUH-TAB-510-CHRZUSINFO        PIC X(100).
 
      ***  Datenfeld: Leistungsfrei von Datum FeldNr. 5334
               10  SUH-TAB-510-CHRDTLSTFV        PIC X(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum FeldNr. 5335
               10  SUH-TAB-510-CHRDTLSTFB        PIC X(8).
 
      ***  Datenfeld: SelbstBeteiligung Ja/Nein FeldNr. 5336
               10  SUH-TAB-510-NHRJNSB           PIC XX.
 
      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 5337
               10  SUH-TAB-510-NHRSBPRZ          PIC 999.
 
      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 5338
               10  SUH-TAB-510-NHRSBFES          PIC 9(5).
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5339
               10  SUH-TAB-510-CHRSBTXT          PIC X(50).
 
      ***  Datenfeld: Nutzung FeldNr. 5340
               10  SUH-TAB-510-NHRLBNUTZ         PIC 9(9).
 
      ***  Datenfeld: Versicherungssumme FeldNr. 5341
               10  SUH-TAB-510-NHRVSUM           PIC 9(8).
 
      ***  Datenfeld: Wohnfl{che in qm FeldNr. 5342
               10  SUH-TAB-510-NHRWOHNFL         PIC 9(5).
 
      ***  Datenfeld: Unterversicherungsverzicht Ja/Nein FeldNr. 5343
               10  SUH-TAB-510-NHRJNUVZ          PIC XX.
 
      ***  Datenfeld: Geb{udetyp FeldNr. 5344
               10  SUH-TAB-510-NHRLBGEBTY        PIC 9(9).
 
      ***  Datenfeld: Eigentumsverh{ltnisse FeldNr. 5345
               10  SUH-TAB-510-NHRLBEIGEN        PIC 9(9).
 
      ***  Datenfeld: Tarifzone FeldNr. 5346
               10  SUH-TAB-510-CHRZONE           PIC X(4).
 
      ***  Datenfeld: Bauartklasse FeldNr. 5347
               10  SUH-TAB-510-NHRLBBRTKL        PIC 9(9).
 
      ***  Datenfeld: Alarmanlage Ja/Nein FeldNr. 5348
               10  SUH-TAB-510-NHRJNALARM        PIC XX.
 
      ***  Datenfeld: Feuer Ja/Nein FeldNr. 5349
               10  SUH-TAB-510-NHRJNFE           PIC XX.
 
      ***  Datenfeld: Sturm Ja/Nein FeldNr. 5350
               10  SUH-TAB-510-NHRJNST           PIC XX.
 
      ***  Datenfeld: Hagel Ja/Nein FeldNr. 5351
               10  SUH-TAB-510-NHRJNHA           PIC XX.
 
      ***  Datenfeld: Einbruch/Diebstahl Ja/Nein FeldNr. 5352
               10  SUH-TAB-510-NHRJNED           PIC XX.
 
      ***  Datenfeld: Glas Ja/Nein FeldNr. 5353
               10  SUH-TAB-510-NHRJNGL           PIC XX.
 
      ***  Datenfeld: Leitungswasser Ja/Nein FeldNr. 5354
               10  SUH-TAB-510-NHRJNLW           PIC XX.
 
      ***  Datenfeld: Elementarsch{den Ja/Nein FeldNr. 5355
               10  SUH-TAB-510-NHRJNEL           PIC XX.
 
      ***  Datenfeld: Bezugspr{mie FeldNr. 5356
               10  SUH-TAB-510-NHRBZGPRM         PIC 999V999.
 
      ***  Datenfeld: Leistungstext FeldNr. 5357
               10  SUH-TAB-510-CHRTXLEIST        PIC X(40).
 
      ***  Datenfeld: Summenanpassungsklausel FeldNr. 5358
               10  SUH-TAB-510-CHRTXSUAK         PIC X(32).
 
      ***  Datenfeld: Produkt FeldNr. 5359
               10  SUH-TAB-510-CHRPRD            PIC X(30).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5942
               10  SUH-TAB-510-CMAILING          PIC X(70).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5521
               10  SUH-TAB-510-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 5522
               10  SUH-TAB-510-CUMSTELLTERM      PIC X(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5524
               10  SUH-TAB-510-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5525
               10  SUH-TAB-510-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein FeldNr. 5526
               10  SUH-TAB-510-NKUENDBEST        PIC X(2).
 
      ***  Datenfeld: Zielgruppe FeldNr. 5986
               10  SUH-TAB-510-CZIELGRP          PIC X(30).
 
      ***  Datenfeld: Smart-Home Paket FeldNr. 5730
080816     10 SUH-TAB-510-SMHPAKET         PIC X(02).
 
      ***  Datenfeld: Smart-Home Datum FeldNr. 5731
080816     10 SUH-TAB-510-SMHDATUM.
080816        15 SUH-TAB-510-SMHTT          PIC X(2).
080816        15 SUH-TAB-510-SMHP1          PIC X.
080816        15 SUH-TAB-510-SMHMM          PIC X(2).
080816        15 SUH-TAB-510-SMHP2          PIC X.
080816        15 SUH-TAB-510-SMHJJJJ        PIC X(4).
 
 
      ***  Datenfeld: Smart-Home TrackID FeldNr. 5732
080816     10 SUH-TAB-510-SMHTRACKID       PIC X(60).
 
      ***  Datenfeld: Smart-Home Controlz FeldNr. 5733
080816     10 SUH-TAB-510-SMHCONTROLZ      PIC X(60).
 
      ***  Datenfeld: Leerstellen
080816         10  SUH-TAB-510-LEERSTELLEN         PIC X(526).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-510-INTLFDNR          PIC 9(4).
 
      ******************************************************************
      *                                                                *
      *              520 U N F A L L - V E R T R A G                   *
      *                                                                *
      ******************************************************************
 
           05  SUH-TAB-520-UNFALL REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Bedingung FeldNr. 5639
               10  SUH-TAB-520-CUFVBEDG          PIC X(16).
 
      ***  Datenfeld: Risiko FeldNr. 5640
               10  SUH-TAB-520-CUFLBRIS          PIC X(50).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5641
               10  SUH-TAB-520-CUFKLAUS          PIC X(80).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5643
               10  SUH-TAB-520-CUFZUSINFO        PIC X(100).
 
      ***  Datenfeld: Anzahl versicherte Personen FeldNr. 5644
               10  SUH-TAB-520-NUFVPANZ          PIC 9(5).
 
      ***  Datenfeld: Leistungsfrei von Datum FeldNr. 5645
               10  SUH-TAB-520-CUFDTLSTFV        PIC X(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum FeldNr. 5646
               10  SUH-TAB-520-CUFDTLSTFB        PIC X(8).
 
      ***  Datenfeld: SelbstBeteiligung Ja/Nein FeldNr. 5647
               10  SUH-TAB-520-NUFJNSB           PIC XX.
 
      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 5648
               10  SUH-TAB-520-NUFSBPRZ          PIC 999.
 
      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 5649
               10  SUH-TAB-520-NUFSBFES          PIC 9(5).
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5650
               10  SUH-TAB-520-CUFSBTXT          PIC X(50).
 
      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5651
               10  SUH-TAB-520-NUFJNMBTR         PIC XX.
 
      ***  Datenfeld: Umstellungstermin FeldNr. 5652
               10  SUH-TAB-520-CUFDTUMST         PIC X(8).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5919
               10  SUH-TAB-520-CMAILING          PIC X(70).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 5547
               10  SUH-TAB-520-CUMSTELLTERM      PIC X(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5549
               10  SUH-TAB-520-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5550
               10  SUH-TAB-520-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein
      ***             NKUENDBESTAET in Bestlief FeldNr. 5551
               10  SUH-TAB-520-NKUENDBEST        PIC X(2).
 
      ***  Datenfeld: Leerstellen
090408         10  SUH-TAB-520-LEERSTELLEN         PIC X(894).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-520-INTLFDNR          PIC 9(4).
 
      ******************************************************************
      *                                                                *
      *                521  V G V - V E R T R A G                      *
      *                                                                *
      ******************************************************************
 
           05  SUH-TAB-521-VGVVTRG REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5658
               10  SUH-TAB-521-CVGRSPLZ          PIC X(6).
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5659
               10  SUH-TAB-521-CVGRSSTR          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5660
               10  SUH-TAB-521-CVGRSHSNR         PIC X(15).
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5661
               10  SUH-TAB-521-CVGRSORT          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5662
               10  SUH-TAB-521-NVGRSNKZ          PIC 9(9).
 
      ***  Datenfeld: Bedingung FeldNr. 5663
               10  SUH-TAB-521-CVGVBEDG          PIC X(16).
 
      ***  Datenfeld: Risiko FeldNr. 5664
               10  SUH-TAB-521-CVGRISIKO         PIC X(50).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5665
               10  SUH-TAB-521-CVGKLAUS          PIC X(80).
 
      ***  Datenfeld: Beilagen FeldNr. 5666
               10  SUH-TAB-521-CVGBEILGN         PIC X(19).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5668
               10  SUH-TAB-521-CVGZUSINFO        PIC X(100).
 
      ***  Datenfeld: Feuer Ja/Nein FeldNr. 5669
               10  SUH-TAB-521-NVGJNFEUER        PIC XX.
 
      ***  Datenfeld: Leitungswasser Ja/Nein FeldNr. 5670
               10  SUH-TAB-521-NVGJNLW           PIC XX.
 
      ***  Datenfeld: Sturm Ja/Nein FeldNr. 5671
               10  SUH-TAB-521-NVGJNST           PIC XX.
 
      ***  Datenfeld: Hagel Ja/Nein FeldNr. 5672
               10  SUH-TAB-521-NVGJNHA           PIC XX.
 
      ***  Datenfeld: Elementarsch{den Ja/Nein FeldNr. 5673
               10  SUH-TAB-521-NVGJNSE           PIC XX.
 
      ***  Datenfeld: Geb{udenutzung FeldNr. 5675
               10  SUH-TAB-521-CVGGBNTZ          PIC X(26).
 
      ***  Datenfeld: gewerbliche Fl{che in Prozent FeldNr. 5677
               10  SUH-TAB-521-NVGGEWFL          PIC 999.
 
      ***  Datenfeld: Baujahr FeldNr. 5678
               10  SUH-TAB-521-CVGBAUJ           PIC X(8).
 
      ***  Datenfeld: Bauartklasse LB FeldNr. 5679
               10  SUH-TAB-521-NVGLBBAUKL        PIC 9(9).
 
      ***  Datenfeld: Rohbau bis Datum FeldNr. 5680
               10  SUH-TAB-521-CVGDTRBB          PIC X(8).
 
      ***  Datenfeld: Leistungsart LB FeldNr. 5681
               10  SUH-TAB-521-NVGLBLA           PIC 9(9).
 
      ***  Datenfeld: Unterversicherungsverzicht Ja/Nein FeldNr. 5934
               10  SUH-TAB-521-NVGJNUVZ          PIC XX.
 
      ***  Datenfeld: Grund f}r UVZ FeldNr. 5683
               10  SUH-TAB-521-CVGUVZGRD         PIC X(40).
 
      ***  Datenfeld: Versicherungsumme 1914 FeldNr. 5684
               10  SUH-TAB-521-NVGSUM1914        PIC 9(9).
 
      ***  Datenfeld: Staffelpr{mie Ja/Nein FeldNr. 5685
               10  SUH-TAB-521-NVGJNSTFPR        PIC XX.
 
      ***  Datenfeld: Zone Leitungswasser FeldNr. 5688
               10  SUH-TAB-521-NVGLWZONE         PIC 9.
 
      ***  Datenfeld: Zone Sturm  FeldNr. 5689
               10  SUH-TAB-521-NVGSTZONE         PIC 9.
 
      ***  Datenfeld: Zone Erdbeben FeldNr. 5690
               10  SUH-TAB-521-NVGEBZONE         PIC 9.
 
      ***  Datenfeld: Zone sonstige Elementarsch{den FeldNr. 5691
               10  SUH-TAB-521-NVGSEZONE         PIC 9.
 
      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5692
               10  SUH-TAB-521-NVGJNMBTR         PIC XX.
 
      ***  Datenfeld: Leistungsfrei von Datum FeldNr. 5693
               10  SUH-TAB-521-CVGDTLSTFV        PIC X(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum FeldNr. 5694
               10  SUH-TAB-521-CVGDTLSTFB        PIC X(8).
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5695
               10  SUH-TAB-521-CVGTXTSB          PIC X(14).
 
      ***  Datenfeld: PRM-Satz Hauptwagnis FeldNr. 5696
               10  SUH-TAB-521-NVGHWPRMS         PIC 99V9(7).
 
      ***  Datenfeld: PRM-Satz weitere Elementargefahren FeldNr. 5697
               10  SUH-TAB-521-NVGWEPRMS         PIC 99V9(4).
 
      ***  Datenfeld: Anzahl Vorsch{den weitere Elementargefahren
      ***             FeldNr. 569
               10  SUH-TAB-521-NVGWEVSANZ        PIC 99.
 
      ***  Datenfeld: Gefahrenklsse feuergef{hrlicher Betriebe/Lager
      ***             FeldNr. 5699
               10  SUH-TAB-521-CVGGFKFBL         PIC X.
 
      ***  Datenfeld: Fl{che in Prozent Gefahrenklasse feuerge-
      ***             gef{hrlicher Betrag FeldNr. 5700
               10  SUH-TAB-521-CVGFLFBL          PIC X(20).
 
      ***  Datenfeld: Produkt FeldNr. 5701
               10  SUH-TAB-521-CVGPRD            PIC X(30).
 
      ***  Datenfeld: Geb{udeart FeldNr. 5707
               10  SUH-TAB-521-CVGLBGEBAR        PIC X(50).
 
      ***  Datenfeld: Betriebsart LB FeldNr. 5708
               10  SUH-TAB-521-CVGLBBTART        PIC X(50).
 
      ***  Datenfeld: Modell FeldNr. 5920
               10  SUH-TAB-521-CMODELL           PIC X(18).
 
      ***  Datenfeld: Monopol (Ja/Nein FeldNr. 5921
               10  SUH-TAB-521-NMONOPOL          PIC XX.
 
      ***  Datenfeld: Auslandsrisiko (Ja/Nein) FeldNr. 5922
               10  SUH-TAB-521-NAUSLRIS          PIC XX.
 
      ***  Datenfeld: Geb{udebezeichnung FeldNr. 5923
               10  SUH-TAB-521-CGEBBEZ           PIC X(80).
 
      ***  Datenfeld: Rohbauversicherung FeldNr. 5924
               10  SUH-TAB-521-CROHBAUV          PIC 9(9).
               10  FILLER                        PIC X(6).
 
      ***  Datenfeld: Wohnfl{che FeldNr. 5925
               10  SUH-TAB-521-NWOHNFL           PIC 9(5).
 
      ***  Datenfeld: Geschossanzahl FeldNr. 5926
               10  SUH-TAB-521-NGESCHOSSE        PIC 99.
 
      ***  Datenfeld: Keller vorhanden (Ja/Nein) FeldNr. 5927
               10  SUH-TAB-521-NKELLER           PIC XX.
 
      ***  Datenfeld: Schwimmbad im Geb{ude (Ja/Nein)FeldNr. 5928
               10  SUH-TAB-521-NSCHWIMM          PIC XX.
 
      ***  Datenfeld: Nebengeb{ude Entsch{digungsgrenze FeldNr. 5930
               10  SUH-TAB-521-NNEBENGEB         PIC 9(9).
 
      ***  Datenfeld: Nebengeb{ude FeldNr. 5931
               10  SUH-TAB-521-CNEBENGEB         PIC X(80).
 
      ***  Datenfeld: Sonstiges FeldNr. 5932
               10  SUH-TAB-521-CSONST            PIC X(80).
 
      ***  Datenfeld: Garagen/Carports FeldNr. 5933
               10  SUH-TAB-521-NGARAGEN          PIC 99.
 
      ***  Datenfeld: Zone !berschwemmung FeldNr. 5682
               10  SUH-TAB-521-NVGUEBZONE        PIC 99.
 
      ***  Datenfeld: Mailingaktion FeldNr. 5944
               10  SUH-TAB-521-CMAILING          PIC X(70).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5539
               10  SUH-TAB-521-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 5540
               10  SUH-TAB-521-CUMSTELLTERM      PIC X(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5542
               10  SUH-TAB-521-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5543
               10  SUH-TAB-521-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein FeldNr. 5544
               10  SUH-TAB-521-NKUENDBEST        PIC X(2).
 
      ***  Datenfeld: Elementar bei Diff-Vers. Ja/Nein FeldNr. 5545
               10  SUH-TAB-521-NELEDIFFJN        PIC X(2).
 
      ***  Datenfeld: sonstige Nutzflaeche FeldNr. 5709
310115         10  SUH-TAB-521-SNUTZFL           PIC 9(5).
 
    <<***  Datenfeld: Leerstellen
               10  SUH-TAB-521-LEERSTELLEN       PIC X(236).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-521-INTLFDNR          PIC 9(4).
 
      ******************************************************************
      *                                                                *
      *                 522  P K - P H O T O V O L T A I K             *
      *                                                                *
      ******************************************************************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-TAB-522-VERTRAG REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Bedingungen FeldNr. 8011
           10  SUH-TAB-522-CPVBEDG          PIC X(30).
 
      ***  Datenfeld: Mailingaktion FeldNr. 8012
           10  SUH-TAB-522-CMAILING          PIC X(70).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 8013
           10  SUH-TAB-522-CPVZUSINFO        PIC X(100).
 
      ***  Datenfeld: Informationsquelle FeldNr. 8014
           10 SUH-TAB-522-LVTQUELLE         PIC X(9).
 
      ***  Datenfeld: Baujahr FeldNr. 8015 Form JJJJ
           10 SUH-TAB-522-CPVBAUJ           PIC 9(4).
 
      ***  Datenfeld: Anlagenleistung FeldNr. 8016
           10 SUH-TAB-522-CPVLEIST      PIC 9(4)V99.
 
      ***  Datenfeld: Abweichender Anlagenbetreiber FeldNr. 8017
               10 SUH-TAB-522-CABWANLBTR      PIC X(35).
 
      ***  Datenfeld: Mitversicherung von Batteriesystemen
      ***                    FeldNr. 8018
               10 SUH-TAB-522-MVBATTSYS      PIC XX.
 
      ***  Datenfeld: Gebaeudeart FeldNr. 8019
               10 SUH-TAB-522-CPVGEBART PIC X(50).
 
      ***  Datenfeld: Leerstellen
           10  SUH-TAB-522-LEERSTELLEN       PIC X(1099).
 
      ******************************************************************
      *                                                                *
      *                 523  F K - V E R T R A G                       *
      *                                                                *
      ******************************************************************
 
          05 SUH-TAB-523-FK-VTRG REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Produkt/Tarif FeldNr. 5894
               10  SUH-TAB-523-CFKPROD           PIC X(50).
 
      ***  Datenfeld: Bedingungen FeldNr. 5895
               10  SUH-TAB-523-CFKVBEDG          PIC X(20).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5896
               10  SUH-TAB-523-CFKZUSINFO        PIC X(100).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5939
               10  SUH-TAB-523-CMAILING          PIC X(70).
 
170209***  Datenfeld: Gesellschaft-Klartext FeldNr. 9152
           10 SUH-TAB-523-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 9153
           10 SUH-TAB-523-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 9154 i.d.F. YYYYMMDD
           10 SUH-TAB-523-CUMSTELLTERM      PIC 9(8).
 
      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9155
               10 SUH-TAB-523-NKUENDBESTAET      PIC XX.
 
      ***  Datenfeld: Leerstellen
170209         10  SUH-TAB-523-LEERSTELLEN         PIC X(1061).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-523-INTLFDNR          PIC 9(4).
 
      ******************************************************************
      *                                                                *
      *      613 S G W                                                 *
      *              B E S O N D E R E   V E R E I N B A R U N G       *
      *                                                                *
      ******************************************************************
 
          05 SUH-TAB-613-SGW REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Bedingung/Klausel-Nummer FeldNr. 6137
              10  SUH-TAB-613-CSGBNRBVB         PIC X(20).
 
      ***  Datenfeld: Text besondere Vertragsbestandteile FeldNr. 6138
              10  SUH-TAB-613-CSGBTXTBVB          PIC X(80).
 
      ***  Datenfeld: Leerstellen
090408        10  SUH-TAB-613-LEERSTELLEN         PIC X(1301).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508        10  SUH-TAB-613-INTLFDNR            PIC 9(4).
 
 
210108******************************************************************
>>    *                                                                *
      *  614  U N F A L L - V E R S I C H E R T E   P E R S O N        *
      *                                                                *
      *                                                                *
      ******************************************************************
 
           05  SUH-TAB-614-UNFVP REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: versicherte Person laufende Nummer (Maske)
      ***             FeldNr. 6145
               10  SUH-TAB-614-NUFVPVBNR         PIC 999.
 
      ***  Datenfeld: VP-Name FeldNr. 6146
               10  SUH-TAB-614-CUFVPNAME         PIC X(25).
 
      ***  Datenfeld: VP-Beruf FeldNr. 6147
170717***      10  SUH-TAB-614-CUFVPBERUF        PIC X(21).
170717         10  SUH-TAB-614-CUFVPBERUF        PIC X(48).
 
      ***  Datenfeld: VP-Geburtsdatum FeldNr. 6148
               10  SUH-TAB-614-CUFVPGEBD         PIC X(8).
 
      ***  Datenfeld: VP-Gefahrengruppe FeldNr. 6149
               10  SUH-TAB-614-CUFVPGEFGR        PIC X.
 
      ***  Datenfeld: VP-Beitrag FeldNr. 6150
               10  SUH-TAB-614-NUFVPBTR          PIC 9(7)V99.
 
      ***  Datenfeld: Tarifart FeldNr. 6151
               10  SUH-TAB-614-CUFTARIF          PIC X(30).
 
      ***  Datenfeld: Dynamik-Text FeldNr. 6152
               10  SUH-TAB-614-CUFDYNTEXT        PIC X(5).
 
      ***  Datenfeld: Dynamik-%-Satz FeldNr. 6153
               10  SUH-TAB-614-NUFDYNPRZ         PIC 99V99.
 
      ***  Datenfeld: Dynamik Ablehnungsjahr FeldNr. 6154
               10  SUH-TAB-614-CUFDYNABLJ        PIC X(8).
 
      ***  Datenfeld: Regulierung Ja/Nein FeldNr. 6155
               10  SUH-TAB-614-NUFJNREGUL        PIC XX.
 
      ***  Datenfeld: R}ckgew{hrsumme Ja/Nein FeldNr. 6156
               10  SUH-TAB-614-NUFSUMRUEK        PIC XX.
 
      ***  Datenfeld: Pr{mienr}ckgew{hr Ja/Nein FeldNr. 6157
               10  SUH-TAB-614-NUFJNUPR          PIC XX.
 
      ***  Datenfeld: Freizeitdeckung Ja/Nein FeldNr. 6158
               10  SUH-TAB-614-NUFJNFREIZ        PIC XX.
 
      ***  Datenfeld: Anzahl versicherte Personen je Personengruppe
      ***             FeldNr. 615
               10  SUH-TAB-614-NUFVPGRANZ        PIC 9(5).
 
      ***  Datenfeld: Geschlechtversicherte Person FeldNr. 6160
               10  SUH-TAB-614-NUFVPSEX          PIC 9(9).
 
      ***  Datenfeld: Pr{mienfristellung KIZ Ja/Nein FeldNr. 6162
               10  SUH-TAB-614-NUFJNKIZFR        PIC XX.
 
      ***  Datenfeld: Pr{mie KZ FeldNr. 6163
               10  SUH-TAB-614-NUFKIZPRM         PIC 9(7)V99.
 
      ***  Datenfeld: PartnerHost-ID der versicherten Person
      ***             FeldNr. 6168
               10  SUH-TAB-614-CGPBHNR           PIC 9(15).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5546
               10  SUH-TAB-614-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Zielgruppe FeldNr. 5988
               10  SUH-TAB-614-CZIELGRP          PIC X(30).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 3235
               10 SUH-TAB-614-CGESELL            PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 3236
               10 SUH-TAB-614-CDIFFVSNR          PIC X(40).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 3237 i.d.F. YYYYMMDD
               10 SUH-TAB-614-CUMSTELLTERM       PIC 9(08).
 
      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 3238
               10 SUH-TAB-614-NKUENDBESTAET      PIC XX.
 
      ***  Datenfeld: Nachname FeldNr. 6161
260716         10 SUH-TAB-614-NACHNAME           PIC X(40).
 
      ***  Datenfeld: Vorname FeldNr. 6169
260716         10 SUH-TAB-614-VORNAME           PIC X(30).
 
      ***  Datenfeld: Leerstellen
260716***      10  SUH-TAB-614-LEERSTELLEN         PIC X(1037).
170717         10  SUH-TAB-614-LEERSTELLEN         PIC X(1010).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-614-INTLFDNR          PIC 9(4).
 
      ******************************************************************
      *                                                                *
      *      615  V G V - S E L B S T B E T E I L I G U N G            *
      *                                                                *
      *                                                                *
      ******************************************************************
 
           05  SUH-TAB-615-VGVSB REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: SelbstBeteiligung-Art FeldNr. 6171
               10  SUH-TAB-615-NVGLBSBART        PIC 9(9).
 
      ***  Datenfeld: SelbstBeteiligung-Bezeichnung FeldNr. 6172
               10  SUH-TAB-615-CVGSBBEZ          PIC X(30).
 
      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 6173
               10  SUH-TAB-615-CVGSBPRZ          PIC X(14).
 
      ***  Datenfeld: SelbstBeteiligung-Minimum FeldNr. 6174
               10  SUH-TAB-615-NVGSBMIN          PIC 9(5)V99.
 
      ***  Datenfeld: SelbstBeteiligung-Maximum FeldNr. 6175
               10  SUH-TAB-615-NVGSBMAX          PIC 9(5)V99.
 
      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 6176
               10  SUH-TAB-615-NVGSBFES          PIC 9(5)V99.
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 6243
               10  SUH-TAB-615-CVGSBTXT          PIC X(30).
 
      ***  Datenfeld: Leerstellen
090408         10  SUH-TAB-615-LEERSTELLEN         PIC X(1297).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-615-INTLFDNR          PIC 9(4).
 
      ******************************************************************
      *                                                                *
      *    616 U N F A L L                                             *
      *                  M I T V E R S I C H E R T E   P E R S O N     *
      *                                                                *
      ******************************************************************
 
           05  SUH-TAB-616-UNFMVP REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Name FeldNr. 6447
               10  SUH-TAB-616-CNAME             PIC X(30).
 
      ***  Datenfeld: Vorname FeldNr. 6448
               10  SUH-TAB-616-CVNAME            PIC X(30).
 
      ***  Datenfeld: Geschlecht FeldNr. 6449
               10  SUH-TAB-616-NSEX              PIC 9(9).
 
      ***  Datenfeld: Geburtsdatum FeldNr. 6450
               10  SUH-TAB-616-CGEBDAT           PIC X(8).
 
      ***  Datenfeld: Beginn FeldNr. 6451
               10  SUH-TAB-616-CBEGINN           PIC X(8).
 
      ***  Datenfeld: Ende FeldNr. 6452
               10  SUH-TAB-616-CENDE             PIC X(8).
 
      ***  Datenfeld: Partner-Host-ID der versicherten Person
      ***             FeldNr. 6457
               10  SUH-TAB-616-CGPBHNR           PIC 9(15).
 
    <<***  Datenfeld: Leerstellen
090408         10  SUH-TAB-616-LEERSTELLEN         PIC X(1293).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-616-INTLFDNR          PIC 9(4).
 
231111******************************************************************
    >>*                                                                *
      *    617 U N F A L L                                             *
      *                  MIT BEITRAGSRÜCKGEWÄHR - KAPITALANTEIL        *
      *                                                                *
      ******************************************************************
 
           05  SUH-TAB-617-UBR REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Tabelle Kapitalwerte
             10    SUH-TAB-617-KAPW      OCCURS 12.
 
      ***  Datenfeld: lfd. Nr. Datensatz  FeldNr. 143
               15  SUH-TAB-617-UBR-NR            PIC 9(03).
 
      ***  Datenfeld: lfd. Versicherungsmonat im Vertragsjahr
      ***             FeldNr. 144
               15  SUH-TAB-617-LFD-MONAT         PIC 9(02).
 
      ***  Datenfeld: lfd. Kalenderjahr FeldNr. 145
               15  SUH-TAB-617-JAHR              PIC 9(04).
 
      ***  Datenfeld: lfd. Kalendermonat FeldNr. 146
               15  SUH-TAB-617-MONAT             PIC 9(02).
 
      ***  Datenfeld: Status des Datensatzes FeldNr. 147
               15  SUH-TAB-617-STATUSCD          PIC X.
 
      ***  Datenfeld: Erstelldatum des Datensatzes FeldNr. 148
               15  SUH-TAB-617-STAT-DAT          PIC 9(08).
 
      ***  Datenfeld: garant. Rückkaufswert FeldNr. 149
               15  SUH-TAB-617-RUECKK-GAR        PIC 9(11)V9(04).
 
      ***  Datenfeld: garant. Todesfallleistung FeldNr. 150
               15  SUH-TAB-617-LEITOD-GAR        PIC 9(11)V9(04).
 
      ***  Datenfeld: Rückkaufswert incl. Übersch.-Betlg.
      ***             FeldNr. 151
               15  SUH-TAB-617-RUECK-UEBS        PIC 9(11)V9(04).
 
      ***  Datenfeld: Todesfallleistung incl. Übersch.-Betlg.
      ***             FeldNr. 152
               15  SUH-TAB-617-TOD-UEBS          PIC 9(11)V9(04).
 
      ***  Datenfeld: stille Reserve FeldNr. 153
               15  SUH-TAB-617-WEANW             PIC 9(11)V9(04).
 
      ***  Datenfeld: Rechnungszins  FeldNr. 154
               15  SUH-TAB-617-RECHZINS          PIC 9(02)V9(02).
 
      ***  Datenfeld: Leerstellen
             10    SUH-TAB-617-LEERSTELLEN         PIC X(213).
 
      ***  Datenfeld: Interne lfd. Nummer für die Datenbank
    <<       10    SUH-TAB-617-INTLFDNR          PIC 9(4).
231111
      ******************************************************************
      *                                                                *
      *       628  V E R T R A G S R I S I K O G R U P P E             *
      *                                                                *
      ******************************************************************
 
          05 SUH-TAB-628-VTRGRISGRP REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Risikogruppe-Typ FeldNr. 6353
               10  SUH-TAB-628-NVRGTYP           PIC 9(9).
 
      ***  Datenfeld: Name/Bezeichnung FeldNr. 6354
               10  SUH-TAB-628-CVRGNAME          PIC X(50).
 
      ***  Datenfeld: Beginndatum der Risikogruppe Nr. 6355
               10  SUH-TAB-628-CVRGBEG           PIC X(8).
 
      ***  Datenfeld: Ablaufdatum der Risikogruppe FeldNr. 6356
               10  SUH-TAB-628-CVRGABL           PIC X(8).
 
      ***  Datenfeld: Nettobeitrag FeldNr. 6357
               10  SUH-TAB-628-NVRGNBTRG         PIC 9(8)V99.
 
      ***  Datenfeld: ID übergeordnete Risikogruppe FeldNr. 6358
               10  SUH-TAB-628-NVRGUEBID         PIC 9(4).
 
170209***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9147
           10 SUH-TAB-628-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9148
           10 SUH-TAB-628-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 9149
           10 SUH-TAB-628-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 9150 i.d.F. YYYYMMDD
           10 SUH-TAB-628-CUMSTELLTERM      PIC 9(8).
 
      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9151
             10 SUH-TAB-628-NKUENDBESTAET              PIC X(2).
 
      ***  Datenfeld: Leerstellen
170209         10  SUH-TAB-628-LEERSTELLEN         PIC X(1210).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-628-INTLFDNR          PIC 9(4).
 
      ******************************************************************
      *                                                                *
      *      629  V E R T R A G S R I S I K O M E R K M A L            *
      *                                                                *
      ******************************************************************
 
          05 SUH-TAB-629-VTRGRISMKM REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Name/Bezeichnung FeldNr. 6363
               10  SUH-TAB-629-CVRISMNAME        PIC X(50).
 
      ***  Datenfeld: Risikomerkmalwert FeldNr. 6364
               10  SUH-TAB-629-CVRISMWERT        PIC X(50).
 
      ***  Datenfeld: Leerstellen
090408         10  SUH-TAB-629-LEERSTELLEN         PIC X(1301).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-629-INTLFDNR          PIC 9(4).
 
      ******************************************************************
      *                                                                *
      *  630  V E R T R A G S S E L B S T B E T E I L I G U N G        *
      *                                                                *
      ******************************************************************
 
          05 SUH-TAB-630-VTRGSB REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Name/Bezeichnung FeldNr. 6374
              10  SUH-TAB-630-CVSBBEZ           PIC X(50).
 
      ***  Datenfeld: Wert in Prozent FeldNr. 6375
              10  SUH-TAB-630-NVSBPRZ           PIC 99.
 
      ***  Datenfeld: Wert in DM/Euro FeldNr. 6376
              10  SUH-TAB-630-NVSBBTRG          PIC 9(8)V99.
 
      ***  Datenfeld: Mindestbetrag in DM/Euro FeldNr. 6377
              10  SUH-TAB-630-NVSBMIN           PIC 9(8)V99.
 
      ***  Datenfeld: Höchstbetrag in DM/Euro FeldNr. 6378
              10  SUH-TAB-630-NVSBMAX           PIC 9(8)V99.
 
      ***  Datenfeld: Text zur Selbstbeteiligung FeldNr. 6379
              10  SUH-TAB-630-CVSBTXT           PIC X(30).
 
      ***  Datenfeld: Leerstellen
090408        10  SUH-TAB-630-LEERSTELLEN         PIC X(1289).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508        10  SUH-TAB-630-INTLFDNR          PIC 9(4).
 
      *****************************************************************
      *                                                               *
      *           631  V E R T R A G S L E I S T U N G                *
      *                                                               *
      *****************************************************************
 
          05 SUH-TAB-631-VTRGLSTG REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Name/Bezeichnung FeldNr. 6383
               10  SUH-TAB-631-CVLSTBEZ          PIC X(50).
 
      ***  Datenfeld: Wert DM/EUR FeldNr. 6384
               10  SUH-TAB-631-NVLSTSUM          PIC 9(9)V99.
 
      ***  Datenfeld: sonstiger Wert FeldNr. 6385
               10  SUH-TAB-631-NVLSTWRT          PIC 9(8)V99.
 
      ***  Datenfeld: sonstiger Wert Einheit FeldNr. 6386
               10  SUH-TAB-631-CVLSTWEH          PIC X(20).
 
      ***  Datenfeld: Text zur Leistung FeldNr. 6387
               10  SUH-TAB-631-CVLSTTXT          PIC X(30).
 
      ***  Datenfeld: Leerstellen
090408         10  SUH-TAB-631-LEERSTELLEN         PIC X(1265).
 
230508***  Datenfeld: Interner Themengebietsschlüssel zusammengesetzt.
    >>***  Bestehend : Stellen 1-3 = FeldNr. 4057 NVEZSPARTE aus SA401
      ***              Stellen 4-6 = Satzart(hier 631)
      ***              Stellen 7-9 = FeldNr. 6443 PTGSCHL (SA631)
               10  SUH-TAB-631-INTPTGKEY         PIC 9(15).
      ***
      ***  Datenfeld: Interne lfd. Nummer für die Datenbank
    >>         10  SUH-TAB-631-INTLFDNR          PIC 9(4).
230508***
      ******************************************************************
      *                                                                *
      *          701  Z U S C H L A G / A B S C H L A G                *
      *                                                                *
      ******************************************************************
 
          05 SUH-TAB-701-ZU-ABSCHLAG REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Zu-/Abschlag in % FeldNr. 7057
               10  SUH-TAB-701-NZSLGPROZ         PIC 99V99.
 
      ***  Datenfeld: Zu-/Abschlag in WE FeldNr. 7058
               10  SUH-TAB-701-NZSLGWE           PIC 9(7)V99.
 
      ***  Datenfeld: Kürzel zum Zu-/Abschlag FeldNr. 7059
               10  SUH-TAB-701-CZSLGKRZL         PIC XXX.
 
      ***  Datenfeld: Erklärungstext zum Zu-/Abschlag FeldNr. 7060
               10  SUH-TAB-701-CZSLGTXT          PIC X(30).
 
230508***  Datenfeld: PZANR STAZ SL1 Zuschlags/Abschlagsnummer.
    >>***             FeldNr. 7065.
               10 SUH-TAB-701-PZANR              PIC 9(09).
      ***
      ***  Datenfeld: Leerstellen
230508         10  SUH-TAB-701-LEERSTELLEN         PIC X(1346).
 
230508***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-701-INTLFDNR          PIC 9(4).
 
      ******************************************************************
      *                                                                *
      *              702 V E R T R A G S R I S I K O                   *
      *                                                                *
      ******************************************************************
 
          05 SUH-TAB-702-VTRGRISIKO REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Risikokennziffer FeldNr. 7069
               10  SUH-TAB-702-CVRIKENNZ         PIC X(10).
 
      ***  Datenfeld: Risikoname FeldNr. 7070
               10  SUH-TAB-702-CVRINAME          PIC X(50).
 
      ***  Datenfeld: Risikobeitrag FeldNr. 7071
               10  SUH-TAB-702-NVRIBTRG          PIC 9(7)V99.
 
      ***  Datenfeld: Risikosumme FeldNr. 7072
               10  SUH-TAB-702-NVRIVSUM          PIC 9(9)V99.
 
      ***  Datenfeld: Risiko-Entschädigungsgrenze FeldNr. 7073
               10  SUH-TAB-702-NVRIEGR           PIC 9(08)V99.
 
      ***  Datenfeld: Risiko-Entschädigungsgrenze-Einheit FeldNr. 7074
               10  SUH-TAB-702-NVRIEGREH         PIC 9(9).
 
      ***  Datenfeld: Risikomenge FeldNr. 7075
               10  SUH-TAB-702-NVRIMG            PIC 9(9).
 
      ***  Datenfeld: Risikomenge-Einheit FeldNr. 7076
               10  SUH-TAB-702-CVRIMGEH          PIC X(25).
 
      ***  Datenfeld: Risikosatz FeldNr. 7077
               10  SUH-TAB-702-NVRISATZ          PIC 999V9(7).
 
      ***  Datenfeld: Risikosatz-Einheit FeldNr. 7078
               10  SUH-TAB-702-NVRISATZEH        PIC 9(9).
 
      ***  Datenfeld: Risikotext FeldNr. 7079
               10  SUH-TAB-702-CVRITXT           PIC X(30).
 
      ***  Datenfeld: Risiko-Mitversichert (Ja/Nein)FeldNr. 7080
               10  SUH-TAB-702-NVRIMITVJN        PIC XX.
 
      ***  Datenfeld: Risikosumme-Einheit FeldNr. 7167
               10  SUH-TAB-702-CVRIVSUMEH        PIC X(20).
 
      ***  Datenfeld: Risikosumme unbegrenzt (Ja/Nein) FeldNr. 7168
               10  SUH-TAB-702-NVRIUNBJN         PIC XX.
 
230508***  Datenfeld: Risikomerkmal FeldNr. 7169
    >>         10  SUH-TAB-702-RISMNR            PIC 9(9).
 
      ***  Datenfeld: Leerstellen
230508         10  SUH-TAB-702-LEERSTELLEN         PIC X(1171).
 
230508***  Datenfeld: Interner Themengebietsschlüssel zusammengesetzt.
    >>***  Bestehend : Stellen 1-3 = FeldNr. 4057 NVEZSPARTE aus SA401
      ***              Stellen 4-6 = Satzart(hier 702)
      ***              Stellen 7-9 = FeldNr. 7067 PTGSCHL (SA702)
               10  SUH-TAB-702-INTPTGKEY         PIC 9(15).
      ***
    <<***  Datenfeld: Interne lfd. Nummer für die Datenbank
230508         10  SUH-TAB-702-INTLFDNR          PIC 9(4).
      *
310115******************************************************************
      *                                                                *
      *                705 - F R E M D V E R S I C H E R E R           *
      *                                                                *
      ******************************************************************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-TAB-705-VERTRAG REDEFINES SUH-TAB-DATEN.
 
      ***  Datenfeld: Differenzdeckungsgefahr FeldNr. 7511
           10  SUH-TAB-705-NDIFFGEF         PIC X(45).
 
      ***  Datenfeld: Differenzdeckungsgesellschaft FeldNr. 7512
           10  SUH-TAB-705-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 7513
           10  SUH-TAB-705-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: Umstellungstermin  FeldNr. 7514
           10 SUH-TAB-705-CUMSTELLTERM      PIC 9(8).
 
      ***  Datenfeld: Kuendigsunsbestaetigung JA/Nein FeldNr. 7515
           10 SUH-TAB-705-NKUENDBEST PIC XX.
 
      ***  Datenfeld: Risikomerkmal FeldNr. 7529
           10 SUH-TAB-705-RISMNR      PIC 9(9).
 
      ***  Datenfeld: Leerstellen
           10  SUH-TAB-705-LEERSTELLEN       PIC X(1232).
      *                                                                *
      ***  Datenfeld: Interner Themengebietsschlüssel zusammengesetzt.
    >>***  Bestehend : Stellen 1-3 = FeldNr. 4057 NVEZSPARTE aus SA401
      ***              Stellen 4-6 = Satzart(hier 702)
      ***              Stellen 7-9 = FeldNr. 7505 PTGSCHL (SA705)
               10  SUH-TAB-705-INTPTGKEY         PIC 9(15).
 
    <<***  Datenfeld: Interne lfd. Nummer für die Datenbank
               10  SUH-TAB-705-INTLFDNR          PIC 9(4).
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2061                       *
      ******************************************************************
      *---------------------------------------------------------------*
 



