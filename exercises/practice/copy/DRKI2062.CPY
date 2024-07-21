      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2062.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRKI2062.CPv  $
      *
      *   Rev 2.14   May 05 2015 09:01:46   t13ae31
      *AKI01358: SUH Ermittlungsprogramm überarbeiten
      *
      *   Rev 2.13   Mar 19 2015 08:42:06   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 2.12   Mar 19 2015 08:03:36   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 2.11   Feb 23 2012 11:12:24   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.10   Feb 03 2012 15:01:22   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.9   Aug 05 2011 11:13:50   t13ae31
      *AKI01037: SUH - FK-And. 613 Erm+Aufb+Akt
      *
      *   Rev 2.8   Feb 18 2009 14:10:20   t13ae31
      *AKI00795: Diff.deck in FK Best.verträgen Ermittl.
      *
      *   Rev 2.7   Dec 12 2008 14:42:50   t13ex52
      *AKI00772: Reisegepäckversicherung
      *
      *   Rev 2.5   Dec 05 2008 09:33:18   t13ex52
      *FKL00012: Pruefen SA401 auf vorkommende SA5nn
      *
      *   Rev 2.4   Jun 21 2008 12:51:10   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.3   Feb 12 2008 09:43:42   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.2   Jan 22 2008 12:17:26   t13ex14
      *AKL00665: Allfinanz; REOB-PK/FK Copystrecken
      *
      *   Rev 2.1   Dec 04 2007 16:46:12   t13ex52
      *AKL00664: Allfinanz: FK Bestandsdaten AM-Vers
      *
      *   Rev 2.0   Aug 21 2007 09:31:22   t13ex52
      *Archiverstellung
      *
      *
      *----------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - Schablone für den Header der Datensaetze aus dem DTA der *
      *       Allfinanz und der Datei KSDS-Komplettbestand.            *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *21.08.07! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
210108*21.01.08! Satzarten  f. Reob-PK eingefügt      ! WMI     !      *
      *        !                                      !         !      *
170908*17.09.08! Verarbeitung zur Deaktitivierung der ! HSL     !      *
      *        ! Entitaet 524[PK-Vertrag]             !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
091208*09.12.08! Integration neues AMV-Produkt Reise- ! HSL     !      *
      *        ! paeck-Versicherung Entitaet 504.     !         !      *
      *        ! Es wird hierfuer die nicht mehr be-  !         !      *
      *        ! noetigte Entitaet 524 in die Entitaet!         !      *
      *        ! 504 geaendert.                       !         !      *
      *        !                                      !         !      *
170209*17.02.09! Differenzdeckung FK eingebaut        ! HHU     !      *
      *        !                                      !         !      *
030811*AKI01037! Änd. SA 613                          !P. Hübner!      *
      *        !                                      !         !      *
231111*AKI01102! neue SA 617                          !B. Stoldt!      *
      *        !                                      !         !      *
310115*AKI01295! neue SA 522 und 705 und Änd. 521     !P. Hübner!      *
      *        !                                      !         !      *
200415*AKI01358! Feldnamen gekuerzt                   !P. Hübner!      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *                   A L L F I N A N Z - R E O B                  *
      *                      Z W S P - H E A D E R                     *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 210 Bytes                   STAND: 21.08.2007      *
      *                                                                *
      **************************************************************** *
       01 SUH-ZWSP-HEADER.
           05 AF-REOB-KEY.
      **  Headerinformation: Vertragsnummer
             10  SUH-ZSP-VTRGNUMMER            PIC 9(15).
      **  Schlüssel: Einzelvertrag Host-ID
             10  SUH-ZSP-CVEZHOSTNR            PIC X(15).
      **  Headerinformation: Nummer der ALLFINANZ-Satzart
             10  SUH-ZSP-SATZART               PIC 999.
      **  Entitätenkey für die einzelnen Satzarten
             10 SUH-ZSP-HD-ETKEY               PIC X(30).
 
      ***** BEGINN DER REDEFINITION FUER DIE ENTITAETEN-SCHLÜSSEL
      ***** JE SATZART WIRD EIN ENTITÄTEN-SCHLÜSSEL GEBRAUCHT.
             10 SUH-ZSP-HD-ETKEY-400 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 4009
               15  SUH-ZSP-HD-400-NGESELLST         PIC 9(9).
               15  SUH-ZSP-HD-400-LEER              PIC X(21).
      ***************************************************************
 
             10 SUH-ZSP-HD-ETKEY-401 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 4066
               15  SUH-ZSP-HD-401-NGESELLST         PIC 9(9).
      **  Schlüssel: VEZ-Sparte FeldNr. 4057
               15  SUH-ZSP-HD-401-NVEZSPARTE        PIC 9(9).
      **  Schlüssel: VEZ-Tarif/Produkt FeldNr. 4058
               15  SUH-ZSP-HD-401-NVEZPBRDNR        PIC 9(9).
               15  SUH-ZSP-HD-401-LEER              PIC X(3).
      ***************************************************************
 
             10 SUH-ZSP-HD-ETKEY-403 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 9999
               15 SUH-ZSP-HD-403-NGESELLST         PIC 9(9).
      **  Schlüssel: Vertragsnummer FeldNr. 9999
      **  Schlüssel: Risikoadresse-HostID FeldNr. 6366
               15 SUH-ZSP-HD-403-NRISADRHID        PIC 9(9).
               15 SUH-ZSP-HD-403-LEER              PIC X(12).
      ***************************************************************
 
091208       10 SUH-ZSP-HD-ETKEY-504 REDEFINES SUH-ZSP-HD-ETKEY.
    >>**  Schlüssel: Versicherungsgesellschaft FeldNr. 5889
               15  SUH-ZSP-HD-504-NGESELLST         PIC 9(9).
    <<         15  SUH-ZSP-HD-504-LEER              PIC X(21).
091208***************************************************************
 
210108       10 SUH-ZSP-HD-ETKEY-508 REDEFINES SUH-ZSP-HD-ETKEY.
>>    **  Schlüssel: Versicherungsgesellschaft FeldNr. 5293
               15  SUH-ZSP-HD-508-NGESELLST         PIC 9(9).
               15  SUH-ZSP-HD-508-LEER              PIC X(21).
      ***************************************************************
 
             10 SUH-ZSP-HD-ETKEY-509 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 5321
               15 SUH-ZSP-HD-509-NGESELLST         PIC 9(9).
               15 SUH-ZSP-HD-509-LEER              PIC X(21).
      ****************************************************************
 
             10 SUH-ZSP-HD-ETKEY-510 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 5361
               15 SUH-ZSP-HD-510-NGESELLST         PIC 9(9).
               15 SUH-ZSP-HD-510-LEER              PIC X(21).
      ****************************************************************
 
             10 SUH-ZSP-HD-ETKEY-520 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 5654
               15 SUH-ZSP-HD-520-NGESELLST         PIC 9(9).
               15 SUH-ZSP-HD-520-LEER              PIC X(21).
      ****************************************************************
 
             10 SUH-ZSP-HD-ETKEY-521 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 5704
               15 SUH-ZSP-HD-521-NGESELLST         PIC 9(9).
               15 SUH-ZSP-HD-521-LEER              PIC X(21).
    <<****************************************************************
310115       10 SUH-ZSP-HD-ETKEY-522
                                      REDEFINES SUH-ZSP-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 8004
310115         15 SUH-ZSP-HD-522-NGESELLST         PIC 9(9).
    <<         15 SUH-ZSP-HD-522-LEER              PIC X(21).
    <<****************************************************************
210108
             10 SUH-ZSP-HD-ETKEY-523 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 5889
               15 SUH-ZSP-HD-523-NGESELLST         PIC 9(9).
               15 SUH-ZSP-HD-523-LEER              PIC X(21).
      ****************************************************************
             10 SUH-ZSP-HD-ETKEY-613 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 6140
               15 SUH-ZSP-HD-613-NGESELLST         PIC 9(9).
      *** Schlüssel: Komponentengruppe FeldNr. 9317
030811         15  SUH-ZSP-HD-613-RGID              PIC 9(4).
      **  Schlüssel: laufende Nummer FeldNr. 6136
               15 SUH-ZSP-HD-613-PSGBLFDNR         PIC 999.
030811         15 SUH-ZSP-HD-613-LEER              PIC X(14).
      ****************************************************************
 
210108       10 SUH-ZSP-HD-ETKEY-614 REDEFINES SUH-ZSP-HD-ETKEY.
>>    **  Schlüssel: Versicherungsgesellschaft FeldNr. 6165
               15  SUH-ZSP-HD-614-NGESELLST         PIC 9(9).
      **  Schlüssel: versicherte Person laufende Nummer (Schlüssel)
      **             FeldNr. 6144
               15  SUH-ZSP-HD-614-PUFVPLFDNR        PIC 999.
               15  SUH-ZSP-HD-614-LEER              PIC X(18).
      ***************************************************************
 
             10 SUH-ZSP-HD-ETKEY-615 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 6178
               15 SUH-ZSP-HD-615-NGESELLST         PIC 9(9).
      **  Schlüssel: Laufende Nummer der Selbstbeteiligung
      **             FeldNr. 6170
               15 SUH-ZSP-HD-615-PVGSBLFDNR        PIC 999.
               15 SUH-ZSP-HD-615-LEER              PIC X(18).
      ***************************************************************
 
             10 SUH-ZSP-HD-ETKEY-616 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 6459
               15 SUH-ZSP-HD-616-NGESELLST         PIC 9(9).
      **  Schlüssel: VP laufende Nummer FeldNr. 6445
               15 SUH-ZSP-HD-616-PUFVPLFDNR        PIC 999.
      **  Schlüssel: MVP laufende Nummer FeldNr. 6446
               15 SUH-ZSP-HD-PUFMVPLFDNR       PIC 999.
               15 SUH-ZSP-HD-616-LEER          PIC X(15).
    <<****************************************************************
210108
231111       10 SUH-ZSP-HD-ETKEY-617 REDEFINES SUH-ZSP-HD-ETKEY.
    >>**  Schlüssel: Versicherungsgesellschaft FeldNr. 138
               15 SUH-ZSP-HD-617-NGESELLST         PIC 9(09).
      **  Schlüssel: Komponentengruppennummer FeldNr. 141
               15 SUH-ZSP-HD-617-PVRIGRP           PIC 9(04).
      **  Schlüssel: Laufendes Vertragsjahr FeldNr. 142
               15 SUH-ZSP-HD-617-LFD-JAHR          PIC 9(02).
    <<         15 SUH-ZSP-HD-617-LEER              PIC X(15).
231111****************************************************************
 
             10 SUH-ZSP-HD-ETKEY-628 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 6401
               15 SUH-ZSP-HD-628-NGESELLST         PIC 9(9).
      **  Schlüssel: Subjekt-Host-ID FeldNr. 6399
               15 SUH-ZSP-HD-628-NVRGSUBHID        PIC 9(9).
      **  Schlüssel: Risikogruppe-ID FeldNr. 6352
               15 SUH-ZSP-HD-628-PVRGID            PIC 9(4).
               15 SUH-ZSP-HD-628-LEER              PIC X(8).
      **************************************************************
 
             10 SUH-ZSP-HD-ETKEY-629 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 6408
               15 SUH-ZSP-HD-629-NGESELLST         PIC 9(9).
      **  Schlüssel: Risikogruppe-ID FeldNr. 6361
               15 SUH-ZSP-HD-629-PVRGID            PIC 9(4).
      **  Schlüssel: Risikomerkmalwert-ID FeldNr. 6362
               15 SUH-ZSP-HD-629-PVRISMID          PIC 999.
               15 SUH-ZSP-HD-629-LEER              PIC X(14).
      **************************************************************
 
             10 SUH-ZSP-HD-ETKEY-630 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 6415
               15 SUH-ZSP-HD-630-NGESELLST         PIC 9(9).
      **  Schlüssel: Risikogruppe-ID FeldNr. 6372
               15 SUH-ZSP-HD-630-PVRGID            PIC 9(4).
      **  Schlüssel: Selbstbeteiligung-ID FeldNr. 6373
               15 SUH-ZSP-HD-630-PVRSBID           PIC 999.
               15 SUH-ZSP-HD-630-LEER              PIC X(14).
      **************************************************************
 
             10 SUH-ZSP-HD-ETKEY-631 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 6391
               15 SUH-ZSP-HD-631-NGESELLST         PIC 9(9).
      **  Schlüssel: Risikogruppe-ID FeldNr. 6381
               15 SUH-ZSP-HD-631-PVRGID            PIC 9(4).
      **  Schlüssel: Leistung-ID FeldNr. 6382
               15 SUH-ZSP-HD-631-PVLSTID           PIC 999.
      **  Schlüssel: Themengebietsschlüssel FeldNr. 6443
               15 SUH-ZSP-HD-631-PTGSCHL           PIC 999.
               15 SUH-ZSP-HD-631-LEER              PIC X(11).
      ***************************************************************
 
             10 SUH-ZSP-HD-ETKEY-701 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 7062
               15 SUH-ZSP-HD-701-NGESELLST         PIC 9(9).
      **  Schlüssel: Universelle Laufende Nummer FeldNr. 7055
               15 SUH-ZSP-HD-701-PZUNILFDNR        PIC 999.
               15 SUH-ZSP-HD-701-LEER              PIC X(18).
      ***************************************************************
 
             10 SUH-ZSP-HD-ETKEY-702 REDEFINES SUH-ZSP-HD-ETKEY.
      **  Schlüssel: Versicherungsgesellschaft FeldNr. 7082
               15 SUH-ZSP-HD-702-NGESELLST         PIC 9(9).
      **  Schlüssel: Risikogruppe FeldNr. 7066
               15 SUH-ZSP-HD-702-PVRIGRP           PIC 9(4).
      **  Schlüssel: Themengebietsschlüssel FeldNr. 7067
               15 SUH-ZSP-HD-702-PTGSCHL           PIC 999.
      **  Schlüssel: Risiko-Lfdnr FeldNr. 7068
               15 SUH-ZSP-HD-702-PVRILFDNR         PIC 999.
               15 SUH-ZSP-HD-702-LEER              PIC X(11).
      ***************************************************************
 
             10 SUH-ZSP-HD-ETKEY-705 REDEFINES SUH-ZSP-HD-ETKEY.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7503
               15 SUH-ZSP-HD-705-NGESELLST          PIC 9(9).
      ***  Schlüssel: Risikogruppe FeldNr. 7504
               15 SUH-ZSP-HD-705-PVRIGRP            PIC 9(4).
      ***  Schlüssel: Themengebietsschluessel FeldNr. 7505
               15 SUH-ZSP-HD-705-PTGSCHL            PIC 999.
      ***  Schlüssel: Risiko-Lfdnr FeldNr. 7506
               15 SUH-ZSP-HD-705-PVRILFDNR          PIC 999.
               15 SUH-ZSP-HD-705-LEER               PIC X(11).
 
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2062                       *
      ******************************************************************
      *----------------------------------------------------------------*
 
 

