      * T13AE38   Stand: 30/08/98    13:45:34
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0342.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI0342.CPv  $
      *
      *   Rev 2.6   Feb 17 2021 14:02:42   t13ac20
      *AKI01825: Leben: Schüler BU Erweiterung und Anpassung im DBO
      *: Sourcen Copies
      *
      *   Rev 2.5   Mar 12 2019 07:38:26   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw.
      *GF - Anpassung Host-Komponenten Sourcen
      *
      *   Rev 2.4   Jun 23 2016 10:41:04   t13ex27
      *AKI01434: AML-Neue Produkte zum RT 06/16
      *
      *   Rev 2.3   Sep 12 2011 10:04:22   t13ex27
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP-Vertrags
      *daten
      *
      *   Rev 2.2   23 Jul 2001 14:57:04   t13ex27
      *AKI00212 AML-Riester-Produkte
      *
      *   Rev 2.1   Mar 25 1999 09:20:08   T13AE38
      *AKI00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:00:44   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                                                                *
      *    COPY-MEMBER ZUR FORMALEN AUFBEREITUNG LAUFENDE AKTUALISIER- *
      *    UNG AML                                                     *
      *        - EINGABE DTA VERTRAGSDATENAENDERUNGEN TEILTYP AML-     *
      *    KSDS-DATEI                                                  *
      *    SATZLÄNGE 50                                                *
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      *---------!--------!----!---------------------------!-------------
      *AKI00001 !04.08.97!    !NEUERSTELLUNG              !S.Jost
      *AKI00212 !20.06.01!2.2 !Feld KOMPL-AML-DYN-PROZKZN !I.Frenzel
      *         !        !    !eingefügt                  !
      *AKI01065 !09.08.11!2.3 !Feld KOMPL-AML-DYN-PROZKZN !I.Frenzel
      *         !        !    !eingefügt                  !
020516* AKI01434!02.05.16!2.4 ! Neue Produkte TR 06/16    !I. Frenzel  *
110219* AKI01664!11.02.19!2.5 ! NEUES FELD: LSTERHPROZ    !Walbert     *
      * AKI01825!01.02.21!2.6 ! NEUES FELD: BERUFGRP      !T13AC20*
      *         !        !    !                           !
      *         !        !    !                           !
      * ENDE-LOGBUCH
      ******************************************************************
      *
       01  AML-KOMPLETT-SATZ.
               05  KOMPL-AML-SA                  PIC  X(001).
      *            SATZART        = 'C'
               05  KOMPL-AML-KEY.
                   10  KOMPL-AML-VNR             PIC  9(010).
      *                VERTRAGSNR. (ERSTE 5 STELLEN = 0)
                   10  KOMPL-AML-TEILTYP         PIC  9(002).
      *                LV-TEILTYP-NUMMER
               05  KOMPL-AML-TEILDATEN.
                   10  KOMPL-AML-DYN-PROZ        PIC  9(002)V9.
      *                ERHÖHUNGSPROZENTSATZ DER DYNAMIKERHÖHUNG
                   10  KOMPL-AML-DYN-KZ          PIC  9(002).
      *                KENNZEICHEN DYNAMIKSPERRE   99 = J   0,10 = N
010620         05  KOMPL-AML-DYN-PROZKZN         PIC  9(002).
010620*            ERHÖHUNGSPROZENTSATZ-KENNZEICHEN
020516         05  KOMPL-AML-DYNWAHL             PIC  9(002).
>>    *            Dynamikwahl
               05  KOMPL-AML-ABSICHART           PIC  9(002).
      *            Absicherungsart
    <<         05  KOMPL-AML-ABSICHVARIANTE      PIC  9(002).
020516*            Absicherungsvariante
               05  KOMPL-AML-AKTUAL-KZ           PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN
090811         05  KOMPL-AML-RZINS               PIC  9(003)V99.
110219*            Rechnungszins
110219         05  KOMPL-AML-LSTERHPROZ          PIC  9(002)V9.
      *            Leistungserhöhungsprozentsatz
               05  KOMPL-AML-BERUFGRP            PIC  9(002).
      *            Berufsgruppenkennzeichen
               05  KOMPL-AML-RESERVE             PIC  X(013).
      *            RESERVE

