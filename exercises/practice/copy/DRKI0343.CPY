      * T13AE38   Stand: 30/08/98    13:45:43
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0343.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI0343.CPv  $
      *
      *   Rev 2.6   Feb 17 2021 13:59:36   t13ac20
      *AKI01825: Leben: Schüler BU Erweiterung und Anpassung im DBO
      *: Sourcen Copies
      *
      *   Rev 2.5   Mar 12 2019 07:38:58   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw.
      *GF - Anpassung Host-Komponenten Sourcen
      *
      *   Rev 2.4   Jun 23 2016 10:41:14   t13ex27
      *AKI01434: AML-Neue Produkte zum RT 06/16
      *
      *   Rev 2.3   Sep 12 2011 10:04:28   t13ex27
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP-Vertrags
      *daten
      *
      *   Rev 2.2   23 Jul 2001 14:57:08   t13ex27
      *AKI00212 AML-Riester-Produkte
      *
      *   Rev 2.1   Mar 25 1999 09:20:14   T13AE38
      *AKI00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:00:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                                                                *
      *    COPY-MEMBER ZUR FORMALEN AUFBEREITUNG LAUFENDE AKTUALISIER- *
      *    UNG AML                                                     *
      *                - Teiltyp AML -                                 *
      *                                                                *
      *    BETROFFENE                                                  *
      *    TABELLEN    LEBENTEILTYP (VERSLEBENTARIF)                   *
      *                                                                *
      *    SATZLÄNGE   86                                              *
      *                                                                *
      *----------------------------------------------------------------*
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      *---------!--------!----!---------------------------!------------*
      *AKI00001 !16.08.98!    !NEUERSTELLUNG              !S.JOST      *
      *AKI00212 !20.06.01!2.2 !Feld AML-DYN-PROZKZN       !I.Frenzel   *
      *         !        !    !eingefügt                  !            *
      * AKI01065!10.08.11!2.3 ! Erweiterung um Rechnungs- !I. Frenzel  *
      *         !        !    ! zins                      !            *
020516* AKI01434!02.05.16!2.4 ! Neue Produkte RT 06/16    !I. Frenzel  *
110219* AKI01664!11.02.19!2.5 ! NEUES FELD: LSTERHPROZ    !Walbert     *
      * AKI01825!04.02.21!2.6 ! NEUES FELD: BERUFSGRP-KZN !T13AC20     *
      *         !        !    !                           !            *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
           05  'P'-KEY.
               10  'P'-GES                       PIC  9(002).
               10  'P'-VNR                       PIC  9(015).
               10  'P'-TEILTYP                   PIC  9(002).
      *
           05  'P'-TEILTYPDATEN.
               10  'P'-DYN-PROZ                  PIC  9(002)V9.
               10  'P'-AEND-DYN-PROZ-KZ          PIC  X(001).
               10  'P'-DYN-KZ                    PIC  9(002).
               10  'P'-AEND-DYN-KZ-KZ            PIC  X(001).
010620         10  'P'-DYN-PROZKZN               PIC  9(002).
010620         10  'P'-AEND-DYN-PROZKZN-KZ       PIC  X(001).
100811         10  'P'-RZINS                     PIC  9(003)V99.
100811         10  'P'-AEND-RZINS-KZ             PIC  X(001).
020516         10  'P'-DYNWAHL                   PIC  9(002).
020516         10  'P'-AEND-DYNWAHL-KZ           PIC  X(001).
020516         10  'P'-ABSICHART                 PIC  9(002).
020516         10  'P'-AEND-ABSICHART-KZ         PIC  X(001).
020516         10  'P'-ABSICHVARIANTE            PIC  9(002).
020516         10  'P'-AEND-ABSICHVARIANTE-KZ    PIC  X(001).
110219         10  'P'-LSTERHPROZ                PIC  9(002)V9.
110219         10  'P'-AEND-LSTERHPROZ-KZ        PIC  X(001).
               10  'P'-BERUFGRP                  PIC  9(002).
               10  'P'-AEND-BERUFGRP-KZ          PIC  X(001).
               10  'P'-RESERVE                   PIC  X(034).
           05  'P'-AKTUAL-KZ                     PIC  X(001).

