      * T13EX33   Stand: 21/09/98    15:34:32
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0353.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0353.CPv  $
      *
      *   Rev 2.5   Feb 17 2021 14:00:24   t13ac20
      *AKI01825: Leben: Schüler BU Erweiterung und Anpassung im DBO
      *: Sourcen Copies
      *
      *   Rev 2.4   Mar 12 2019 07:39:28   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw.
      *GF - Anpassung Host-Komponenten Sourcen
      *
      *   Rev 2.3   Jun 23 2016 10:41:22   t13ex27
      *AKI01434: AML-Neue Produkte zum RT 06/16
      *
      *   Rev 2.2   Sep 12 2011 10:04:34   t13ex27
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP-Vertrags
      *daten
      *
      *   Rev 2.1   23 Jul 2001 14:57:14   t13ex27
      *AKI00212 AML-Riester-Produkte
      *
      *   Rev 2.0   Mar 23 1999 15:52:32   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:01:40   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 3 5 3    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMM AML VERTRAGSDATEN      *
      *       LEBENTEILTYP UND VERSLEBENTARIF                          *
      *                                                                *
      *    SATZLÄNGE   120                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      *---------!--------!----!---------------------------!------------*
      *AKI00001 !16.07.98!    !NEUERSTELLUNG              !HUEBNER     *
      *AKI00212 !20.06.01!2.1 !Feld AML-DYNGRVID ver-     !I.Frenzel   *
      *         !        !    !größert                    !            *
      * AKI01065!10.08.11!2.2 ! Erweiterung um Rechnungs- !I. Frenzel  *
      *         !        !    ! zins                      !            *
020516* AKI01434!02.05.16!2.3 ! Neue Produkte RT 06/16    !I. Frenzel  *
110219* AKI01664!11.02.19!2.4 ! NEUES FELD: LSTERHPROZ    !Walbert     *
      * AKI01825!04.02.21!2.5 ! NEUES FELD: BERUFSGRP-KZN !T13AC20     *
      *         !        !    !                           !            *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
 
           02  'P'-LEBENTEILTYP.
           05  'P'-GES                           PIC  9(002).
           05  'P'-VNR                           PIC  9(015).
 
           05  'P'-TEILTYPNR                     PIC  9(002).
           05  'P'-DYNPROZ                       PIC  9(003)V99.
           05  'P'-AEND-DYNPROZ-KZ               PIC  X(001).
      *        J WENN DYNPROZ GEAENDERT
           05  'P'-DYNKZ                         PIC  X(001).
           05  'P'-AEND-DYNKZ-KZ                 PIC  X(001).
      *        J WENN DYNKZ GEAENDERT
010620*    05  AML-DYNGRVID                      PIC S9(001).
           05  'P'-DYNGRVID                      PIC S9(002).
           05  'P'-AEND-DYNGRVID-KZ              PIC  X(001).
      *        J WENN DYNGRVID GEAENDERT
           05  'P'-ABLAUF                        PIC  X(010).
           05  'P'-AEND-ABLAUF-KZ                PIC  X(001).
      *        J WENN ABLAUF GEAENDERT
           05  'P'-BEGINN                        PIC  X(010).
           05  'P'-AEND-BEGINN-KZ                PIC  X(001).
      *        J WENN BEGINN GEAENDERT
           05  'P'-DAUER                         PIC  X(010).
           05  'P'-AEND-DAUER-KZ                 PIC  X(001).
      *        J WENN DAUER GEAENDERT
100811     05  'P'-RZINS                         PIC  9(003)V99.
100811     05  'P'-AEND-RZINS-KZ                 PIC  X(001).
      *        J WENN DAUER GEAENDERT
020516     05  'P'-DYNWAHL                       PIC  9(002).
>>         05  'P'-AEND-DYNWAHL-KZ               PIC  X(001).
      *        J WENN DAUER GEAENDERT
           05  'P'-ABSICHART                     PIC  9(002).
           05  'P'-AEND-ABSICHART-KZ             PIC  X(001).
      *        J WENN DAUER GEAENDERT
    <<     05  'P'-ABSICHVARIANTE                PIC  9(002).
020516     05  'P'-AEND-ABSICHVARIANTE-KZ        PIC  X(001).
           05  'P'-AKTUAL-KZ                     PIC  X(001).
110219     05  'P'-LSTERHPROZ                    PIC  9(002)V9.
110219     05  'P'-AEND-LSTERHPROZ-KZ            PIC  X(001).
           05  'P'-BERUFGRP                      PIC  9(002).
           05  'P'-AEND-BERUFGRP-KZ              PIC  X(001).
110219     05  'P'-RESERVE                       PIC  X(034).

