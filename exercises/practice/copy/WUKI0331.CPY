      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0331.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/WUKI0331.CPv  $
      *
      *   Rev 2.5   Feb 02 2021 15:09:50   t13ac20
      *AKI01825: Leben: Schüler BU Erweiterung und Anpassung im DBO
      *: Sourcen Db2
      *
      *   Rev 2.4   Mar 12 2019 07:32:46   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw.
      *GF - Anpassung Host-Komponenten Standard-OZP
      *
      *   Rev 2.3   Sep 20 2017 15:21:00   t13ac20
      *AKI01503: Bereinigung AML-Tabellen
      *
      *   Rev 2.2   Jun 23 2016 10:41:54   t13ex27
      *AKI01434: AML-Neue Produkte zum RT 06/16
      *
      *   Rev 2.1   Sep 12 2011 10:05:30   t13ex27
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP-Vertrags
      *daten
      *
      *   Rev 1.1   09 May 1998 14:13:54   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:08:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-WUKI0331.
           10 LTTVERTRAGSID                 PIC  X(0013).
           10 LTTVERSTARLFDNR               PIC S9(4) COMP.
           10 LTTTTNR                       PIC S9(4) COMP.
           10 LTTPROVHAFTZVON               PIC  X(010).
           10 NIL-LTTPROVHAFTZVON           PIC  X(01).
           10 LTTPROVHAFTZBIS               PIC  X(010).
           10 NIL-LTTPROVHAFTZBIS           PIC  X(01).
           10 LTTABSCHLUSSVERM              PIC S9(9) COMP.
           10 NIL-LTTABSCHLUSSVERM          PIC  X(01).
           10 LTTDYNPROZ                    PIC S9(3)V9(2) COMP-3.
           10 NIL-LTTDYNPROZ                PIC  X(01).
           10 LTTVERTRAGSABLAUF             PIC  X(010).
           10 NIL-LTTVERTRAGSABLAUF         PIC  X(01).
           10 LTTVERTRAGSBEGINN             PIC  X(010).
           10 NIL-LTTVERTRAGSBEGINN         PIC  X(01).
           10 LTTBEITRZAHLDAUER             PIC  X(010).
           10 NIL-LTTBEITRZAHLDAUER         PIC  X(01).
           10 LTTDYNAMIKKZ                  PIC  X(0001).
           10 LTTDYNLTGRVID                 PIC S9(4) COMP.
           10 NIL-LTTDYNLTGRVID             PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 LTTRZINS                      PIC S9(3)V9(2) COMP-3.
           10 NIL-LTTRZINS                  PIC  X(01).
           10 LTTDYNWAHL-KTY                PIC S9(4) COMP.
           10 NIL-LTTDYNWAHL-KTY            PIC  X(01).
           10 LTTDYNWAHL-KNR                PIC S9(15)V COMP-3.
           10 NIL-LTTDYNWAHL-KNR            PIC  X(01).
           10 LTTABSICHART-KTY              PIC S9(4) COMP.
           10 NIL-LTTABSICHART-KTY          PIC  X(01).
           10 LTTABSICHART-KNR              PIC S9(15)V COMP-3.
           10 NIL-LTTABSICHART-KNR          PIC  X(01).
           10 LTTABSICHVAR-KTY              PIC S9(4) COMP.
           10 NIL-LTTABSICHVAR-KTY          PIC  X(01).
           10 LTTABSICHVAR-KNR              PIC S9(15)V COMP-3.
           10 NIL-LTTABSICHVAR-KNR          PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 LTTLSTERHPROZ                 PIC S9(2)V9(1) COMP-3.
           10 NIL-LTTLSTERHPROZ             PIC  X(01).
           10 LTTBUZBERUFGRP-KTY            PIC S9(4) COMP.
           10 NIL-LTTBUZBERUFGRP-KTY        PIC  X(01).
           10 LTTBUZBERUFGRP-KNR            PIC S9(15)V COMP-3.
           10 NIL-LTTBUZBERUFGRP-KNR        PIC  X(01).

