      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0503.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0503.CPv  $
      *
      *   Rev 2.3   Jun 30 2020 09:53:40   t13ex78
      *AKI01780: Standard-OZP ZTKI0503
      *
      *   Rev 2.2   Sep 02 2015 15:13:14   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:49:06   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 1.1   09 May 1998 14:15:32   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   15 Jan 1998 18:06:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-WUKI0503.
           10 DVZVERTRAGSID                 PIC  X(0013).
           10 DVZPRODUKTID                  PIC S9(4) COMP.
           10 DVZPRODUKTART                 PIC S9(4) COMP.
           10 DVZABRGEBSATZ                 PIC S9(1)V9(2) COMP-3.
           10 NIL-DVZABRGEBSATZ             PIC  X(01).
           10 DVZFACTGEBSATZ                PIC S9(1)V9(2) COMP-3.
           10 NIL-DVZFACTGEBSATZ            PIC  X(01).
           10 DVZEINRGEBBETR                PIC S9(8)V9(2) COMP-3.
           10 NIL-DVZEINRGEBBETR            PIC  X(01).
           10 DVZPROGVONDAT                 PIC  X(010).
           10 NIL-DVZPROGVONDAT             PIC  X(01).
           10 DVZPROGBISDAT                 PIC  X(010).
           10 NIL-DVZPROGBISDAT             PIC  X(01).
           10 DVZPROGANZRECHN               PIC S9(9) COMP.
           10 NIL-DVZPROGANZRECHN           PIC  X(01).
           10 DVZPROGGESUMS                 PIC S9(8)V9(2) COMP-3.
           10 NIL-DVZPROGGESUMS             PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 DVZERSTELLTTST                PIC  X(0026).
           10 DVZFACTANKBETR                PIC S9(10)V9(2) COMP-3.
           10 NIL-DVZFACTANKBETR            PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 DVZPROGVONDATMM               PIC  X(010).
           10 NIL-DVZPROGVONDATMM           PIC  X(01).
           10 DVZPROGBISDATMM               PIC  X(010).
           10 NIL-DVZPROGBISDATMM           PIC  X(01).
           10 DVZPROGANZRECHNMM             PIC S9(9) COMP.
           10 NIL-DVZPROGANZRECHNMM         PIC  X(01).
           10 DVZPROGGESUMSMM               PIC S9(8)V9(2) COMP-3.
           10 NIL-DVZPROGGESUMSMM           PIC  X(01).

