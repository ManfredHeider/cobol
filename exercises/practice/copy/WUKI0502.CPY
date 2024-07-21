      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0502.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0502.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:13:20   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:49:12   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 1.1   09 May 1998 14:15:30   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   15 Jan 1998 18:06:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-DVMFIRMA.
           10 DKZVERTRAGSID                 PIC  X(0013).
           10 DKZRECHTSFORM-KTY             PIC S9(4) COMP.
           10 DKZRECHTSFORM-KNR             PIC S9(4) COMP.
           10 DKZINHABER.
             49 DKZINHABER-LEN              PIC S9(4) COMP.
             49 DKZINHABER-TEXT             PIC X(100).
           10 NIL-DKZINHABER                PIC  X(01).
           10 DKZPOSTFACH                   PIC  X(0010).
           10 NIL-DKZPOSTFACH               PIC  X(01).
           10 DKZPOSTFACHPLZ                PIC S9(9) COMP.
           10 NIL-DKZPOSTFACHPLZ            PIC  X(01).
           10 DKZPOSTFACHORT                PIC  X(0040).
           10 NIL-DKZPOSTFACHORT            PIC  X(01).
           10 DKZTELNR                      PIC  X(0030).
           10 NIL-DKZTELNR                  PIC  X(01).
           10 DKZMOBILNR                    PIC  X(0030).
           10 NIL-DKZMOBILNR                PIC  X(01).
           10 DKZFAXNR                      PIC  X(0030).
           10 NIL-DKZFAXNR                  PIC  X(01).
           10 DKZAPTITEL-KTY                PIC S9(4) COMP.
           10 DKZAPTITEL-KNR                PIC S9(4) COMP.
           10 NIL-DKZAPTITEL-KNR            PIC  X(01).
           10 DKZAPANREDE-KTY               PIC S9(4) COMP.
           10 DKZAPANREDE-KNR               PIC S9(4) COMP.
           10 NIL-DKZAPANREDE-KNR           PIC  X(01).
           10 DKZAPVORNAME                  PIC  X(0030).
           10 NIL-DKZAPVORNAME              PIC  X(01).
           10 DKZAPNACHNAME                 PIC  X(0030).
           10 NIL-DKZAPNACHNAME             PIC  X(01).
           10 DKZAPGEBDAT                   PIC  X(010).
           10 NIL-DKZAPGEBDAT               PIC  X(01).
           10 DKZAPEMAIL.
             49 DKZAPEMAIL-LEN              PIC S9(4) COMP.
             49 DKZAPEMAIL-TEXT             PIC X(128).
           10 NIL-DKZAPEMAIL                PIC  X(01).
           10 DKZAPFUNKTION.
             49 DKZAPFUNKTION-LEN           PIC S9(4) COMP.
             49 DKZAPFUNKTION-TEXT          PIC X(50).
           10 NIL-DKZAPFUNKTION             PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 DKZERSTELLTTST                PIC  X(0026).

