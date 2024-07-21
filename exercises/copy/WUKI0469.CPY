      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06/06/14
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0469.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WUKI0469.CPv  $
      *
      *   Rev 2.4   Jun 17 2020 10:17:46   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.3   Jun 23 2016 08:44:50   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP ZT/ZM
      *
      *   Rev 2.2   Oct 08 2014 13:44:38   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:06:42   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Jun 6 2014 11:14:34   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0469.
           10 G10EZVVERTRAGSNR              PIC  X(0015).
           10 G10EZVHOSTNR                  PIC  X(0015).
           10 G10EZVSPARTE                  PIC S9(9) COMP.
           10 G10EZVPBRDNR                  PIC S9(9) COMP.
           10 G10EZVBEGINN                  PIC  X(010).
           10 G10EZVABLAUF                  PIC  X(010).
           10 G10EZVSTATUS                  PIC  X(0009).
           10 G10EZVNVEZBTRG                PIC S9(8)V9(2) COMP-3.
           10 G10EZVSUMME                   PIC S9(9) COMP.
           10 G10EZVVART                    PIC S9(9) COMP.
           10 G10EZVAENDGRD                 PIC  X(0050).
           10 G10EZVAENDDAT                 PIC  X(010).
           10 G10EZVNETJBTR                 PIC S9(8)V9(2) COMP-3.
           10 G10EZVBRUTJBTR                PIC S9(8)V9(2) COMP-3.
           10 G10EZVNBTRZW                  PIC S9(8)V9(2) COMP-3.
           10 G10EZVLETBEARB                PIC  X(010).
           10 G10EZVZWEIG                   PIC  X(0002).
           10 G10EZVRABNETTO                PIC S9(8)V9(2) COMP-3.
           10 G10EZVSUAKTBTR                PIC S9(8)V9(2) COMP-3.
           10 G10EZVREOBPRDNR               PIC  X(0005).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 G10EZVPRODBEZ                 PIC  X(0050).
           10 NIL-G10EZVPRODBEZ             PIC  X(01).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 G10EZVFLEXNSP                 PIC S9(9)V9(2) COMP-3.
           10 NIL-G10EZVFLEXNSP             PIC  X(01).
           10 G10EZVFLEXNSPGW               PIC  X(0001).
           10 NIL-G10EZVFLEXNSPGW           PIC  X(01).

