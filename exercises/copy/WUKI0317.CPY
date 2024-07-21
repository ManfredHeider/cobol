      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0317.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/WUKI0317.CPv  $
      *
      *   Rev 2.8   May 23 2017 10:50:50   t13ac17
      *AKI01510: Standard OZP
      *
      *   Rev 2.7   Oct 17 2005 10:07:28   t13ex27
      *AKI00521: DB-Änderung für Fix 80 PMKI0079 und WUKI's
      *
      *   Rev 2.6   May 24 2004 11:44:32   t13ex27
      *DB-Änderung für Fix 61 PMKI0079 und WUKI's
      *
      *   Rev 2.5   25 Jul 2001 10:33:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01510!18.04.17!2.8 ! Neuerstellung             ! GWA      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05  WU-WUKI0317.
           10 LRVVERTRAGSID                 PIC  X(0013).
           10 LRVVERSTARLFDNR               PIC S9(4) COMP.
           10 LRVRKWHV                      PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVRKWHV                  PIC  X(01).
           10 LRVRKWHVBKZ                   PIC  X(0001).
           10 NIL-LRVRKWHVBKZ               PIC  X(01).
           10 LRVRKWBUZV                    PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVRKWBUZV                PIC  X(01).
           10 LRVRKWBUZVBKZ                 PIC  X(0001).
           10 NIL-LRVRKWBUZVBKZ             PIC  X(01).
           10 LRVRKWTZV                     PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVRKWTZV                 PIC  X(01).
           10 LRVRKWTZVBKZ                  PIC  X(0001).
           10 NIL-LRVRKWTZVBKZ              PIC  X(01).
           10 LRVRKWPZV                     PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVRKWPZV                 PIC  X(01).
           10 LRVRKWPZVBKZ                  PIC  X(0001).
           10 NIL-LRVRKWPZVBKZ              PIC  X(01).
           10 LRVRKWEUZV                    PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVRKWEUZV                PIC  X(01).
           10 LRVRKWEUZVBKZ                 PIC  X(0001).
           10 NIL-LRVRKWEUZVBKZ             PIC  X(01).
           10 LRVBTFRSUMBUZV                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRSUMBUZV            PIC  X(01).
           10 LRVBTFRSUMBUZVBKZ             PIC  X(0001).
           10 NIL-LRVBTFRSUMBUZVBKZ         PIC  X(01).
           10 LRVBTFRSUMTZV                 PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRSUMTZV             PIC  X(01).
           10 LRVBTFRSUMTZVBKZ              PIC  X(0001).
           10 NIL-LRVBTFRSUMTZVBKZ          PIC  X(01).
           10 LRVBTFRSUMPZV                 PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRSUMPZV             PIC  X(01).
           10 LRVBTFRSUMPZVBKZ              PIC  X(0001).
           10 NIL-LRVBTFRSUMPZVBKZ          PIC  X(01).
           10 LRVBTFRSUMEUZV                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRSUMEUZV            PIC  X(01).
           10 LRVBTFRSUMEUZVBKZ             PIC  X(0001).
           10 NIL-LRVBTFRSUMEUZVBKZ         PIC  X(01).
           10 LRVFMBTFRTTBUZV               PIC  X(010).
           10 NIL-LRVFMBTFRTTBUZV           PIC  X(01).
           10 LRVFMBTFRTTBUZVBKZ            PIC  X(0001).
           10 NIL-LRVFMBTFRTTBUZVBKZ        PIC  X(01).
           10 LRVFMBTFRTTTZV                PIC  X(010).
           10 NIL-LRVFMBTFRTTTZV            PIC  X(01).
           10 LRVFMBTFRTTTZVBKZ             PIC  X(0001).
           10 NIL-LRVFMBTFRTTTZVBKZ         PIC  X(01).
           10 LRVFMBTFRTTPZV                PIC  X(010).
           10 NIL-LRVFMBTFRTTPZV            PIC  X(01).
           10 LRVFMBTFRTTPZVBKZ             PIC  X(0001).
           10 NIL-LRVFMBTFRTTPZVBKZ         PIC  X(01).
           10 LRVFMBTFRTTEUZV               PIC  X(010).
           10 NIL-LRVFMBTFRTTEUZV           PIC  X(01).
           10 LRVFMBTFRTTEUZVBKZ            PIC  X(0001).
           10 NIL-LRVFMBTFRTTEUZVBKZ        PIC  X(01).
           10 LRVBTFRISTWERTBUZV            PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRISTWERTBUZV        PIC  X(01).
           10 LRVBTFRISTWERTTZV             PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRISTWERTTZV         PIC  X(01).
           10 LRVBTFRISTWERTPZV             PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRISTWERTPZV         PIC  X(01).
           10 LRVBTFRISTWERTEUZV            PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRISTWERTEUZV        PIC  X(01).
           10 LRVBTFRMINDWERTBUZ            PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRMINDWERTBUZ        PIC  X(01).
           10 LRVBTFRMINDWERTTZV            PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRMINDWERTTZV        PIC  X(01).
           10 LRVBTFRMINDWERTPZV            PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRMINDWERTPZV        PIC  X(01).
           10 LRVBTFRMINDWERTEUZ            PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBTFRMINDWERTEUZ        PIC  X(01).
           10 LRVBETBWRRKHV                 PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBETBWRRKHV             PIC  X(01).
           10 LRVBETBWRRKHVBKZ              PIC  X(0001).
           10 NIL-LRVBETBWRRKHVBKZ          PIC  X(01).
           10 LRVBETBWRRKBUZV               PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBETBWRRKBUZV           PIC  X(01).
           10 LRVBETBWRRKBUZVBKZ            PIC  X(0001).
           10 NIL-LRVBETBWRRKBUZVBKZ        PIC  X(01).
           10 LRVBETBWRRKTZV                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBETBWRRKTZV            PIC  X(01).
           10 LRVBETBWRRKTZVBKZ             PIC  X(0001).
           10 NIL-LRVBETBWRRKTZVBKZ         PIC  X(01).
           10 LRVBETBWRRKPZV                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBETBWRRKPZV            PIC  X(01).
           10 LRVBETBWRRKPZVBKZ             PIC  X(0001).
           10 NIL-LRVBETBWRRKPZVBKZ         PIC  X(01).
           10 LRVBETBWRRKEUZV               PIC S9(11)V9(2) COMP-3.
           10 NIL-LRVBETBWRRKEUZV           PIC  X(01).
           10 LRVBETBWRRKEUZVBKZ            PIC  X(0001).
           10 NIL-LRVBETBWRRKEUZVBKZ        PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).

