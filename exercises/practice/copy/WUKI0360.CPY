      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0360.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0360.CPv  $
      *
      *   Rev 2.1   Apr 29 2009 14:58:26   t13ex14
      *AKI00818: Tabelle VERTRAGRABATT OZP-Objekte
      *
      *   Rev 2.0   Apr 08 2009 14:00:16   t13ex14
      *
      *   Rev 1.1   09 May 1998 14:14:44   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:10:38   P13PVCS
      *Initial revision.
      *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
090409*AKI00818!VRARABTEXT verlängert für Kfz           !W.Misgeld!    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-VERTRAGRABATT.
           10 VRAVERTRAGSID               PIC  X(13).
           10 VRALFDNR                    PIC  S9(4) COMP.
090409*    10 VRARABTEXT                  PIC  X(20).
090409     10 VRARABTEXT                  PIC  X(26).
           10 NIL-VRARABTEXT              PIC  X(01).
           10 VRARABWERT                  PIC  S9(9)V9(2) COMP-3.
           10 NIL-VRARABWERT              PIC  X(01).
           10 VRARABWERWKZ                PIC  X(1).
           10 VRARABEINHEIT               PIC  S9(4) COMP.
           10 NIL-VRARABEINHEIT           PIC  X(01).
           10 LETZTEAENDERUNG             PIC  X(026).
           10 NIL-LETZTEAENDERUNG         PIC  X(01).
