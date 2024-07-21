      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.01.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUOK0008.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUOK0008.CPv  $
      *
      *   Rev 2.1   29 Jan 2007 10:17:10   t13ex39
      *AKI00556: P M K I 0 7 9 3
      *
      *   Rev 2.0   29 Jan 2007 10:05:54   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00556!21.12.06! 1.0! NEU                       ! Hornung  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05  WU-OKITICKET.
           10 OTTICKETID                  PIC S9(018) COMP-3.
           10 OTTICKETTYP-KTY             PIC S9(4) COMP.
           10 OTTICKETTYP-KNR             PIC S9(4) COMP.
           10 OTAENDGRUND-KTY             PIC S9(4) COMP.
           10 OTAENDGRUND-KNR             PIC S9(4) COMP.
           10 OTERSTELLT                  PIC X(026).
           10 OTERSTELLTVON               PIC X(30).
           10 OTOKIKUNDENID               PIC S9(018) COMP-3.
           10 NIL-OTOKIKUNDENID           PIC X(01).
           10 OTHOSTKUNDENID              PIC X(13).
           10 NIL-OTHOSTKUNDENID          PIC X(01).
           10 OTOKIVERTRAGID              PIC S9(018) COMP-3.
           10 NIL-OTOKIVERTRAGID          PIC X(01).
           10 OTHOSTVERTRAGID             PIC X(13).
           10 NIL-OTHOSTVERTRAGID         PIC X(01).
           10 OTSWVERSION.
              49 OTSWVERSION-LEN               PIC S9(4) COMP.
              49 OTSWVERSION-TEXT              PIC X(100).
           10 NIL-OTSWVERSION             PIC X(01).
           10 OTSTATUS-KTY                PIC S9(4) COMP.
           10 OTSTATUS-KNR                PIC S9(4) COMP.
           10 OTAUSLESEJOBID              PIC S9(018) COMP-3.
           10 NIL-OTAUSLESEJOBID          PIC X(01).
           10 OTFEHLER-KTY                PIC S9(4) COMP.
           10 OTFEHLER-KNR                PIC S9(4) COMP.
           10 OTPARAMETER.
              49 OTPARAMETER-LEN               PIC S9(4) COMP.
              49 OTPARAMETER-TEXT              PIC X(1000).
           10 NIL-OTPARAMETER             PIC X(01).
      *****************************************************************
