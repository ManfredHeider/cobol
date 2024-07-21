      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0413.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0413.CPv  $
      *
      *   Rev 2.2   24 Jan 2007 13:32:42   t13ex39
      *AKI00556: SYSIBM-SYS-TABELLEN
      *
      *   Rev 2.0   Jun 08 2006 13:25:34   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00556!07.08.06!    ! Neu                       ! WHO       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************

       01  PI-DRAT0413.
           05 PI-DRAT0413-PROGRAMM                 PIC  X(008).
           05 PI-DRAT0413-UFKT                     PIC  9(002).
           05  PI-DRAT0413-DATEN.
               10  PI-DRAT0413-TBNAME              PIC  X(18).

      *
       01  PO-DRAT0413.
           05  PO-DRAT0413-RC                      PIC  X(003).
               88 PO-DRAT0413-RC-OK                       VALUE 'OK '.
               88 PO-DRAT0413-RC-LEER                     VALUE 'LER'.
               88 PO-DRAT0413-RC-EOF                      VALUE 'EOF'.

           05 PO-DRAT0413-DATEN.
              10 PO-DRAT0413-NAME.
                 49 PO-DRAT0413-NAME-LEN           PIC S9(4) USAGE COMP.
                 49 PO-DRAT0413-NAME-TEXT          PIC X(18).
               10 PO-DRAT0413-COLNO                PIC S9(4) USAGE COMP.
               10 PO-DRAT0413-COLTYPE              PIC X(8).
               10 PO-DRAT0413-LENGTH               PIC S9(4) USAGE COMP.
               10 PO-DRAT0413-SCALE                PIC S9(4) USAGE COMP.
               10 PO-DRAT0413-NULLS                PIC X(1).
               10 PO-DRAT0413-KEYSEQ               PIC S9(4) USAGE COMP.
               10 PO-DRAT0413-FOREIGNKEY           PIC X(1).
      *
      ************************ ENDE DRAT0413 **************************
