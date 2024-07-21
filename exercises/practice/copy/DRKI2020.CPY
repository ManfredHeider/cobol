      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2020.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2020.CPv  $
      *
      *   Rev 2.1   Apr 02 2007 11:40:06   t13ex14
      *AKI00556: Cachen Attribute der KI-Tabellen - UPKI0204
      *
      *   Rev 2.0   Feb 08 2007 10:54:48   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00556!26.03.07!    ! Neu                       ! WMI       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************

       01  PI-DRKI2020.
           05 PI-DRKI2020-PROGRAMM                 PIC  X(008).
           05 PI-DRKI2020-UFKT                     PIC  9(002).
           05  PI-DRKI2020-DATEN.
               10  PI-DRKI2020-TBNAME              PIC  X(30).

      *
       01  PO-DRKI2020.
           05  PO-DRKI2020-RC                      PIC  X(003).
               88 PO-DRKI2020-RC-OK                       VALUE 'OK '.
               88 PO-DRKI2020-RC-LEER                     VALUE 'LER'.
               88 PO-DRKI2020-RC-EOF                      VALUE 'EOF'.

           05 PO-DRKI2020-DATEN.
              10 PO-DRKI2020-NAME.
                 49 PO-DRKI2020-NAME-LEN           PIC S9(4) USAGE COMP.
                 49 PO-DRKI2020-NAME-TEXT          PIC X(30).
               10 PO-DRKI2020-COLNO                PIC S9(4) USAGE COMP.
               10 PO-DRKI2020-COLTYPE              PIC X(8).
               10 PO-DRKI2020-LENGTH               PIC S9(4) USAGE COMP.
               10 PO-DRKI2020-SCALE                PIC S9(4) USAGE COMP.
               10 PO-DRKI2020-NULLS                PIC X(1).
               10 PO-DRKI2020-KEYSEQ               PIC S9(4) USAGE COMP.
               10 PO-DRKI2020-FOREIGNKEY           PIC X(1).
      *
      ************************ ENDE DRKI2020 **************************
