      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0022.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0022.CPv  $
      *
      *   Rev 2.1   25 Jan 2007 10:49:50   t13ex49
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Teil 2 
      *
      *   Rev 2.0   Apr 18 2006 08:03:14   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00556!12.04.06!    ! Neu                       ! P. Pohl   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************

       01  PI-DROK0022.
           05 PI-DROK0022-PROGRAMM                 PIC  X(008).
           05 PI-DROK0022-UFKT                     PIC  9(002).
           05  PI-DROK0022-DATEN.
               10  PI-DROK0022-OMHOSTTBNAME        PIC  X(30).
               10  PI-DROK0022-OMGESELLSCHAFT      PIC S9(4) COMP.

      *
       01  PO-DROK0022.
           05  PO-DROK0022-RC                      PIC  X(003).
               88 PO-DROK0022-RC-OK                       VALUE 'OK '.
               88 PO-DROK0022-RC-LEER                     VALUE 'LER'.
               88 PO-DROK0022-RC-EOF                      VALUE 'EOF'.

           05 PO-DROK0022-DATEN.
               07  PO-DROK0022-OMHOSTTBNAME         PIC X(30).
               07  PO-DROK0022-OMHOSTATTRIBUT       PIC X(30).
               07  PO-DROK0022-OMGESELLSCHAFT       PIC S9(4) COMP.
               07  PO-DROK0022-OMOKITBNAMEE         PIC X(30).
               07  PO-DROK0022-OMOKIATTRIBUTE       PIC X(30).
               07  PO-DROK0022-OMGESID              PIC X(1).
               07  PO-DROK0022-OMID-NR              PIC X(1).
               07  PO-DROK0022-OMINSUPD             PIC X(1).
               07  PO-DROK0022-OMDELUE              PIC X(1).
               07  PO-DROK0022-OMSUBSELECT          PIC X(2).
               07  PO-DROK0022-OMOKIFORMAT          PIC X(1).
               07  PO-DROK0022-OMSUPUMS             PIC X(1).





      *
      ************************ ENDE DROK0022 **************************

