      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0410.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0410.CPv  $
      *
      *   Rev 2.1   25 Jan 2007 10:49:26   t13ex49
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Teil 2 
      *
      *   Rev 2.0   Apr 11 2006 15:06:22   t13ex49
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

       01  PI-DRAT0410.
           05 PI-DRAT0410-PROGRAMM                 PIC  X(008).
           05 PI-DRAT0410-UFKT                     PIC  9(002).
           05  PI-DRAT0410-DATEN.
               10  PI-DRAT0410-TBNAME              PIC  X(30).

      *
       01  PO-DRAT0410.
           05  PO-DRAT0410-RC                      PIC  X(003).
               88 PO-DRAT0410-RC-OK                       VALUE 'OK '.
               88 PO-DRAT0410-RC-LEER                     VALUE 'LER'.
               88 PO-DRAT0410-RC-EOF                      VALUE 'EOF'.

           05 PO-DRAT0410-DATEN.
               07  PO-DRAT0410-COLNAME             PIC  X(30).


      *
      ************************ ENDE DRAT0410 **************************
