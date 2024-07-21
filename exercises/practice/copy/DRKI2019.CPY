      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2019.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2019.CPv  $
      *
      *   Rev 2.1   Apr 02 2007 11:39:50   t13ex14
      *AKI00556: Cachen Attribute der KI-Tabellen - UPKI0204
      *
      *   Rev 2.0   Feb 08 2007 10:54:32   t13ex14
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
      *
      * Funktionencopy für Unterprg UPKI0204
      * Lesen des PK einer Tabelle in alphabetischer Reihenfolge
      *
      *---------------------------------------------------------------*
       01  PI-UPKI0204.
           05  PI-UPKI0204-FUNKTION            PIC  X(008).
               88 PI-UPKI0204-DRKI2020              VALUE 'DRKI2020'.
      *
      ********************** ENDE DRKI2019 ****************************
