      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.11.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2221.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2221.CPv  $
      *
      *   Rev 2.1   Jan 02 2008 17:30:54   t13ex48
      *AKL00666: Anpassung RUECK-DTA für Allfinanz PGM KUe
      *
      *   Rev 2.0   Nov 23 2007 09:45:26   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2221  FUNKTION LESE-OZP                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: H. Sebbach                                      *
      *    DATUM:    19.12.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00666!19.12.07! 01 ! NEUERSTELLUNG             ! HSE      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI2221.
           05  PI-DRKI2221-MITARBPERSROLLE    PIC X(1).
           05  PI-DRKI2221-MITARBPERSONENID   PIC X(13).



      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI2221.
           05  PO-DRKI2221-RC                  PIC  X(03).
               88  PO-DRKI2221-RC-OK           VALUE 'OK '.
               88  PO-DRKI2221-RC-LEER         VALUE 'LER'.
               88  PO-DRKI2221-RC-EOF          VALUE 'EOF'.
               88  PO-DRKI2221-RC-NFD          VALUE 'NFD'.
           05  PO-DRKI2221-MITARBEITERNR     PIC S9(9) COMP.
           05  PO-DRKI2221-MITARBMANDANTNR   PIC S9(9) COMP.
      *
      ******** ENDE COPY-MEMBER (DRKI0115) *******************

