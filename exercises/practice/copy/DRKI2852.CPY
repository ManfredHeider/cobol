      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2852.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2852.CPv  $
      *
      *   Rev 2.2   Apr 22 2014 11:44:12   t13ex78
      *AKI01063: Wartungsauftrag-Ticketverarbeitung Kundenlöschung 
      *Gemeinschaftskonten
      *
      *   Rev 2.1   Mar 27 2014 11:27:36   t13ex78
      *AKI01063: Wartungsauftrag-Ticketverarbeitung - Kundenlöschun
      *g Gemeinschaftskonten
      *
      *   Rev 2.0   Mar 17 2014 13:47:20   t13ex78
      *Archiverstellung
      *
      ****************************************************************
      * Funktions-Copy für das ZWKI0000.
      ****************************************************************

      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01063!17.03.14! 2.0! NEU                       !T.Flierl  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZWKI0000.
           05  PI-ZWKI0000-FKT                 PIC  X(008).
               88 PI-ZWKI0000-DRKI2851
                                            VALUE 'DRKI2851'.
