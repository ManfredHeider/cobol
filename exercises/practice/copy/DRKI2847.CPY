      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.02.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2847.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2847.CPv  $
      *
      *   Rev 2.2   Apr 15 2014 14:22:20   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden-Mi
      *gration Tabelle Neukunde und Datum_Altkunde
      *
      *   Rev 2.1   Feb 26 2014 09:21:56   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden-Mi
      *gration Tabelle Neukunde und Datum_Altkunde
      *
      *   Rev 2.0   Feb 20 2014 10:37:12   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * Funktions-Copy für das UPRO UPKI0244, welches im Rahmen der
      * Ticketverarbeitung die BDSG-Sperren verarbeitet.
      ****************************************************************

      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01180!18.02.14! 2.0! NEU                       !T.Flierl  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPKI0244.
           05  PI-UPKI0244-FKT                 PIC  X(008).
               88 PI-UPKI0244-DRKI2846
                                            VALUE 'DRKI2846'.

