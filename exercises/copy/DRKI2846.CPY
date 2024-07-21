      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.02.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2846.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2846.CPv  $
      *
      *   Rev 2.2   Apr 15 2014 14:22:12   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden-Mi
      *gration Tabelle Neukunde und Datum_Altkunde
      *
      *   Rev 2.1   Feb 26 2014 09:21:52   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden-Mi
      *gration Tabelle Neukunde und Datum_Altkunde
      *
      *   Rev 2.0   Feb 20 2014 10:29:54   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * Bei diesem Copy handelt es sich um das Übergabe-Copy für das
      * UPKI0244, welches in der Ticketverarbeitung die BDSG-Sperren
      * verarbeitet.
      ****************************************************************

      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01180!18.02.14! 2.0! NEU                       !T.Flierl  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************


       01  PI-DRKI2846.
           05  PI-DATEN.
               10  PI-DRKI2846-OTTICKETID         PIC  9(018) COMP-3.
               10  PI-DRKI2846-OTERSTELLT.
                   15 PI-DRKI2846-OTERSTELLT-DATE PIC  X(010).
                   15 PI-DRKI2846-OTERSTELLT-REST PIC  X(016).
               10  PI-DRKI2846-OTERSTELLTVON      PIC  X(030).
               10  PI-DRKI2846-OTHOSTKUNDENID     PIC  X(013).
               10  PI-DRKI2846-OTHOSTVERTRAGID    PIC  X(013).
               10  PI-DRKI2846-OTAENDGRUND-KNR    PIC S9(004) COMP.

       01  PO-DRKI2846.
           05  PO-DRKI2846-RC                   PIC  X(003).
               88 PO-DRKI2846-RC-OK             VALUE 'OK '.
               88 PO-DRKI2846-RC-NOK            VALUE 'NOK'.
           05  PO-DATEN.
               10  PO-DRKI2846-OTFEHLER-KNR     PIC S9(4)  COMP.

