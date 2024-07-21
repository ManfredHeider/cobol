      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2397.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2397.CPv  $
      *
      *   Rev 2.1   Apr 15 2010 09:35:30   t13ae49
      *AKI00793: Einarbeitungs-/Einarbeitungszeiten in OKI - Änderu
      *ngen Ticketprogramm
      *
      *   Rev 2.0   Mar 22 2010 14:50:42   t13ae49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00793!24.03.10! 2.1! NEU                       ! JWH      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI2397.
           05  PI-DATEN.
               10  PI-DRKI2397-OTTICKETID             PIC 9(018) COMP-3.
               10  PI-DRKI2397-OTERSTELLT             PIC X(026).
               10  PI-DRKI2397-OTERSTELLER            PIC X(030).
               10  PI-DRKI2397-OTAENDGRUND-KTY        PIC S9(4)  COMP.
               10  PI-DRKI2397-OTAENDGRUND-KNR        PIC S9(4)  COMP.

       01  PO-DRKI2397.
           05  PO-DRKI2397-RC                         PIC  X(003).
               88 PO-DRKI2397-RC-OK                   VALUE 'OK '.
               88 PO-DRKI2397-RC-NOK                  VALUE 'NOK'.
           05  PO-DATEN.
               10  PO-DRKI2397-OTFEHLER-KNR          PIC S9(4)  COMP.

