      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1758.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1758.CPv  $
      *
      *   Rev 2.1   Jan 13 2014 09:47:42   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.0   Aug 26 2013 08:26:34   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1758  FUNKTION ALLE MELD ZUR PRINTE      *
      *                 FÜR SPEZIELLE SORTIERREIHENFOLGE IM PO23     *
      *                 DER GESELLSCHAFT 20 (AM-LEBEN)               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    26.08.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!26.08.013 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1758.
          05 PI-DRPA1758-PROGRAMM            PIC  X(008).
          05 PI-DRPA1758-UFKT                PIC  9(002).
          05 PI-DRPA1758-DATEN.
             10 PI-DRPA1758-PRINTE           PIC S9(015) PACKED-DECIMAL.

      *
       01 PO-DRPA1758.
          05 PO-DRPA1758-RC                  PIC  X(003).
             88 PO-DRPA1758-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1758-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1758-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1758-DATEN.
             10 PO-DRPA1758-MELD.
                COPY DRPA0411.
                35   PMBE-KENNUNG            PIC X(008).
                35   PMBE-LAST-UPDATE        PIC X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
