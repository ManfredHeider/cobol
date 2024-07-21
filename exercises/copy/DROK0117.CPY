      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0117.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0117.CPv  $
      *
      *   Rev 2.1   May 29 2012 10:21:06   t13ex27
      *AOK00047: Auswertung Einarbeitungszeiten und Satzstatistik-D
      *B und OZPs
      *
      *   Rev 2.0   Mar 08 2012 11:27:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DROK0117  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOK00047!08.03.12!2.1 ! NEUERSTELLUNG             !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DROK0117.
          05 PI-DROK0117-PROGRAMM               PIC X(008).
          05 PI-DROK0117-UFKT                   PIC 9(002).

          05 PI-DROK0117-DATEN.
            10 PI-DROK0117-DATUM                PIC X(10).

       01 PO-DROK0117.
          05 PO-DROK0117-DATEN.

            10 PO-DROK0117-AK-KEY               PIC X(026).
            10 PO-DROK0117-AK-SQL-ANZAHL        PIC S9(9)  COMP.
            10 PO-DROK0117-AK-SATZ-ANZAHL       PIC S9(9)  COMP.
            10 PO-DROK0117-DAUER                PIC S9(9)  COMP.
            10 PO-DROK0117-DIFFERENZ            PIC S9(9)  COMP.

          05 PO-DROK0117-RC                       PIC  X(003).
             88 PO-DROK0117-RC-OK                        VALUE 'OK '.
             88 PO-DROK0117-RC-EOF                       VALUE 'EOF'.
             88 PO-DROK0117-RC-LEER                      VALUE 'LER'.

      *---------------------------------------------------------------*
