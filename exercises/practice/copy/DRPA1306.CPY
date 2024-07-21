      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1306.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1306.CPv  $
      *
      *   Rev 2.2   Oct 30 2006 15:09:50   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Jan 18 2000 14:37:16   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Nov 29 1999 13:55:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1306                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0029                        *
      *        FUNKTION: ZULÄSSIGKEIT PRODUKT/PGV PRÜFEN              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    08.12.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00117!08.12.99! 03 ! NEUERSTELLUNG             ! STORCH   *
      * APA00452!30.10.06!2.02! Umbenennung wg. 3-stell.  ! Jäger    *
      *         !        !    ! Produkte                  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01 PI-DRPA1306.
          05 PI-DRPA1306-PGEB-GES         PIC S9(004) BINARY.
301006*   05 PI-DRPA1306-PRODUKT          PIC S9(004) BINARY.
301006    05 PI-DRPA1306-PROD-PROD-SCHL   PIC S9(004) BINARY.
          05 PI-DRPA1306-PGV              PIC S9(004) BINARY.
          05 PI-DRPA1306-GUELTIG-VON      PIC  X(010).
      *
       01 PO-DRPA1306.
          05 PO-DRPA1306-RC                  PIC   X(003).
             88 PO-DRPA1306-RC-ZUL             VALUE  'ZUL'.
             88 PO-DRPA1306-RC-UNZ             VALUE  'UNZ'.
      *
      ******* ENDE COPY-MEMBER **************************************
