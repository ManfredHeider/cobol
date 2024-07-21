      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1660.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1660.CPv  $
      *
      *   Rev 2.2   Jun 14 2010 10:01:40   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.1   Oct 06 2006 12:14:34   t13ex25
      *APA00495: Generali Versicherung
      *
      *   Rev 2.0   Oct 02 2006 14:21:48   t13ex25
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1660                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST PPIA MELDEKENNZEICHEN = N      >>   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                           *
      *    DATUM:    APRIL 2010                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01000!15.06.10!2.1 ! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  DRPA1660-DATEN.
      ****     VERTRAGSNUMMER
               10 DRPA1660-VERTRAGSNUMMER               PIC 9(15).
               10 FILLER                                PIC X(01).
      ****     PIN-NUMMER
               10 DRPA1660-PIN-NR                       PIC 9(09).
      *
      ******* ENDE COPY-MEMBER **************************************
