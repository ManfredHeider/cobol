      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0469.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0469.CPv  $
      *
      *   Rev 2.2   09 May 2000 18:17:28   T13AE30
      *ASL00046 - Förderbonus Auszahlung
      *
      *   Rev 2.1   Apr 18 2000 12:43:20   T13AE26
      *ASL00046 Einführung Förderbonus (Dimension 7)
      *
      *   Rev 2.0   Apr 03 2000 14:10:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0469                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONEN FüR DAS LESE-OZP ZWVB0077 - FBAZ                *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00046!30.03.00!2.1 ! Neu                       ! T. Moll  *
      * ASL00046!04.05.00!2.2 ! neue Funktion DRVB0475    ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
       01  PI-ZWVB0077.
           05  PI-ZWVB0077-FUNKTION            PIC  X(008).
               88 PI-ZWVB0077-DRVB0470              VALUE 'DRVB0470'.
040500         88 PI-ZWVB0077-DRVB0475              VALUE 'DRVB0475'.
      *
      ******* ENDE COPY-MEMBER **************************************
