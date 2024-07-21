      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1385.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1385.CPv  $
      *
      *   Rev 2.1   17 May 2000 15:17:42   T13EX23
      *APA00090 - PINZ-DATEN
      *
      *   Rev 2.0   Apr 11 2000 16:58:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1385                                      *
      *    --------------------------------------------------------   *
      *    PINZ-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRAY                                             *
      *    DATUM:    16.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!16.05.00! 2.1! NEUERSTELLUNG             ! GRAY     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      ****************************************************************
      *
             30 PINZ-DATEN.
                35 PGEB-GES                         PIC S9(004)
                                                        BINARY.
                35 PINZ-PIN                         PIC S9(009)
                                                        BINARY.
                35 PRINTE                           PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PINZ-PG-VBNR                     PIC S9(009)
                                                        BINARY.
                35 PINZ-GUELTIG-AB                  PIC  X(010).
      *
      *            NAME BEGINNT NICHT MIT PINZ, UM DAMIT ANZUDEUTEN
      *            DASS DIESES FELD NICHT ZU SPEICHERN IST
                35 KNR-AKZ-PINZ                     PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
