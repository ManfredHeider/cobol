      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1091.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1091.CPv  $
      *
      *   Rev 2.1   Feb 23 2000 13:58:36   T13EX21
      *APA00081 - Erweiterung um PGEB-GES
      *
      *   Rev 2.0   May 27 1999 12:05:20   T01JM2S
      *APA00042
      *
      *   Rev 1.0   20 Jul 1998 10:21:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1091                                      *
      *    --------------------------------------------------------   *
      *    PROT-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    06.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!06.10.98! 01 ! NEU                       ! WOLFF    *
      * APA00081!08.02.00!2.1 ! ERWEITERUNG UM PGEB-GES   ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PROT.                   *
      *                                                              *
      ****************************************************************
      *
             30 PROT-DATEN.
080200          35 PGEB-GES                         PIC S9(004) BINARY.
                35 PROT-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 KNR-PROT-ART                     PIC S9(004) BINARY.
                35 PROT-ERSTELL-DAT                 PIC  X(010).
                35 KNR-OBJEKT-HKF-TAB               PIC S9(004) BINARY.
                35 PROT-PTR-ID                      PIC S9(015)
                                                        PACKED-DECIMAL.
                35 KNR-PROT-PTR-OBJ                 PIC S9(004) BINARY.
                35 KNR-PROT-PTR-AUSPR               PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
