      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1258.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1258.cpv  $
      *
      *   Rev 2.1   Jun 09 1999 13:33:24   T01JM2S
      *APA00012 - UMSETZUNG FACHL. ANFORDERUNG FKPAG026
      *
      *   Rev 2.0   Mar 30 1999 08:44:16   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1258                                      *
      *    --------------------------------------------------------   *
      *    PMPG-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    07.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!07.06.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PMPG                    *
      *    (INKL. LAST-UPDATE, KENNUNG)                              *
      *                                                              *
      ****************************************************************
      *
             30 PMPG-DATEN.
                35 PMPG-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PMPG-VERSION                     PIC S9(004) BINARY.
                35 PDTA-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PMPG-LFD-NR-VNR                  PIC S9(009) BINARY.
                35 PMPG-VSTAT                       PIC  X(004).
                35 PMPG-LAST-UPDATE                 PIC  X(026).
                35 PMPG-KENNUNG                     PIC  X(008).
                35 PMPG-INHALT.
                   40 PMPG-INHALT-LEN               PIC S9(004) BINARY.
                   40 PMPG-INHALT-TEXT              PIC  X(3900).
      *
      ******* ENDE COPY-MEMBER **************************************
