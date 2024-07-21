      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0894.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0894.CPv  $
      *
      *   Rev 2.1   23 Aug 2007 09:53:18   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.0   Feb 27 1999 14:29:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:32:50   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:49:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0894                                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: schrieber                                       *
      *    DATUM:    12.03.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.03.98! 01 ! NEUERSTELLUNG             ! SCHRIEBER*
      * APA00639!25.06.07!2.1 ! ERW. DER PIN AUF 9 STELLEN! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA0894.
          05 PI-DRPA0894-PROGRAMM                   PIC  X(008).
          05 PI-DRPA0894-DATEN.
             10 PI-DRPA0894-PGEB-GES                PIC S9(004)
                                                        BINARY.
250607*      10 PI-DRPA0894-PIN                     PIC S9(008)
250607       10 PI-DRPA0894-PIN                     PIC S9(009)
                                                        BINARY.
      *
       01 PO-DRPA0894.
          05 PO-DRPA0894-RC                         PIC  X(003).
             88 PO-DRPA0894-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0894-RC-NOK                        VALUE 'NOK'.
          05 PO-DRPA0894-DATEN.
             10 PO-DRPA0894-PG-PM.
                COPY DRPA0295.
      *
      ******* ENDE COPY-MEMBER **************************************
