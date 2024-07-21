      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:50:28   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   08 Jul 1998 08:51:36   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1006  FUNKTION LIST PMPR-MELDUNGEN       *
      *                           SORTIERT NACH PROD-SCHL            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    08.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.07.98! 01 ! NEUERSTELLUNG             !BOHLINGER *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
       01 PI-DRPA1006.
          05 PI-DRPA1006-PROGRAMM            PIC  X(008).
          05 PI-DRPA1006-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1006-DATEN.
             10 PI-DRPA1006-PMBE-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1006-PMBE-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA1006.
          05 PO-DRPA1006-RC                         PIC  X(003).
             88 PO-DRPA1006-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1006-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1006-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1006-DATEN.
             10 PO-DRPA1006-PMPR.
                COPY DRPA0412.
                35 PMPR-KENNUNG                     PIC X(008).
                35 PMPR-LAST-UPDATE                 PIC X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
