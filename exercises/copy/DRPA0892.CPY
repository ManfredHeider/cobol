      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0892.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0892.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:31:42   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:49:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0892                                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: schrieber                                       *
      *    DATUM:    12.03.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.03.98! 01 ! NEUERSTELLUNG             ! schrieber*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA0892.
          05 PI-DRPA0892-PROGRAMM                   PIC  X(008).
          05 PI-DRPA0892-DATEN.
             10 PI-DRPA0892-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0892-PGEB-GES                PIC S9(004)
                                                        BINARY.
             10 PI-DRPA0892-VNR                     PIC  X(015).
             10 PI-DRPA0892-FAELLIG                 PIC  X(010).
      *
       01 PO-DRPA0892.
          05 PO-DRPA0892-RC                         PIC  X(003).
             88 PO-DRPA0892-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0892-RC-NOK                        VALUE 'NOK'.
          05 PO-DRPA0892-RC-ZUSATZ                  PIC  X(004).
             88 PO-DRPA0892-RC-PVER                       VALUE 'PVER'.
             88 PO-DRPA0892-RC-PMBE                       VALUE 'PMBE'.
             88 PO-DRPA0892-RC-PMPR                       VALUE 'PMPR'.
          05 PO-DRPA0892-DATEN.
             10 PO-DRPA0892-PG-PM.
                COPY DRPA0295.
      *
      ******* ENDE COPY-MEMBER **************************************
