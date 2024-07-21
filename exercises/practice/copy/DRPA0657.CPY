      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0657.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0657.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 11:08:20   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:58:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0657  FUNKTION AKT MELD ERM              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    03.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.12.97! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA0657.
          05 PI-DRPA0657-DATEN.
             10 PI-DRPA0657-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA0657.
          05 PO-DRPA0657-RC                         PIC  X(003).
             88 PO-DRPA0657-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0657-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0657-DATEN.
             10 PO-DRPA0657-PMBE.
                COPY DRPA0411.
      *
      ******* ENDE COPY-MEMBER **************************************
