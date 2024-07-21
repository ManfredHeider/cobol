      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0659.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0659.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 11:09:30   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:58:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0659  FUNKTION DIE MELD ZU FÄLLIG        *
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
      *
       01 PI-DRPA0659.
          05 PI-DRPA0659-DATEN.
             10 PI-DRPA0659-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0659-FAELLIG-VON             PIC  X(010).
             10 PI-DRPA0659-FAELLIG-BIS             PIC  X(010).
      *
       01 PO-DRPA0659.
          05 PO-DRPA0659-RC                         PIC  X(003).
             88 PO-DRPA0659-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0659-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0659-DATEN.
             10 PO-DRPA0659-PMBE.
                COPY DRPA0411.
      *
      ******* ENDE COPY-MEMBER **************************************
