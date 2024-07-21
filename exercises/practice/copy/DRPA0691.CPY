      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0691.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0691.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 11:10:42   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 17:00:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0691  FUNKTION alle prod  meldungen      *
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
       01 PI-DRPA0691.
          05 PI-DRPA0691-PROGRAMM            PIC  X(008).
          05 PI-DRPA0691-UFKT-CODE           PIC  9(002).
          05 PI-DRPA0691-DATEN.
             10 PI-DRPA0691-PMBE-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0691-PMBE-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA0691.
          05 PO-DRPA0691-RC                         PIC  X(003).
             88 PO-DRPA0691-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0691-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA0691-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA0691-DATEN.
             10 PO-DRPA0691-PMPR.
                COPY DRPA0412.
      *
      ******* ENDE COPY-MEMBER **************************************
