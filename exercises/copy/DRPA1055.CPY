      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1055.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1055.CPv  $
      *
      *   Rev 2.1   May 27 1999 12:35:12   T13EX26
      *NT-Umstellung
      *
      *   Rev 2.0   May 26 1999 16:58:22   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 14:01:18   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1055  FUNKTION LIST2 FÜR ZXPA0122        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:     1.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !01.09.98! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1055.
          05 PI-DRPA1055-PROGRAMM                   PIC  X(008).
          05 PI-DRPA1055-UFKT-CODE                  PIC  9(002).
          05 PI-DRPA1055-DATEN.
             10 PI-DRPA1055-PMPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1055-PMPT-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA1055.
          05 PO-DRPA1055-RC                         PIC  X(003).
             88 PO-DRPA1055-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1055-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1055-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1055-PDPT-PDPE.
      *
      *
          COPY DRPA0451.
      *
      ******* ENDE COPY-MEMBER **************************************
