      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1204.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1204.cpv  $
      *
      *   Rev 2.1   Mar 31 1999 09:51:14   T13EX22
      *Sichern verarbeitete nPM's
      *
      *   Rev 2.0   Mar 05 1999 09:20:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      * ---------------------------------------------------------------*
      * APA00012!30.03.99! 01 ! NEUERSTELLUNG             ! MUND       *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      ******************************************************************
      *    COPY-MEMBER: DRPA1204  FUNKTION LESEN PVW3200               *
      *    ------------------------------------------------------------*
      *    ERSTELLT:                                                   *
      *    DATUM:                                                      *
      ******************************************************************
       01 PI-DRPA1204.
          05 PI-DRPA1204-UFKT                     PIC  9(002).
          05 PI-DRPA1204-DATEN                    PIC  X(112).
      *
       01 PO-DRPA1204.
          05 PO-DRPA1204-DATEN                    PIC X(112).
          05 PO-DRPA1204-RC                       PIC  X(003).
             88 PO-DRPA1204-RC-OK                      VALUE 'OK '.
             88 PO-DRPA1204-RC-EOF                     VALUE 'EOF'.
             88 PO-DRPA1204-RC-LEER                    VALUE 'LER'.
             88 PO-DRPA1204-RC-NOK                     VALUE 'NOK'.
      *
      ***** ENDE-COPY-MEMBER *****************************************

