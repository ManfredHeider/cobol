      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1326.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1326.CPv  $
      *
      *   Rev 2.1   Jan 18 2000 14:44:46   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Dec 14 1999 16:42:14   P13SP20
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1326  FUNKTION LIST PGV ZUORDNUNG        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    22.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!22.11.99! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
       01 PI-DRPA1326.
          05 PI-DRPA1326-UFKT                PIC  9(002).
      *
       01  PO-DRPA1326.
           05  PO-DRPA1326-RC                      PIC  X(03).
               88 PO-DRPA1326-RC-OK                     VALUE 'OK '.
               88 PO-DRPA1326-RC-EOF                    VALUE 'EOF'.
               88 PO-DRPA1326-RC-LEER                   VALUE 'LER'.
           05 PO-DRPA1326-ERGEBNISZEILE.
              COPY DRPA0955.

      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
