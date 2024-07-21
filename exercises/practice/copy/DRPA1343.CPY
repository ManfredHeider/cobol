      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1343.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1343.CPv  $
      *
      *   Rev 2.1   Mar 09 2000 17:34:52   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.0   Mar 01 2000 09:57:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1343  FUNKTION LIST PGV ZUORDNUNG        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WILKENS                                         *
      *    DATUM:    01.03.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00087!01.03.00! 01 ! NEUERSTELLUNG             ! WILKENS  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
       01 PI-DRPA1343.
          05 PI-DRPA1343-UFKT                PIC  9(002).
      *
       01  PO-DRPA1343.
           05  PO-DRPA1343-RC                      PIC  X(03).
               88 PO-DRPA1343-RC-OK                     VALUE 'OK '.
               88 PO-DRPA1343-RC-EOF                    VALUE 'EOF'.
               88 PO-DRPA1343-RC-LEER                   VALUE 'LER'.
           05 PO-DRPA1343-ERGEBNISZEILE.
              COPY DRPA1344.

      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
