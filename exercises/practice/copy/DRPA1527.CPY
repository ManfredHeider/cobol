      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1527.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1527.CPv  $
      *
      *   Rev 2.1   Apr 16 2002 15:03:24   T13ex22
      *FPA00346: Änderungen für CRPA0108
      *
      *   Rev 2.0   04 Jul 2001 11:11:16   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1527  FUNKTION LIST PMPT- PRODUKTE ZU    *
      *                           PRINTE SORT. NACH PVER-VERSION     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    11.03.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00346!11.03.02! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
       01 PI-DRPA1527.
          05 PI-DRPA1527-PROGRAMM            PIC  X(008).
          05 PI-DRPA1527-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1527-DATEN.
             10 PI-DRPA1527-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1527.
          05 PO-DRPA1527-RC                         PIC  X(003).
             88 PO-DRPA1527-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1527-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1527-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1527-DATEN.
             10 PO-DRPA1527-PRINTE          PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA1527-PVER-VNR        PIC  X(015).
             10 PO-DRPA1527-PVER-VERSION    PIC S9(004) BINARY.
             10 PO-DRPA1527-PROD-PROD-SCHL  PIC S9(004) BINARY.
             10 PO-DRPA1527-PVPT-ID         PIC S9(015) PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
