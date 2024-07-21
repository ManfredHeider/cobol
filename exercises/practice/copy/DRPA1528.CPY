      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1528.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1528.CPv  $
      *
      *   Rev 2.1   Apr 16 2002 15:04:22   T13ex22
      *FPA00346: Änderungen für CRPA0108
      *
      *   Rev 2.0   04 Jul 2001 11:11:16   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1528  FUNKTION LIST PMPT- MELDUNGEN      *
      *                           NACH PRINTE UND PRODUKT            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    13.03.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00346!13.03.02! 02 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1528.
          05 PI-DRPA1528-PROGRAMM            PIC  X(008).
          05 PI-DRPA1528-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1528-DATEN.
             10 PI-DRPA1528-PRINTE                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1528-PROD-PROD-SCHL         PIC S9(004) BINARY.
             10 PI-DRPA1528-PVPT-ID                PIC S9(015)
                                                        PACKED-DECIMAL.
       01 PO-DRPA1528.
          05 PO-DRPA1528-RC                         PIC  X(003).
             88 PO-DRPA1528-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1528-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1528-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1528-DATEN.
             10 PO-DRPA1528-PMPT.
                COPY DRPA0413.
                35 PMPT-KENNUNG                     PIC X(008).
                35 PMPT-LAST-UPDATE                 PIC X(026).
