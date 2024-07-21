      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0933.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0933.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:37:50   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0933  FUNKTION ALLE MELD ZUR PRINTE      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    15.04.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.04.98! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA0933.
          05 PI-DRPA0933-PROGRAMM            PIC  X(008).
          05 PI-DRPA0933-UFKT                PIC  9(002).
          05 PI-DRPA0933-DATEN.
             10 PI-DRPA0933-PRINTE           PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA0933.
          05 PO-DRPA0933-RC                  PIC  X(003).
             88 PO-DRPA0933-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0933-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA0933-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA0933-DATEN.
             10 PO-DRPA0933-MELD.
                COPY DRPA0411.
                35   PMBE-KENNUNG            PIC X(008).
                35   PMBE-LAST-UPDATE        PIC X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
