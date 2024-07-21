      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1155.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1155.CPv  $
      *
      *   Rev 2.1   31 Jan 2001 14:25:52   T13EX39
      *AKI00068 KBI
      *
      *   Rev 2.0   23 Nov 2000 11:09:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1155                                      *
      *    --------------------------------------------------------   *
      *                                                               *
      *                                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WHO                                              *
      *    DATUM:     6.12.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068! 6.12.00! 01 ! NEUERSTELLUNG             ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1155                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1155.
          05 PI-DRKI1155-PROGRAMM                 PIC X(008).
          05 PI-DRKI1155-UFKT-CODE                PIC 9(002).
          05 PI-DRKI1155-DATEN.
            07 PI-DRKI1155-VRBPERSONENID          PIC X(013).
      *
       01 PO-DRKI1155.

          05 PO-DRKI1155-RC                       PIC X(003).
             88 PO-DRKI1155-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1155-RC-LEER                  VALUE 'LER'.
             88 PO-DRKI1155-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1155-DATEN.
             10 PO-DRKI1155-VB-COUNT               PIC S9(09) COMP.
