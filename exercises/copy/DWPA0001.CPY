      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DWPA0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWPA0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:15:00   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:15:06   P13PVCS
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DWPA0001  Modul zur ID-Ermittlung            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    09.09.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !09.09.96! 01 ! Neuerstellung             ! Priggen  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE BENÖTIGTEN DATEN ZUR ID-ERMITT-   *
      *    LUNG, DIE IM MODUL DPPA0001 BENÖTIGT WERDEN               *
      *                                                              *
      ****************************************************************
      *
       01 DWPA0001.
          05 DWPA0001-ID                     PIC  9(015) VALUE ZERO.
          05 DWPA0001-ID-R                   REDEFINES DWPA0001-ID.
             10 DWPA0001-COBOL-DAY.
                15 DWPA0001-COBOL-DAY-JJ     PIC  9(002).
                15 FILLER                    PIC  X(003).
             10 DWPA0001-COBOL-TIME          PIC  9(008).
             10 DWPA0001-ID-UNR              PIC  9(002).
      *
      ****** Ende Copy-Member ****************************************
